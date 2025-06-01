open Tree.Syntax
open Demo
open Texpr

type abstraction = {
  sketch : texpr;
  init : record;
  steps : (action * record) list;
}
[@@deriving eq, show]

exception Not_supported of string

let aexpr_of_attr_value (v : attr_value) : aexpr =
  match v with AttrFunc l -> AttrFunc l | AttrConst c -> AttrConst (Const c)

(* texpr_of_tree t = e => teval e [] = t *)
let rec texpr_of_tree (t : tree) : texpr =
  match t with
  | Const c -> Val (Const c)
  | Elem { name; attrs; children } ->
      Elem
        {
          name;
          attrs = List.map (fun (k, v) -> (k, aexpr_of_attr_value v)) attrs;
          children = Fixed (List.map texpr_of_tree children);
        }

let abstract_step (edit : edit) (e : texpr) (env : record) :
    texpr * (record -> record) * record =
  match (edit, e) with
  | _, OMap _ -> raise (Invalid_argument "OMap should have been traversed")
  | Replace new_c, Val (Var var) ->
      let e' = e in
      let s = Fun.id in
      let env' = record_update env var (Const new_c) in
      (e', s, env')
  | Replace new_c, Val (Const old_c) ->
      let var = fresh_var env in
      let e' = Val (Var var) in
      let s env = record_update env var (Const old_c) in
      let env' = record_update env var (Const new_c) in
      (e', s, env')
  | (Dup _ | Del _ | Insert _ | SetAttr _), Val _ ->
      raise (Type_error "Cannot apply edit to Val")
  | Replace _, Elem _ -> raise (Type_error "Cannot replace Elem with Const")
  | Dup 0, Elem ({ children = Fixed [ child ]; _ } as elem) ->
      let var = fresh_var env in
      let vars = free_vars_texpr child in
      let e' = Elem { elem with children = LMap { var; body = child } } in
      let s env =
        let child_env, new_env =
          List.partition (fun (v, _) -> List.mem v vars) env
        in
        record_update new_env var (List [ child_env ])
      in
      let env' =
        let child_env, new_env =
          List.partition (fun (v, _) -> List.mem v vars) env
        in
        record_update new_env var (List [ child_env; child_env ])
      in
      (e', s, env')
  | Dup _, Elem { children = Fixed _; _ } ->
      raise
        (Not_supported "Duplication not supported for fixed multiple children")
  | Dup i, Elem { children = LMap { var; _ }; _ } ->
      let lst = List.assoc var env |> list_of_value in
      if i >= List.length lst then
        raise (Invalid_argument "Index out of bounds for duplication");
      let e' = e in
      let s = Fun.id in
      let env' =
        record_update env var
          (List
             (List.mapi (fun j v -> if j = i then [ v; v ] else [ v ]) lst
             |> List.concat))
      in
      (e', s, env')
  | Del i, Elem ({ children = Fixed children; _ } as elem) ->
      if i >= List.length children then
        raise (Invalid_argument "Index out of bounds for deletion");
      let var = fresh_var env in
      let child = List.nth children i in
      let vars = free_vars_texpr child in
      let children' =
        List.mapi
          (fun j c -> if j = i then [ OMap { var; body = child } ] else [ c ])
          children
        |> List.concat
      in
      let e' = Elem { elem with children = Fixed children' } in
      let s env =
        let child_env, new_env =
          List.partition (fun (v, _) -> List.mem v vars) env
        in
        record_update new_env var (Record child_env)
      in
      let env' =
        let new_env' = List.filter (fun (v, _) -> not (List.mem v vars)) env in
        record_update new_env' var Null
      in
      (e', s, env')
  | Del i, Elem { children = LMap { var; _ }; _ } ->
      let lst = List.assoc var env |> list_of_value in
      if i >= List.length lst then
        raise (Invalid_argument "Index out of bounds for deletion");
      let env' =
        record_update env var (List (List.filteri (fun j _ -> j <> i) lst))
      in
      (e, Fun.id, env')
  | Insert (i, new_tree), Elem ({ children = Fixed children; _ } as elem) ->
      if i > List.length children then
        raise (Invalid_argument "Index out of bounds for insertion");
      let new_texpr = texpr_of_tree new_tree in
      let var = fresh_var env in
      let children' =
        if i = List.length children then
          children @ [ OMap { var; body = new_texpr } ]
        else
          List.mapi
            (fun j c ->
              if j = i then [ OMap { var; body = new_texpr }; c ] else [ c ])
            children
          |> List.concat
      in
      let e' = Elem { elem with children = Fixed children' } in
      let s env = record_update env var Null in
      let env' = record_update env var (Record []) in
      (e', s, env')
  | Insert _, Elem { children = LMap _; _ } ->
      raise (Not_supported "Insert not supported for LMap children")
  | SetAttr (key, attr), Elem ({ attrs; _ } as elem) -> (
      match List.assoc_opt key attrs with
      | Some (AttrConst (Var var)) ->
          let e' = e in
          let s = Fun.id in
          let env' =
            record_update env var
              (match attr with Some c -> Const c | None -> Null)
          in
          (e', s, env')
      | Some (AttrConst (Const v)) ->
          let var = fresh_var env in
          let attrs' =
            (key, AttrConst (Var var)) :: List.remove_assoc key attrs
          in
          let e' = Elem { elem with attrs = attrs' } in
          let s env = record_update env var (Const v) in
          let env' =
            record_update env var
              (match attr with Some c -> Const c | None -> Null)
          in
          (e', s, env')
      | Some (AttrFunc _) ->
          raise (Not_supported "Setting function attribute not supported")
      | None ->
          let var = fresh_var env in
          let attrs' = (key, AttrConst (Var var)) :: attrs in
          let e' = Elem { elem with attrs = attrs' } in
          let s env = record_update env var Null in
          let env' =
            record_update env var
              (match attr with Some c -> Const c | None -> Null)
          in
          (e', s, env'))

(* abstract_step_traverse path edit e env0 = (e', s, env') =>
   (forall env ~ env0, teval e' (s env) = teval e env) AND
   teval e' env' = apply_traverse path edit (teval e' (s env0)) *)
let rec abstract_step_traverse (path : path) (edit : edit) (e : texpr)
    (env : record) : texpr * (record -> record) * record =
  match (e, path) with
  | OMap { var; body }, _ ->
      (* traverse into OMap *)
      (* the path is editable only if it is not null *)
      let inner_env = List.assoc var env |> record_of_value in
      let body', inner_s, inner_env' =
        abstract_step_traverse path edit body inner_env
      in
      let e' = OMap { var; body = body' } in
      let s env =
        (* other pathes with same type might be null *)
        match List.assoc var env with
        | Null -> env
        | Record inner_env ->
            let inner_env' = inner_s inner_env in
            record_update env var (Record inner_env')
        | _ -> raise (Type_error "Expected Record or Null for OMap")
      in
      let env' = record_update env var (Record inner_env') in
      (e', s, env')
  | _, [] ->
      (* no more path, apply the edit directly *)
      abstract_step edit e env
  | Val _, _ -> raise (Invalid_argument "Cannot traverse into Val")
  | Elem ({ children = Fixed children; _ } as elem), i :: rest ->
      if i >= List.length children then
        raise (Invalid_argument "Index out of bounds for path");
      let child_e = List.nth children i in
      let child_e', s, env' = abstract_step_traverse rest edit child_e env in
      let children' =
        List.mapi (fun j c -> if j = i then child_e' else c) children
      in
      let e' = Elem { elem with children = Fixed children' } in
      (e', s, env')
  | Elem ({ children = LMap { var; body }; _ } as elem), i :: rest ->
      let lst = List.assoc var env |> list_of_value in
      if i >= List.length lst then
        raise (Invalid_argument "Index out of bounds for path");
      let child_env = List.nth lst i in
      let body', child_s, child_env' =
        abstract_step_traverse rest edit body child_env
      in
      let e' = Elem { elem with children = LMap { var; body = body' } } in
      let s env =
        let lst = List.assoc var env |> list_of_value in
        record_update env var (List (List.map child_s lst))
      in
      let env' =
        record_update env var
          (List
             (List.mapi
                (fun j c -> if j = i then child_env' else child_s c)
                lst))
      in
      (e', s, env')

let init_abstraction (init : tree) : abstraction =
  let sketch = texpr_of_tree init in
  { sketch; init = []; steps = [] }

let add_step ({ sketch; init; steps } : abstraction)
    ((action, edits) : demo_step) : abstraction =
  let last_env =
    match List.rev steps with [] -> init | (_, env) :: _ -> env
  in
  let sketch', s, env' =
    List.fold_left
      (fun (e, s, env) (path, edit) ->
        let e', s', env' = abstract_step_traverse path edit e env in
        (e', Fun.compose s' s, env'))
      (sketch, Fun.id, last_env) edits
  in
  let steps' = List.map (fun (a, e) -> (a, s e)) steps @ [ (action, env') ] in
  { sketch = sketch'; init = s init; steps = steps' }

let abstract_demo ({ init; steps } : demo) : abstraction =
  let init = init_abstraction init in
  List.fold_left add_step init steps
