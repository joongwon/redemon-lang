open Tree.Syntax
open Demo
open Texpr

type abstraction = {
  sketch : expr;
  init : record;
  steps : (action * record) list;
}
[@@deriving eq, show]

let expr_of_attr_value (v : attr_value) : expr =
  match v with AttrFunc l -> HandlerHole l | AttrConst c -> Const c

(* expr_of_tree t = e => eval e [] = t *)
let rec expr_of_tree (t : tree) : expr =
  match t with
  | Const c -> Const c
  | Elem { name; attrs; children } ->
      Elem
        {
          name;
          attrs = List.map (fun (k, v) -> (k, expr_of_attr_value v)) attrs;
          children = List (List.map expr_of_tree children);
        }

let record_update (r : record) (var : var) (v : value) : record =
  (var, v) :: List.remove_assoc var r

let fresh_var (r : record) : var =
  let keys = List.map (fun (Var k, _) -> k) r in
  Var (1 + List.fold_left max 0 keys)

let rec free_vars (e : expr) : var list =
  match e with
  | Const _ -> []
  | Access var -> [ var ]
  | Elem { name = _; attrs; children } ->
      free_vars children @ List.concat_map (fun (_, v) -> free_vars v) attrs
  | ListMap { lst; _ } -> [ lst ]
  | HandlerHole _ -> []
  | OptionMap { opt; _ } -> [ opt ]
  | Record kvs -> List.concat_map (fun (_, v) -> free_vars v) kvs
  | List l -> List.concat_map free_vars l
  | Fun _ -> []

let list_of_value (v : value) : value list =
  match v with
  | List lst -> lst
  | _ -> raise (Invalid_argument "Expected List for list_of_value")

let abstract_step (edit : edit) (e : expr) (env : record) :
    expr * (record -> record) * record =
  match (edit, e) with
  | Replace new_c, Access var ->
      let e' = e in
      let s = Fun.id in
      let env' = record_update env var (Const new_c) in
      (e', s, env')
  | Replace new_c, Const old_c ->
      let var = fresh_var env in
      let e' = Access var in
      let s env = record_update env var (Const old_c) in
      let env' = record_update env var (Const new_c) in
      (e', s, env')
  | ( Dup (Index 0),
      Elem ({ children = List [ OptionMap { opt = var; body } ]; _ } as elem) )
    ->
      let e' = Elem { elem with children = ListMap { lst = var; body } } in
      let s env =
        (* convert option to list *)
        let r = List.assoc var env in
        let l =
          match r with
          | Record _ -> [ r ]
          | Null -> []
          | _ -> raise (Invalid_argument "Expected Record for Dup")
        in
        record_update env var (List l)
      in
      let env' =
        let r = List.assoc var env in
        record_update env var (List [ r; r ])
      in
      (e', s, env')
  | Dup (Index 0), Elem ({ children = List [ child ]; _ } as elem) ->
      let var = fresh_var env in
      let vars = free_vars child in
      let e' =
        Elem { elem with children = ListMap { lst = var; body = child } }
      in
      let s env =
        let child_env, new_env =
          List.partition (fun (v, _) -> List.mem v vars) env
        in
        record_update new_env var (List [ Record child_env ])
      in
      let env' =
        let child_env, new_env =
          List.partition (fun (v, _) -> List.mem v vars) env
        in
        record_update new_env var (List [ Record child_env; Record child_env ])
      in
      (e', s, env')
  | Dup (Index i), Elem { children = ListMap { lst = var; _ }; _ } ->
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
  | Del (Index i), Elem ({ children = List children; _ } as elem) ->
      if i >= List.length children then
        raise (Invalid_argument "Index out of bounds for deletion");
      let var = fresh_var env in
      let child = List.nth children i in
      let vars = free_vars child in
      let children' =
        List.mapi
          (fun j c ->
            if j = i then [ OptionMap { opt = var; body = child } ] else [ c ])
          children
        |> List.concat
      in
      let e' = Elem { elem with children = List children' } in
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
  | Del (Index i), Elem { children = ListMap { lst = var; _ }; _ } ->
      let lst = List.assoc var env |> list_of_value in
      if i >= List.length lst then
        raise (Invalid_argument "Index out of bounds for deletion");
      let env' =
        record_update env var (List (List.filteri (fun j _ -> j <> i) lst))
      in
      (e, Fun.id, env')
  | Insert (Index i, new_tree), Elem ({ children = List children; _ } as elem)
    ->
      if i > List.length children then
        raise (Invalid_argument "Index out of bounds for insertion");
      let new_texpr = expr_of_tree new_tree in
      let var = fresh_var env in
      let children' =
        if i = List.length children then
          children @ [ OptionMap { opt = var; body = new_texpr } ]
        else
          List.mapi
            (fun j c ->
              if j = i then [ OptionMap { opt = var; body = new_texpr }; c ]
              else [ c ])
            children
          |> List.concat
      in
      let e' = Elem { elem with children = List children' } in
      let s env = record_update env var Null in
      let env' = record_update env var (Record []) in
      (e', s, env')
  | SetAttr (key, attr), Elem ({ attrs; _ } as elem) -> (
      match List.assoc_opt key attrs with
      | Some (Access var) ->
          let e' = e in
          let s = Fun.id in
          let env' =
            record_update env var
              (match attr with Some c -> Const c | None -> Null)
          in
          (e', s, env')
      | Some (Const v) ->
          let var = fresh_var env in
          let attrs' = (key, Access var) :: List.remove_assoc key attrs in
          let e' = Elem { elem with attrs = attrs' } in
          let s env = record_update env var (Const v) in
          let env' =
            record_update env var
              (match attr with Some c -> Const c | None -> Null)
          in
          (e', s, env')
      | Some _ ->
          raise (Invalid_argument "Setting non-const attribute not supported")
      | None ->
          let var = fresh_var env in
          let attrs' = (key, Access var) :: attrs in
          let e' = Elem { elem with attrs = attrs' } in
          let s env = record_update env var Null in
          let env' =
            record_update env var
              (match attr with Some c -> Const c | None -> Null)
          in
          (e', s, env'))
  | _, _ -> raise (Invalid_argument "Unsupported edit type or expression")

let record_of_value (v : value) : record =
  match v with
  | Record r -> r
  | _ -> raise (Invalid_argument "Expected Record for record_of_value")

let s_value (s : record -> record) (v : value) : value =
  match v with
  | Record r -> Record (s r)
  | _ -> raise (Invalid_argument "Expected Record for apply_substitution_value")

(* abstract_step_traverse path edit e env0 = (e', s, env') =>
   (forall env ~ env0, teval e' (s env) = teval e env) AND
   teval e' env' = apply_traverse path edit (teval e' (s env0)) *)
let rec abstract_step_traverse (path : path) (edit : edit) (e : expr)
    (env : record) : expr * (record -> record) * record =
  match (e, path) with
  | OptionMap { opt = var; body }, _ ->
      (* traverse into OMap *)
      (* the path is editable only if it is not null *)
      let inner_env = List.assoc var env |> record_of_value in
      let body', inner_s, inner_env' =
        abstract_step_traverse path edit body inner_env
      in
      let e' = OptionMap { opt = var; body = body' } in
      let s env =
        (* other pathes with same type might be null *)
        match List.assoc var env with
        | Null -> env
        | Record inner_env ->
            let inner_env' = inner_s inner_env in
            record_update env var (Record inner_env')
        | _ -> raise (Invalid_argument "Expected Record or Null for OMap")
      in
      let env' = record_update env var (Record inner_env') in
      (e', s, env')
  | _, [] ->
      (* no more path, apply the edit directly *)
      abstract_step edit e env
  | Elem ({ children = List children; _ } as elem), Index i :: rest ->
      if i >= List.length children then
        raise (Invalid_argument "Index out of bounds for path");
      let child_e = List.nth children i in
      let child_e', s, env' = abstract_step_traverse rest edit child_e env in
      let children' =
        List.mapi (fun j c -> if j = i then child_e' else c) children
      in
      let e' = Elem { elem with children = List children' } in
      (e', s, env')
  | ( Elem ({ children = ListMap { lst = var; body }; _ } as elem),
      Index i :: rest ) ->
      let lst = List.assoc var env |> list_of_value in
      if i >= List.length lst then
        raise (Invalid_argument "Index out of bounds for path");
      let child_env = List.nth lst i |> record_of_value in
      let body', child_s, child_env' =
        abstract_step_traverse rest edit body child_env
      in
      let e' =
        Elem { elem with children = ListMap { lst = var; body = body' } }
      in
      let s env =
        let lst = List.assoc var env |> list_of_value in
        record_update env var (List (List.map (s_value child_s) lst))
      in
      let env' =
        record_update env var
          (List
             (List.mapi
                (fun j c ->
                  if j = i then Record child_env' else s_value child_s c)
                lst))
      in
      (e', s, env')
  | _ ->
      raise (Invalid_argument "Unsupported expression or path for abstraction")

let init_abstraction (init : tree) : abstraction =
  let sketch = expr_of_tree init in
  { sketch; init = []; steps = [] }

let add_step ({ sketch; init; steps } : abstraction)
    ({ action; edits } : demo_step) : abstraction =
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
