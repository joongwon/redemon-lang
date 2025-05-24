open Tree.Syntax
open Texpr

type path = int list
type action = Click of label | Input of label * string

type edit =
  | Dup of int
  | Del of int
  | Replace of const
  | Insert of int * tree
  | SetAttr of string * const option

let replace_of_edit (edit : edit) : const =
  match edit with Replace c -> c | _ -> failwith "Not a replace edit"

(* semantics of edit *)
let rec apply (path : path) (edit : edit) (t : tree) : tree option =
  match path with
  | [] -> (
      match (edit, t) with
      | Dup i, Elem ({ children; _ } as t) ->
          if i >= List.length children then
            failwith "Index out of bounds for duplication";
          let updated_children =
            List.mapi (fun j c -> if j = i then [ c; c ] else [ c ]) children
            |> List.concat
          in
          Some (Elem { t with children = updated_children })
      | Del i, Elem ({ children; _ } as t) ->
          if i >= List.length children then
            failwith "Index out of bounds for deletion";
          let updated_children = List.filteri (fun j _ -> j <> i) children in
          Some (Elem { t with children = updated_children })
      | Replace new_tree, _ -> Some (Const new_tree)
      | Insert (i, new_tree), Elem ({ children; _ } as t) ->
          if i > List.length children then
            failwith "Index out of bounds for insertion";
          if i = List.length children then
            Some (Elem { t with children = children @ [ new_tree ] })
          else
            Some
              (Elem
                 {
                   t with
                   children =
                     List.mapi
                       (fun j c -> if j = i then [ new_tree; c ] else [ c ])
                       children
                     |> List.concat;
                 })
      | SetAttr (key, None), Elem ({ attrs; _ } as t) ->
          let updated_attrs = List.remove_assoc key attrs in
          Some (Elem { t with attrs = updated_attrs })
      | SetAttr (key, Some value), Elem ({ attrs; _ } as t) ->
          let updated_attrs =
            (key, Tree.Syntax.AttrConst value) :: List.remove_assoc key attrs
          in
          Some (Elem { t with attrs = updated_attrs })
      | _ -> failwith "Invalid edit or path")
  | i :: rest -> (
      match t with
      | Elem ({ children; _ } as t) when i < List.length children ->
          Some
            (Elem
               {
                 t with
                 children =
                   List.mapi
                     (fun j c -> if j = i then apply rest edit c else Some c)
                     children
                   |> List.filter_map Fun.id;
               })
      | _ -> failwith "Invalid path or index out of bounds")

type demo_step = action * (path * edit) list
type demo = { init : tree; steps : demo_step list }

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

(* abstract e env path edit = (e', s, env') =>
   (forall env, teval e' (s env) = teval e env) AND
   teval e' env' = apply path edit (teval e' (s env)) *)
let rec abstract (e : texpr) (env : record) (path : path) (edit : edit) :
    texpr * (record -> record) * record =
  match (e, path) with
  | OMap { var; body }, _ ->
      let inner_env = List.assoc var env |> record_of_value in
      let body', inner_s, inner_env' = abstract body inner_env path edit in
      let e' = OMap { var; body = body' } in
      let s env =
        let inner_env = inner_s (List.assoc var env |> record_of_value) in
        record_update env var (Record inner_env)
      in
      let env' = record_update env var (Record inner_env') in
      (e', s, env')
  | Val v, [] -> (
      let new_c = replace_of_edit edit in
      match v with
      | Const old_c ->
          let var = fresh_var env in
          let e' = Val (Var var) in
          let s env = record_update env var (Const old_c) in
          let env' = record_update env var (Const new_c) in
          (e', s, env')
      | Var var ->
          let e' = e in
          let s = Fun.id in
          let env' = record_update env var (Const new_c) in
          (e', s, env'))
  | Val _, _ -> failwith "Cannot apply path to Val with non-empty path"
  | Elem ({ children = Fixed children; _ } as elem), i :: rest ->
      if i >= List.length children then failwith "Index out of bounds for path";
      let child_e = List.nth children i in
      let child_e', s, env' = abstract child_e env rest edit in
      let children' =
        List.mapi (fun j c -> if j = i then child_e' else c) children
      in
      let e' = Elem { elem with children = Fixed children' } in
      (e', s, env')
  | Elem ({ children = LMap { var; body }; _ } as elem), i :: rest ->
      let lst = List.assoc var env |> list_of_value in
      if i >= List.length lst then failwith "Index out of bounds for path";
      let child_env = List.nth lst i in
      let body', child_s, child_env' = abstract body child_env rest edit in
      let e' = Elem { elem with children = LMap { var; body = body' } } in
      let s env =
        let lst = List.assoc var env |> list_of_value in
        record_update env var (List (List.map child_s lst))
      in
      let env' =
        record_update env var
          (List
             (List.mapi
                (fun j v -> if j = i then child_env' else child_s v)
                lst))
      in
      (e', s, env')
  | Elem elem, [] -> (
      match (edit, elem) with
      | Replace _, _ -> failwith "Replace of Elem not supported"
      | Dup 0, { children = Fixed [ child ]; _ } ->
          let vars = free_vars_texpr child in
          let child_env = List.filter (fun (v, _) -> List.mem v vars) env in
          let var = fresh_var env in
          let e' = Elem { elem with children = LMap { var; body = child } } in
          let s env = record_update env var (List [ child_env ]) in
          let env' = record_update env var (List [ child_env; child_env ]) in
          (e', s, env')
      | Dup _, { children = Fixed _; _ } ->
          failwith "Duplication not supported for fixed multiple children"
      | Dup i, { children = LMap { var; _ }; _ } ->
          let lst = List.assoc var env |> list_of_value in
          if i >= List.length lst then
            failwith "Index out of bounds for duplication";
          let e' = e in
          let s = Fun.id in
          let env' =
            record_update env var
              (List
                 (List.mapi (fun j v -> if j = i then [ v; v ] else [ v ]) lst
                 |> List.concat))
          in
          (e', s, env')
      | Del i, { children = Fixed children; _ } ->
          if i >= List.length children then
            failwith "Index out of bounds for deletion";
          let child = List.nth children i in
          let vars = free_vars_texpr child in
          let child_env = List.filter (fun (v, _) -> List.mem v vars) env in
          let var = fresh_var env in
          let children' =
            List.mapi
              (fun j c ->
                if j = i then [ OMap { var; body = child } ] else [ c ])
              children
            |> List.concat
          in
          let e' = Elem { elem with children = Fixed children' } in
          let s env = record_update env var (Record child_env) in
          let env' = record_update env var Null in
          (e', s, env')
      | Del i, { children = LMap { var; _ }; _ } ->
          let lst = List.assoc var env |> list_of_value in
          if i >= List.length lst then
            failwith "Index out of bounds for deletion";
          let env' =
            record_update env var (List (List.filteri (fun j _ -> j <> i) lst))
          in
          (e, Fun.id, env')
      | Insert (i, new_tree), { children = Fixed children; _ } ->
          if i > List.length children then
            failwith "Index out of bounds for insertion";
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
      | Insert _, { children = LMap _; _ } ->
          failwith "Insert not supported for LMap children"
      | SetAttr (key, attr), { attrs; _ } -> (
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
              let env' = record_update env var Null in
              (e', s, env')
          | Some (AttrFunc _) -> failwith "Cannot set attribute to a function"
          | None ->
              let var = fresh_var env in
              let attrs' = (key, AttrConst (Var var)) :: attrs in
              let e' = Elem { elem with attrs = attrs' } in
              let s env = record_update env var Null in
              let env' =
                record_update env var
                  (match attr with Some c -> Const c | None -> Null)
              in
              (e', s, env')))
