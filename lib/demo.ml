open Tree.Syntax

type path = int list

type edit =
  | Dup of int
  | Del of int
  | Replace of const
  | Insert of int * tree
  | SetAttr of string * const option

type event = Click | Input [@@deriving eq, show]
type action = label * event * string option [@@deriving eq, show]
type demo_step = action * (path * edit) list
type demo = { init : tree; steps : demo_step list }

exception Type_error of string

(* semantics of edit *)
let apply_do (edit : edit) (t : tree) : tree =
  match (edit, t) with
  | Dup i, Elem ({ children; _ } as t) ->
      if i >= List.length children then
        raise (Invalid_argument "Index out of bounds for duplication");
      (* Duplicate the i-th child *)
      let children' =
        List.mapi (fun j c -> if j = i then [ c; c ] else [ c ]) children
        |> List.concat
      in
      Elem { t with children = children' }
  | Del i, Elem ({ children; _ } as t) ->
      if i >= List.length children then
        raise (Invalid_argument "Index out of bounds for deletion");
      (* Remove the i-th child *)
      let children' = List.filteri (fun j _ -> j <> i) children in
      Elem { t with children = children' }
  | Replace _, Elem _ -> raise (Type_error "Cannot replace Elem with Const")
  | Replace new_tree, Const _ -> Const new_tree
  | Insert (i, new_tree), Elem ({ children; _ } as t) ->
      if i > List.length children then
        raise (Invalid_argument "Index out of bounds for insertion");
      (* Insert new_tree at index i *)
      let children' =
        if i = List.length children then children @ [ new_tree ]
        else
          List.mapi
            (fun j c -> if j = i then [ new_tree; c ] else [ c ])
            children
          |> List.concat
      in
      Elem { t with children = children' }
  | SetAttr (key, None), Elem ({ attrs; _ } as t) ->
      (* Remove the attribute if it exists *)
      let attrs' = List.remove_assoc key attrs in
      Elem { t with attrs = attrs' }
  | SetAttr (key, Some value), Elem ({ attrs; _ } as t) ->
      (* Set or update the attribute *)
      let attrs' =
        (key, Tree.Syntax.AttrConst value) :: List.remove_assoc key attrs
      in
      Elem { t with attrs = attrs' }
  | (Dup _ | Del _ | Insert _ | SetAttr _), Const _ ->
      raise (Type_error "Cannot apply edit to Const")

let rec apply_traverse (path : path) (edit : edit) (t : tree) : tree =
  match path with
  | [] -> apply_do edit t
  | i :: rest -> (
      match t with
      | Elem ({ children; _ } as t) ->
          if i >= List.length children then
            raise (Invalid_argument "Index out of bounds for path");
          (* Traverse to the i-th child and apply the edit *)
          let children' =
            List.mapi
              (fun j c -> if j = i then apply_traverse rest edit c else c)
              children
          in
          Elem { t with children = children' }
      | Const _ -> raise (Invalid_argument "Cannot traverse into Const"))
