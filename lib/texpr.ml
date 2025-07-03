open Tree.Syntax

type var = Var of int [@@unboxed] [@@deriving eq, show]

(* an expression accepting single record (namely, 'root') as an environment *)
type expr =
  | Const of const
  | Access of var (* root.x *)
  | Elem of elem
  | HandlerHole of label
  | List of expr list
  | Record of (var * expr) list
    (* expr : const | record | null | list of record *)
  | OptionMap of { opt : var; (* opt : record | null *) body : expr }
    (* root.x ? (let root = root.x in body) : null *)
  | ListMap of {
      lst : var; (* lst : list of record *)
      body : expr; (* body : const | elem *)
    }
  | Fun of {
      func : string; (* e.g. "add", "mul" *)
      args : expr list; (* e.g. [root.x, root.y] *)
    }
(* root.x.map(λroot. body) *)

and elem = {
  name : string;
  attrs : (string * expr) list; (* expr : const | handler_hole *)
  children : expr; (* expr : list of (const | elem) *)
}
[@@deriving eq, show]

type value =
  | Tree of tree
  | Const of const
  | HandlerHole of label
  | Null
  | List of value list
  | Record of record

and record = (var * value) list [@@deriving eq, show]

exception NotFound of string

let lookup ~(var : var) (r : record) : value =
  try List.assoc var r
  with Not_found ->
    raise
      (NotFound
         (Printf.sprintf "Variable %s not found in record" (show_var var)))

(* definitional interpreter for expressions *)
let rec eval (e : expr) (root : record) : value =
  match e with
  | Const c -> Const c
  | Access v -> List.assoc v root
  | Elem { name; attrs; children } ->
      let attrs =
        List.map
          (fun (k, e') ->
            match eval e' root with
            | Const c -> (k, AttrConst c)
            | HandlerHole l -> (k, AttrFunc l)
            | _ -> failwith "Expected a constant or handler hole")
          attrs
      in
      let children =
        match eval children root with
        | List es ->
            List.map
              (fun e' ->
                match e' with
                | Const c -> tree_const c
                | Tree e'' -> e''
                | _ -> failwith "Expected a constant or tree element")
              es
        | _ -> failwith "Expected a list"
      in
      Tree (Elem { name; attrs; children })
  | HandlerHole l -> HandlerHole l
  | List es -> List (List.map (fun e' -> eval e' root) es)
  | Record r -> Record (List.map (fun (v, e') -> (v, eval e' root)) r)
  | OptionMap { opt; body } -> (
      match List.assoc opt root with
      | Null -> Null
      | Record r -> eval body r
      | _ -> failwith "Expected a record or null")
  | ListMap { lst; body } -> (
      match List.assoc lst root with
      | List records ->
          List
            (List.map
               (fun r ->
                 match r with
                 | Record r' -> eval body r'
                 | _ -> failwith "Expected a record in list")
               records)
      | _ -> failwith "Expected a list")
  | Fun _ -> failwith "Function not implemented in eval"
