open Tree.Syntax

type var = int
[@@deriving eq, show]
type vexpr = Const of const | Var of var
[@@deriving eq, show]
type aexpr = AttrConst of vexpr | AttrFunc of label
[@@deriving eq, show]

type texpr = Val of vexpr | Elem of elem | OMap of { var : var; body : texpr }
and elem = { name : string; attrs : (string * aexpr) list; children : lexpr }
and lexpr = Fixed of texpr list | LMap of { var : var; body : texpr }
[@@deriving eq, show]

type value = Const of const | Null | List of record list | Record of record
and record = (var * value) list
[@@deriving eq, show]

let record_update (r : record) (v : var) (value : value) : record =
  (v, value) :: List.remove_assoc v r

let const_of_value (v : value) : const =
  match v with Const c -> c | _ -> failwith "Expected a constant value"

let list_of_value (v : value) : record list =
  match v with List l -> l | _ -> failwith "Expected a list value"

let record_of_value (v : value) : record =
  match v with Record r -> r | _ -> failwith "Expected a record value"

let var_of_aexpr_opt (a : aexpr) : var option =
  match a with AttrConst (Var v) -> Some v | AttrConst _ | AttrFunc _ -> None

let fresh_var (record : record) : var =
  List.fold_left (fun acc (k, _) -> max acc k) (-1) record + 1

let free_vars_vexpr (e : vexpr) : var list =
  match e with Const _ -> [] | Var v -> [ v ]

let free_vars_aexpr (e : aexpr) : var list =
  match e with AttrConst v -> free_vars_vexpr v | AttrFunc _ -> []

let rec free_vars_texpr (e : texpr) : var list =
  match e with
  | Val v -> free_vars_vexpr v
  | Elem { attrs; children; _ } ->
      let attrs_vars =
        List.concat (List.map (fun (_, v) -> free_vars_aexpr v) attrs)
      in
      let children_vars = free_vars_lexpr children in
      attrs_vars @ children_vars
  | OMap { var; _ } -> [ var ]

and free_vars_lexpr (e : lexpr) : var list =
  match e with
  | Fixed es -> List.concat (List.map free_vars_texpr es)
  | LMap { var; _ } -> [ var ]

let string_of_vexpr ~(prefix : string) (e : vexpr) =
  match e with
  | Const c -> string_of_const c
  | Var v -> prefix ^ string_of_int v

let string_of_aexpr ~(prefix : string) (e : aexpr) =
  match e with
  | AttrConst (Const (String s)) -> Printf.sprintf "\"%s\"" s
  | AttrConst v -> Printf.sprintf "{%s}" (string_of_vexpr ~prefix v)
  | AttrFunc l -> Printf.sprintf "{$%d}" l

let rec string_of_texpr ~(prefix : string) (e : texpr) =
  match e with
  | Val v -> string_of_vexpr ~prefix v
  | Elem { name; attrs; children } ->
      let attrs_str =
        String.concat " "
          (List.map (fun (k, v) -> k ^ "=" ^ string_of_aexpr ~prefix v) attrs)
      in
      let children_str = string_of_lexpr ~prefix children in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str children_str name
  | OMap { var; body } ->
      let inner_var = prefix ^ string_of_int var in
      let inner_prefix = inner_var ^ ".x" in
      Printf.sprintf "%s && %s" inner_var
        (string_of_texpr ~prefix:inner_prefix body)

and string_of_texpr_child ~prefix e =
  match e with
  | Val _ | OMap _ -> "{" ^ string_of_texpr ~prefix e ^ "}"
  | Elem _ -> string_of_texpr ~prefix e

and string_of_lexpr ~prefix e =
  match e with
  | Fixed es -> String.concat "" (List.map (string_of_texpr_child ~prefix) es)
  | LMap { var; body } ->
      let inner_var = "item" in
      let inner_prefix = inner_var ^ ".x" in
      Printf.sprintf "{%s.map(%s => %s)}"
        (prefix ^ string_of_int var)
        inner_var
        (string_of_texpr ~prefix:inner_prefix body)

let default_prefix = "x"

let veval (e : vexpr) (env : record) : value =
  match e with Const c -> Const c | Var v -> List.assoc v env

let aeval (e : aexpr) (env : record) : attr_value option =
  match e with
  | AttrConst v -> (
      match veval v env with
      | Const c -> Some (AttrConst c)
      | Null -> None
      | _ -> failwith "Expected a constant or null")
  | AttrFunc l -> Some (AttrFunc l)

let rec teval (e : texpr) (env : record) : tree option =
  match e with
  | Val v -> (
      match veval v env with
      | Const c -> Some (tree_const c)
      | _ -> failwith "Expected a constant")
  | Elem { name; attrs; children } ->
      Some
        (Elem
           {
             name;
             attrs =
               attrs
               |> List.filter_map (fun (k, v) ->
                      aeval v env |> Option.map (fun v -> (k, v)));
             children = leval children env;
           })
  | OMap { var; body } -> (
      match List.assoc var env with
      | Record r -> teval body r
      | Null -> None
      | _ -> failwith "Expected a record or null")

and leval (e : lexpr) (env : record) : tree list =
  match e with
  | Fixed es -> es |> List.filter_map (fun t -> teval t env)
  | LMap { var; body } -> (
      let lst = veval (Var var) env in
      match lst with
      | List records -> records |> List.filter_map (fun r -> teval body r)
      | _ -> failwith "Expected a list")
