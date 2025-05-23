open Tree.Syntax

type vexpr = Const of const | Var of string
type aexpr = AttrConst of vexpr | AttrFunc of label

type texpr =
  | Skip
  | Val of vexpr
  | Elem of { name : string; attrs : (string * aexpr) list; children : lexpr }
  | Cond of { cond : vexpr; left : texpr; right : texpr }

and lexpr = Fixed of texpr list | Map of { var : string; body : texpr }

type value = Const of const | Null | List of record list
and record = (string * value) list

type diff = texpr * value list

let env_var = "env"

let string_of_vexpr (e : vexpr) =
  match e with Const c -> string_of_const c | Var v -> env_var ^ "." ^ v

let string_of_aexpr (e : aexpr) =
  match e with
  | AttrConst (Const (String s)) -> Printf.sprintf "\"%s\"" s
  | AttrConst v -> Printf.sprintf "{%s}" (string_of_vexpr v)
  | AttrFunc l -> Printf.sprintf "{$%d}" l

let rec string_of_texpr e =
  match e with
  | Skip -> "null"
  | Val v -> string_of_vexpr v
  | Elem { name; attrs; children } ->
      let attrs_str =
        String.concat " "
          (List.map (fun (k, v) -> k ^ "=" ^ string_of_aexpr v) attrs)
      in
      let children_str = string_of_lexpr children in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str children_str name
  | Cond { cond; left; right } ->
      Printf.sprintf "%s !== null ? %s : %s" (string_of_vexpr cond)
        (string_of_texpr left) (string_of_texpr right)

and string_of_texpr_child e =
  match e with
  | Skip | Val _ | Cond _ -> "{" ^ string_of_texpr e ^ "}"
  | Elem _ -> string_of_texpr e

and string_of_lexpr e =
  match e with
  | Fixed es -> String.concat "" (List.map string_of_texpr_child es)
  | Map { var; body } ->
      Printf.sprintf "{%s.map(%s => %s)}"
        (env_var ^ "." ^ var)
        env_var (string_of_texpr body)

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
  | Skip -> None
  | Val v -> (
      match veval v env with
      | Const c -> Some (tree_const c)
      | Null -> None
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
  | Cond { cond; left; right } -> (
      match veval cond env with Null -> teval right env | _ -> teval left env)

and leval (e : lexpr) (env : record) : tree list =
  match e with
  | Fixed es -> es |> List.filter_map (fun t -> teval t env)
  | Map { var; body } -> (
      let lst = veval (Var var) env in
      match lst with
      | List records -> records |> List.filter_map (fun r -> teval body r)
      | _ -> failwith "Expected a list")
