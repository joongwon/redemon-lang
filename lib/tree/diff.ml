open Syntax

type vexpr = Const of const | Var of string

type texpr =
  | Val of vexpr
  | Elem of { name : string; attrs : (string * vexpr) list; children : lexpr }
  | Cond of { cond : vexpr; left : texpr; right : texpr }

and lexpr = Fixed of texpr list | Map of { var : string; body : texpr }

type value = Const of const | Undef | Dontcare | List of record list
and record = (string * value) list

type diff = texpr * value list

let env_var = "env"

let string_of_vexpr (e : vexpr) =
  match e with
  | Const c -> Syntax.string_of_const c
  | Var v -> env_var ^ "." ^ v

let rec string_of_texpr e =
  match e with
  | Val v -> string_of_vexpr v
  | Elem { name; attrs; children } ->
      let attrs_str =
        String.concat " "
          (List.map (fun (k, v) -> k ^ "=" ^ string_of_vexpr v) attrs)
      in
      let children_str = string_of_lexpr children in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str children_str name
  | Cond { cond; left; right } ->
      Printf.sprintf "%s ? %s : %s" (string_of_vexpr cond)
        (string_of_texpr left) (string_of_texpr right)

and string_of_texpr_child e =
  match e with
  | Val _ | Cond _ -> "{" ^ string_of_texpr e ^ "}"
  | Elem _ -> string_of_texpr e

and string_of_lexpr e =
  match e with
  | Fixed es -> String.concat "" (List.map string_of_texpr_child es)
  | Map { var; body } ->
      Printf.sprintf "{%s.map(%s => %s)}"
        (env_var ^ "." ^ var)
        env_var (string_of_texpr body)
