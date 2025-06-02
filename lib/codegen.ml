open Tree.Syntax
open Texpr
open Demo

type dexpr =
  | Const of const
  | Var of var
  | Null
  | List of dexpr list
  | Record of (var * dexpr) list
  | OMap of { var : var; body : dexpr }
  | LMap of { var : var; body : dexpr }

type hexpr = Const of const | Var of var | Input | Add of hexpr * hexpr

type prog = {
  view : texpr;
  data : dexpr;
  handlers : (label * event * (var * hexpr) list) list;
  states : (var * value) list;
}

let rec js_of_dexpr ~prefix (dexpr : dexpr) : string =
  match dexpr with
  | Const c -> string_of_const c
  | Var v -> prefix ^ string_of_int v
  | Null -> "null"
  | List l -> "[" ^ String.concat ", " (List.map (js_of_dexpr ~prefix) l) ^ "]"
  | Record r ->
      "{" ^ String.concat ", " (List.map (fun (v, d) -> Printf.sprintf "x%s: %s" (string_of_int v) (js_of_dexpr ~prefix d)) r) ^ "}"
  | OMap { var; body } ->
      let inner_var = prefix ^ string_of_int var in
      let inner_prefix = inner_var ^ ".x" in
      Printf.sprintf "%s && %s" inner_var
        (js_of_dexpr ~prefix:inner_prefix body)
  | LMap { var; body } ->
      let inner_var = "item" in
      let inner_prefix = inner_var ^ ".x" in
      Printf.sprintf "{%s.map(%s => %s)}"
        (prefix ^ string_of_int var)
        inner_var
        (js_of_dexpr ~prefix:inner_prefix body)

let rec js_of_hexpr ~prefix (hexpr : hexpr) : string =
  match hexpr with
  | Const c -> string_of_const c
  | Var v -> prefix ^ string_of_int v
  | Input -> "input"
  | Add (e1, e2) ->
      Printf.sprintf "(%s + %s)" (js_of_hexpr ~prefix e1) (js_of_hexpr ~prefix e2)

let js_of_prog (p : prog) : string =
  "function App() {\n" ^
  String.concat "" (List.map (fun (v, value) -> Printf.sprintf "  const [s%d, setS%d] = useState(%s);\n" v v (js_of_value value)) p.states) ^
  Printf.sprintf "  const data = %s;\n" (js_of_dexpr ~prefix:"s" p.data) ^
  String.concat "" (List.map (fun (label, event, body) ->
    let body =
      String.concat "\n" (List.map (fun (v, e) -> Printf.sprintf "    setS%d(%s);\n" v (js_of_hexpr ~prefix:"s" e)) body)
    in
    match event with
    | Demo.Input ->
        Printf.sprintf "  const $%d = (e) => {\n  let input = e.target.value;\n%s  };\n" label body
    | Demo.Click ->
        Printf.sprintf "  const $%d = () => {\n%s  };\n" label body
  ) p.handlers) ^
  Printf.sprintf "  return %s;\n" (js_of_texpr ~prefix:"data.x" p.view) ^
  "}\n\n" ^
  "render(<App />, document.getElementById('root'));\n"
