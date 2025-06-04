open Tree.Syntax
open Texpr
open Demo

type prog = {
  view : expr;
  data : expr;
  handlers : (label * event * (var * expr) list) list;
  states : (var * value) list;
}

(* translation of expressions to js syntax *)

(* every prop name is prefixed with this, for example: 0 -> "x0" *)
let prop_prefix = "x"

(* prefix: prefix appended before Access, for example: prefix="data.x", var=0 -> "data.x0" *)
let rec js_of_expr ~(prefix : string) (e : expr) : string =
  match e with
  | Const c -> string_of_const c
  | Access v -> Printf.sprintf "%s%d" prefix v
  | Elem { name; attrs; children } ->
      let attrs_str =
        String.concat " "
          (List.map
             (fun (k, v) -> Printf.sprintf "%s={%s}" k (js_of_expr ~prefix v))
             attrs)
      in
      let children_str = jsx_of_expr ~prefix children in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str children_str name
  | HandlerHole l -> Printf.sprintf "$%d" l
  | List es -> "[" ^ String.concat ", " (List.map (js_of_expr ~prefix) es) ^ "]"
  | Record r ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (v, e') ->
               Printf.sprintf "%s%d: %s" prop_prefix v (js_of_expr ~prefix e'))
             r)
      ^ "}"
  | OptionMap { opt; body } ->
      let opt = Printf.sprintf "%s%d" prefix opt in
      Printf.sprintf "(%s ? %s : null)" opt (js_of_expr ~prefix:opt body)
  | ListMap { lst; body } ->
      let lst = Printf.sprintf "%s%d" prefix lst in
      let arg = "item" in
      (* could be any name *)
      let inner_prefix = Printf.sprintf "%s.%s" arg prop_prefix in
      Printf.sprintf "%s.map(%s => %s)" lst arg
        (js_of_expr ~prefix:inner_prefix body)

and jsx_of_expr ~(prefix : string) (e : expr) : string =
  match e with
  | Const c -> string_of_const c
  | Elem _ -> js_of_expr ~prefix e
  | List es -> String.concat "\n" (List.map (jsx_of_expr ~prefix) es)
  | _ -> Printf.sprintf "{%s}" (js_of_expr ~prefix e)

let js_of_attr_value (v : attr_value) : string =
  match v with
  | AttrConst (String s) -> Printf.sprintf "\"%s\"" s
  | AttrConst (Int i) -> Printf.sprintf "{%d}" i
  | AttrFunc l -> Printf.sprintf "$%d" l

let rec js_of_tree (t : tree) : string =
  match t with
  | Const c -> string_of_const c
  | Elem { name; attrs; children } ->
      let attrs_str =
        String.concat " "
          (List.map
             (fun (k, v) -> Printf.sprintf "%s={%s}" k (js_of_attr_value v))
             attrs)
      in
      let children_str = String.concat "\n" (List.map js_of_tree children) in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str children_str name

let rec js_of_value (v : value) : string =
  match v with
  | Tree t -> js_of_tree t
  | Const c -> string_of_const c
  | HandlerHole l -> Printf.sprintf "$%d" l
  | Null -> "null"
  | List vs -> "[" ^ String.concat ", " (List.map js_of_value vs) ^ "]"
  | Record r ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (v, e) -> Printf.sprintf "x%d: %s" v (js_of_value e))
             r)
      ^ "}"

let js_of_prog (p : prog) : string =
  "function App() {\n"
  ^ String.concat ""
      (List.map
         (fun (v, value) ->
           Printf.sprintf "  const [s%d, setS%d] = useState(%s);\n" v v
             (js_of_value value))
         p.states)
  ^ Printf.sprintf "  const data = %s;\n" (js_of_expr ~prefix:"s" p.data)
  ^ String.concat ""
      (List.map
         (fun (label, event, body) ->
           let body =
             String.concat "\n"
               (List.map
                  (fun (v, e) ->
                    Printf.sprintf "    setS%d(%s);\n" v
                      (js_of_expr ~prefix:"s" e))
                  body)
           in
           match event with
           | Demo.Input ->
               Printf.sprintf
                 "  const $%d = (e) => {\n\
                 \  let input = e.target.value;\n\
                  %s  };\n"
                 label body
           | Demo.Click ->
               Printf.sprintf "  const $%d = () => {\n%s  };\n" label body)
         p.handlers)
  ^ Printf.sprintf "  return %s;\n"
      (js_of_expr ~prefix:("data." ^ prop_prefix) p.view)
  ^ "}\n\n" ^ "render(<App />, document.getElementById('root'));\n"
