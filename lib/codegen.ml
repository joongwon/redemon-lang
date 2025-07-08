open Tree.Syntax
open Texpr
open Demo
open Synthesis
open Stdlib.Effect
open Stdlib.Effect.Deep

type prog = {
  view : expr;
  data : expr;
  handlers : synthesized_rule list;
  states : (var * value) list;
}
[@@deriving show, eq]

(* translation of expressions to js syntax *)

(* context of translation *)
type _ eff += Get_prefix : string eff | Get_handler : label -> string eff

(* every prop name is prefixed with this, for example: 0 -> "x0" *)
let prop_prefix = "x"

(* prefix: prefix appended before Access, for example: prefix="data.x", var=0 ->
   "data.x0" *)
let with_prefix (prefix : string) (f : 'a -> 'b) (x : 'a) : 'b =
  match f x with r -> r | effect Get_prefix, k -> continue k prefix

let rec js_of_expr (e : expr) : string =
  match e with
  | Const (String s) -> Printf.sprintf "\"%s\"" s
  | Const (Int i) -> Printf.sprintf "%d" i
  | Access (Var v) ->
      let prefix = perform Get_prefix in
      Printf.sprintf "%s%d" prefix v
  | Elem { name; attrs; children } ->
      let attrs_str =
        String.concat " "
          (List.map
             (fun (k, v) ->
               match (v : expr) with
               | Const (String s) -> Printf.sprintf "%s=\"%s\"" k s
               | _ -> Printf.sprintf "%s={%s}" k (js_of_expr v))
             attrs)
      in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str (jsx_of_expr children) name
  | HandlerHole l -> js_of_handler l
  | List es -> "[" ^ String.concat ", " (List.map js_of_expr es) ^ "]"
  | Record r ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (Var v, e') ->
               Printf.sprintf "%s%d: %s" prop_prefix v (js_of_expr e'))
             r)
      ^ "}"
  | OptionMap { opt = Var opt; body } ->
      let prefix = perform Get_prefix in
      let opt = Printf.sprintf "%s%d" prefix opt in
      Printf.sprintf "(%s ? %s : null)" opt (with_prefix opt js_of_expr body)
  | ListMap { lst = Var lst; body } ->
      let prefix = perform Get_prefix in
      let lst = Printf.sprintf "%s%d" prefix lst in
      let arg = "item" in
      (* could be any name *)
      let inner_prefix = Printf.sprintf "%s.%s" arg prop_prefix in
      Printf.sprintf "%s.map(%s => %s)" lst arg
        (with_prefix inner_prefix js_of_expr body)
  | Fun { func; args } ->
      let args = List.map js_of_expr args in
      Synthesis.Candidate.func_to_js func args

and jsx_of_expr (e : expr) : string =
  match e with
  | Const c -> string_of_const c
  | Elem _ -> js_of_expr e
  | List es -> String.concat "\n" (List.map jsx_of_expr es)
  | _ -> Printf.sprintf "{%s}" (js_of_expr e)

and js_of_handler (l : label) : string = perform (Get_handler l)

let js_of_attr_value (v : attr_value) : string =
  match v with
  | AttrConst (String s) -> Printf.sprintf "\"%s\"" s
  | AttrConst (Int i) -> Printf.sprintf "{%d}" i
  | AttrFunc l -> Printf.sprintf "{%s}" (js_of_handler l)

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
  | Const c -> js_of_const c
  | HandlerHole l -> js_of_handler l
  | Null -> "null"
  | List vs -> "[" ^ String.concat ", " (List.map js_of_value vs) ^ "]"
  | Record r ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (Var v, e) ->
               Printf.sprintf "%s%d: %s" prop_prefix v (js_of_value e))
             r)
      ^ "}"

let with_rules (rules : synthesized_rule list) (f : 'a -> 'b) (x : 'a) : 'b =
  match f x with
  | r -> r
  | effect Get_handler label, k ->
      let handlers =
        List.filter (fun (rule : synthesized_rule) -> rule.label = label) rules
      in
      if List.is_empty handlers then continue k "undefined"
      else
        let rule = List.hd handlers in
        let action_type = rule.action_type in
        let sets = List.map (fun rule -> (rule.state, rule.func)) handlers in
        let sets_str =
          String.concat "\n"
            (List.map
               (fun (Var v, e) ->
                 Printf.sprintf "setS%d(%s);" v (with_prefix "s" js_of_expr e))
               sets)
        in
        (match action_type with
        | Demo.Input ->
            Printf.sprintf "e => {\n  let input = e.target.value;\n%s}" sets_str
        | Demo.Click -> Printf.sprintf "() => {\n%s}" sets_str)
        |> continue k

let with_rules_todo (f : 'a -> 'b) (x : 'a) : 'b =
  match f x with
  | r -> r
  | effect Get_handler (Label (l, _)), k ->
      Printf.sprintf "e => { /* TODO: implement handler %d */ }" l |> continue k

let js_of_prog (p : prog) : string =
  "function App() {\n"
  ^ String.concat ""
      (List.map
         (fun (Var v, value) ->
           Printf.sprintf "  const [s%d, setS%d] = useState(%s);\n" v v
             (js_of_value value))
         p.states)
  ^ Printf.sprintf "  const data = %s;\n" (with_prefix "s" js_of_expr p.data)
  ^ Printf.sprintf "  return %s;\n"
      (((fun () -> js_of_expr p.view)
       |> with_prefix ("data." ^ prop_prefix)
       |> with_rules p.handlers)
         ())
  ^ "}\n\n" ^ "render(<App />);\n"

let js_of_abs (abs : Abstract.abstraction_multi) : string =
  "function App() {\n"
  ^ String.concat ""
      (List.map
         (fun (Var v, value) ->
           Printf.sprintf "  const [s%d, setS%d] = useState(%s);\n" v v
             (js_of_value value))
         abs.init)
  ^ Printf.sprintf "  return %s;\n"
      (((fun () -> js_of_expr abs.sketch) |> with_prefix "s" |> with_rules_todo)
         ())
  ^ "}\n\n" ^ "render(<App />);\n" ^ "/* Demo timelines:\n" ^ "  Init:\n"
  ^ String.concat "\n"
      (List.map
         (fun (Var v, e) -> Printf.sprintf "      s%d: %s" v (js_of_value e))
         abs.init)
  ^ "\n\n"
  ^ String.concat "\n\n"
      (List.mapi
         (fun i timeline ->
           Printf.sprintf "  Timeline %d:\n" i
           ^ String.concat "\n"
               (List.map
                  (fun ({ label = Label (l, k); action_type; arg }, record) ->
                    let label_str =
                      match k with
                      | None -> Printf.sprintf "Handler %d" l
                      | Some s -> Printf.sprintf "Handler %d (key=%d)" l s
                    in
                    let action_type_str =
                      match action_type with
                      | Demo.Click -> "Click"
                      | Demo.Input -> "Input"
                    in
                    let arg_str =
                      match arg with
                      | None -> ""
                      | Some a -> Printf.sprintf " with arg '%s'" a
                    in
                    let action_str =
                      Printf.sprintf "    %s %s%s" label_str action_type_str
                        arg_str
                    in
                    Printf.sprintf "%s\n%s" action_str
                      (String.concat "\n"
                         (List.map
                            (fun (Var v, e) ->
                              Printf.sprintf "      s%d: %s" v (js_of_value e))
                            record)))
                  timeline))
         abs.timelines)
  ^ "\n*/\n"
