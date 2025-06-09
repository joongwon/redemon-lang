open! Base
open Redemon_lang.Tree
open Redemon_lang

let position (lexbuf : Lexing.lexbuf) : string =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.tree =
  Parser.document Lexer.token lexbuf

let parse_program_str (program_str : string) : (Syntax.tree, string) Result.t =
  let lexbuf = Lexing.from_string program_str in
  match parse_with_error lexbuf with
  | prog -> Ok prog
  | exception Parser.Error ->
      Error (Printf.sprintf "%s: syntax error" (position lexbuf))

let synthesize (tree_src : string) (timelines : Demo.demo_timeline list) :
    (string, string) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  let* tree = parse_program_str tree_src in
  let demo = Demo.{ init = tree; timelines } in
  let abs = Abstract.abstract_demo_multi demo in
  let result =
    Synthesis.synthesize abs |> Synthesis.translate_synthesized_rules
  in
  let prog =
    Codegen.
      {
        view = abs.sketch;
        data = Record (List.map ~f:(fun (v, _) -> (v, Texpr.Access v)) abs.init);
        handlers = result;
        states = abs.init;
      }
  in
  Ok (Codegen.js_of_prog prog)

let () =
  (* Logs.set_reporter (Logs_browser.console_reporter ()); *)
  Logs.set_reporter (Logs_browser.console_reporter ());
  Logs.set_level (Some Logs.Debug);

  let open Js_of_ocaml in
  Js.export_all
    (object%js
       method parse program_str =
         (let ( let* ) x f = Result.bind x ~f in
          let* prog = parse_program_str program_str in
          Ok prog)
         |> function
         | Ok prog ->
             let json_str =
               prog |> Syntax.yojson_of_tree |> Yojson.Safe.to_string
             in
             Js.Unsafe.global##._JSON##parse json_str
         | Error err ->
             Js.Unsafe.obj [| ("error", err |> Js.string |> Js.Unsafe.inject) |]

       method synthesize tree_src steps =
         (* Use JSON-encoded string of demo_step list *)
         Logs.debug (fun m -> m "Steps: %s" steps);

         let steps =
           steps |> Yojson.Safe.from_string
           |> Ppx_yojson_conv_lib.Yojson_conv.Primitives.(
                [%of_yojson: Demo.demo_step list list])
         in
         Logs.info (fun m ->
             m "Steps to synthesize: %s"
               ([%show: Demo.demo_step list list] steps));

         synthesize tree_src steps |> function
         | Ok js_code ->
             Js.Unsafe.obj
               [| ("code", js_code |> Js.string |> Js.Unsafe.inject) |]
         | Error err ->
             Js.Unsafe.obj [| ("error", err |> Js.string |> Js.Unsafe.inject) |]
    end)
