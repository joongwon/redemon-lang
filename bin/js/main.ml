open! Base
open Redemon_lang.Tree
open Redemon_lang
open Lwt.Infix

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

(* In js/main.ml *)
let synthesize (tree_src : string) (timelines : Demo.demo_timeline list) :
    (string, string) Result.t Lwt.t =
  (* 반환 타입이 Lwt.t를 포함하도록 변경 *)

  (* Result 모나드 처리 부분 *)
  let res =
    let ( let* ) x f = Result.bind x ~f in
    let* tree = parse_program_str tree_src in
    let demo = Demo.{ init = tree; timelines } in
    let abs = Abstract.abstract_demo_multi demo in
    Ok abs
  in

  match res with
  | Error e -> Lwt.return (Error e)
  | Ok abs ->
      let synthesize_with_llm =
        Synthesis.Llm_backend.synthesize (module Redemon_with_llm_js.Api)
      in
      synthesize_with_llm abs >>= fun rules_ht ->
      let result = Synthesis.translate_synthesized_rules rules_ht in

      let prog =
        Codegen.
          {
            view = abs.sketch;
            data =
              Record (List.map ~f:(fun (v, _) -> (v, Texpr.Access v)) abs.init);
            handlers = result;
            states = abs.init;
          }
      in
      Lwt.return_ok (Codegen.js_of_prog prog)

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
         Logs.debug (fun m -> m "Steps: %s" steps);

         let timelines =
           steps |> Yojson.Safe.from_string
           |> Ppx_yojson_conv_lib.Yojson_conv.Primitives.(
                [%of_yojson: Demo.demo_timeline list])
         in
         Logs.info (fun m ->
             m "Steps to synthesize: %s"
               ([%show: Demo.demo_timeline list] timelines));

         let result_lwt = synthesize tree_src timelines in

         Promise_lwt.to_promise
           ( result_lwt >>= function
             | Ok js_code ->
                 let js_obj =
                   Js.Unsafe.obj
                     [| ("code", js_code |> Js.string |> Js.Unsafe.inject) |]
                 in
                 Lwt.return js_obj
             | Error err ->
                 let js_obj =
                   Js.Unsafe.obj
                     [| ("error", err |> Js.string |> Js.Unsafe.inject) |]
                 in
                 Lwt.return js_obj )
    end)
