open! Base
open Redemon_lang.Tree

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

let () =
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
    end)
