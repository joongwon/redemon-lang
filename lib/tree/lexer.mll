{
open Parser
open Syntax

exception LexingError of string * Lexing.position
}

let word = ['a'-'z' 'A'-'Z' '0'-'9']+

rule token = parse
  | "<" (['a'-'z' '0'-'9']+ as tagname) { props tagname [] lexbuf }
  | "</" (['a'-'z' '0'-'9']+ as tagname) ">" { CLOSE tagname }
  | ['\n'] [' ' '\t']* { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']* (([^'{' '<' '\n']*[^'{' '<' ' ' '\t' '\n']) as text) { TEXT text }
  | "{\"" ([^'"' '\n']+ as value) "\"}" { CONST (String value) }
  | "{" (['0'-'9']+ as value) "}" { CONST (Int (int_of_string value)) }
  (*
  | "{true}" { CONST (Bool true) }
  | "{false}" { CONST (Bool false) }
  *)
  | eof { EOF }
  | _ { raise (LexingError (Printf.sprintf "Unexpected character: %c" (Lexing.lexeme_char lexbuf 0), Lexing.lexeme_start_p lexbuf)) }

and props tagname acc = parse
  | ['\n'] { Lexing.new_line lexbuf; props tagname acc lexbuf }
  | [' ' '\t'] { props tagname acc lexbuf }
  | ">" { OPEN (tagname, List.rev acc) }
  | "/>" { SELF (tagname, List.rev acc) }
  | (word as name) "=\"" ([^'"' '\n']+ as value) '"' { props tagname ((name, AttrConst (String value)) :: acc) lexbuf }
  | (word as name) "={" (['0'-'9']+ as value) "}" { props tagname ((name, AttrConst (Int (int_of_string value))) :: acc) lexbuf }
  (*
  | (word as name) "={true}" { props tagname ((name, AttrConst (Bool true)) :: acc) lexbuf }
  | (word as name) "={false}" { props tagname ((name, AttrConst (Bool false)) :: acc) lexbuf }
  *)
  | (word as name) "={$" (['0'-'9']+ as label) "}" { props tagname ((name, AttrFunc (Label (int_of_string label))) :: acc) lexbuf }
  | _ { raise (LexingError (Printf.sprintf "Unexpected character in attribute: %c" (Lexing.lexeme_char lexbuf 0), Lexing.lexeme_start_p lexbuf)) }
