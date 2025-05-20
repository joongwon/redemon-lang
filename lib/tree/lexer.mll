{
open Parser
open Syntax
}

rule token = parse
  | "<" (['a'-'z' '0'-'9']+ as tagname) { props tagname [] lexbuf }
  | "</" (['a'-'z' '0'-'9']+ as tagname) ">" { CLOSE tagname }
  | ['\n'] [' ' '\t']* { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']* (([^'{' '<' '\n']*[^'{' '<' ' ' '\t' '\n']) as text) { TEXT text }
  | "{\"" ([^'"' '\n']+ as value) "\"}" { CONST (String value) }
  | "{" (['0'-'9']+ as value) "}" { CONST (Int (int_of_string value)) }
  | "{true}" { CONST (Bool true) }
  | "{false}" { CONST (Bool false) }
  | eof { EOF }

and props tagname acc = parse
  | [' ' '\t'] { props tagname acc lexbuf }
  | ">" { OPEN (tagname, List.rev acc) }
  | "/>" { SELF (tagname, List.rev acc) }
  | (['a'-'z' '0'-'9']+ as name) "=\"" ([^'"' '\n']+ as value) '"' { props tagname ((name, AttrConst (String value)) :: acc) lexbuf }
  | (['a'-'z' '0'-'9']+ as name) "={" (['0'-'9']+ as value) "}" { props tagname ((name, AttrConst (Int (int_of_string value))) :: acc) lexbuf }
  | (['a'-'z' '0'-'9']+ as name) "={true}" { props tagname ((name, AttrConst (Bool true)) :: acc) lexbuf }
  | (['a'-'z' '0'-'9']+ as name) "={false}" { props tagname ((name, AttrConst (Bool false)) :: acc) lexbuf }
  | (['a'-'z' '0'-'9']+ as name) "={$}" { props tagname ((name, AttrFunc) :: acc) lexbuf }
