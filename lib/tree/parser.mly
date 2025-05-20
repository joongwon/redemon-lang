%{
open Syntax
exception Mismatched_tags
%}

%token <string * (string * Syntax.attr_value) list> OPEN SELF
%token <string> CLOSE
%token <string> TEXT
%token <Syntax.const> CONST
%token EOF

%start <Syntax.tree> document

%%

document:
  | n = node EOF { n }

node:
  | n = SELF { tree_elem (fst n) (snd n) [] }
  | n1 = OPEN; l = list(node); n2 = CLOSE { if fst n1 = n2 then tree_elem (fst n1) (snd n1) l else raise Mismatched_tags }
  | t = TEXT { tree_const (String t) }
