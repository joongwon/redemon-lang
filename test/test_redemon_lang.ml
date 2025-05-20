open Alcotest
open Redemon_lang.Tree.Syntax

let test_parse_tree (name, input, expected) =
  let open Redemon_lang.Tree in
  let result = Parser.document Lexer.token (Lexing.from_string input) in
  test_case name `Quick (fun () ->
      check (testable Syntax.pp_tree Syntax.equal_tree) name expected result)

let parse_tree_testcases =
  [
    ("text", "hello=\">/", tree_const (String "hello=\">/"));
    ("elem", "<hello></hello>", tree_elem "hello" [] []);
    ("self_closing_elem", "<hello/>", tree_elem "hello" [] []);
    ( "elem_with_text",
      "<hello>world</hello>",
      tree_elem "hello" [] [ tree_const (String "world") ] );
    ( "elem_with_props",
      "<hello a=\"b\" c={1} d={true} e={false} f={$}></hello>",
      tree_elem "hello"
        [
          ("a", AttrConst (String "b"));
          ("c", AttrConst (Int 1));
          ("d", AttrConst (Bool true));
          ("e", AttrConst (Bool false));
          ("f", AttrFunc);
        ]
        [] );
  ]

let () =
  run "Redemon Lang Tests"
    [ ("Parse Tree", List.map test_parse_tree parse_tree_testcases) ]
