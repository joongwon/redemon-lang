open Alcotest
open Redemon_lang.Tree.Syntax
open Redemon_lang.Tree
open Redemon_lang

let test_parse_tree (name, input, expected) =
  test_case name `Quick (fun () ->
      try
        let result = Parser.document Lexer.token (Lexing.from_string input) in
        check (testable Syntax.pp_tree Syntax.equal_tree) name expected result
      with Lexer.LexingError (msg, pos) ->
        let line_no = pos.pos_lnum in
        let col_no = pos.pos_cnum - pos.pos_bol + 1 in
        Alcotest.failf "Lexer error at line %d, column %d: %s" line_no col_no
          msg)

let parse_tree_testcases =
  [
    ("text", "hello=\">/", tree_const (String "hello=\">/"));
    ("elem", "<hello></hello>", tree_elem "hello" [] []);
    ("self_closing_elem", "<hello/>", tree_elem "hello" [] []);
    ( "elem_with_text",
      "<hello>world</hello>",
      tree_elem "hello" [] [ tree_const (String "world") ] );
    ( "elem_with_props",
      "<hello a=\"b\" c={1} f={$1}></hello>",
      tree_elem "hello"
        [
          ("a", AttrConst (String "b"));
          ("c", AttrConst (Int 1));
          ("f", AttrFunc 1);
        ]
        [] );
    ( "counter example",
      {|<div className="flex flex-col items-center">
  <span className="font-semibold text-lg">
    0
  </span>
  <button
    className="bg-stone-500 text-white px-2 py-1 rounded"
    onClick={$1}
  >
    Increment
  </button>
  </div>|},
      tree_elem "div"
        [ ("className", AttrConst (String "flex flex-col items-center")) ]
        [
          tree_elem "span"
            [ ("className", AttrConst (String "font-semibold text-lg")) ]
            [ tree_const (String "0") ];
          tree_elem "button"
            [
              ( "className",
                AttrConst (String "bg-stone-500 text-white px-2 py-1 rounded")
              );
              ("onClick", AttrFunc 1);
            ]
            [ tree_const (String "Increment") ];
        ] );
    ( "number_as_children",
      "<hello>{1}</hello>",
      tree_elem "hello" [] [ tree_const (Int 1) ] );
  ]

let test_init_abstraction (name, input, expected) =
  test_case name `Quick (fun () ->
      let result = Abstract.expr_of_tree input in
      check (testable Texpr.pp_expr Texpr.equal_expr) name expected result)

let init_abstraction_testcases =
  [
    ("text", tree_const (String "foo"), (Const (String "foo") : Texpr.expr));
    ( "counter example",
      tree_elem "div"
        [ ("className", AttrConst (String "flex flex-col items-center")) ]
        [
          tree_elem "span"
            [ ("className", AttrConst (String "font-semibold text-lg")) ]
            [ tree_const (String "0") ];
          tree_elem "button"
            [
              ( "className",
                AttrConst (String "bg-stone-500 text-white px-2 py-1 rounded")
              );
              ("onClick", AttrFunc 1);
            ]
            [ tree_const (String "Increment") ];
        ],
      Elem
        {
          name = "div";
          attrs = [ ("className", Const (String "flex flex-col items-center")) ];
          children =
            List
              [
                Elem
                  {
                    name = "span";
                    attrs =
                      [ ("className", Const (String "font-semibold text-lg")) ];
                    children = List [ Const (String "0") ];
                  };
                Elem
                  {
                    name = "button";
                    attrs =
                      [
                        ( "className",
                          Const
                            (String "bg-stone-500 text-white px-2 py-1 rounded")
                        );
                        ("onClick", HandlerHole 1);
                      ];
                    children = List [ Const (String "Increment") ];
                  };
              ];
        } );
  ]

let test_abstract_demo (name, input, expected) =
  test_case name `Quick (fun () ->
      let result = Abstract.abstract_demo input in
      check
        (testable Abstract.pp_abstraction Abstract.equal_abstraction)
        name expected result)

let abstract_demo_testcases =
  [
    ( "counter demo",
      Demo.
        {
          init =
            tree_elem "div"
              [ ("className", AttrConst (String "flex flex-col items-center")) ]
              [
                tree_elem "span"
                  [ ("className", AttrConst (String "font-semibold text-lg")) ]
                  [ tree_const (String "0") ];
                tree_elem "button"
                  [
                    ( "className",
                      AttrConst
                        (String "bg-stone-500 text-white px-2 py-1 rounded") );
                    ("onClick", AttrFunc 1);
                  ]
                  [ tree_const (String "Increment") ];
                tree_elem "button"
                  [
                    ( "className",
                      AttrConst
                        (String "bg-stone-500 text-white px-2 py-1 rounded") );
                    ("onClick", AttrFunc 2);
                  ]
                  [ tree_const (String "Decrement") ];
              ];
          steps =
            [
              ((1, Demo.Click, None), [ ([ 0; 0 ], Demo.Replace (String "1")) ]);
              ((1, Demo.Click, None), [ ([ 0; 0 ], Demo.Replace (String "2")) ]);
              ((2, Demo.Click, None), [ ([ 0; 0 ], Demo.Replace (String "1")) ]);
            ];
        },
      Abstract.
        {
          sketch =
            Elem
              {
                name = "div";
                attrs =
                  [ ("className", Const (String "flex flex-col items-center")) ];
                children =
                  List
                    [
                      Elem
                        {
                          name = "span";
                          attrs =
                            [
                              ( "className",
                                Const (String "font-semibold text-lg") );
                            ];
                          children = List [ Access 1 ];
                        };
                      Elem
                        {
                          name = "button";
                          attrs =
                            [
                              ( "className",
                                Const
                                  (String
                                     "bg-stone-500 text-white px-2 py-1 rounded")
                              );
                              ("onClick", HandlerHole 1);
                            ];
                          children = List [ Const (String "Increment") ];
                        };
                      Elem
                        {
                          name = "button";
                          attrs =
                            [
                              ( "className",
                                Const
                                  (String
                                     "bg-stone-500 text-white px-2 py-1 rounded")
                              );
                              ("onClick", HandlerHole 2);
                            ];
                          children = List [ Const (String "Decrement") ];
                        };
                    ];
              };
          init = [ (1, Const (String "0")) ];
          steps =
            [
              ((1, Demo.Click, None), [ (1, Const (String "1")) ]);
              ((1, Demo.Click, None), [ (1, Const (String "2")) ]);
              ((2, Demo.Click, None), [ (1, Const (String "1")) ]);
            ];
        } );
  ]

let () =
  run "Redemon Lang Tests"
    [
      ("Parse Tree", List.map test_parse_tree parse_tree_testcases);
      ( "Init Abstraction",
        List.map test_init_abstraction init_abstraction_testcases );
      ("Abstract Demo", List.map test_abstract_demo abstract_demo_testcases);
    ]
