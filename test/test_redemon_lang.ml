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
      let result = Abstract.texpr_of_tree input in
      check (testable Texpr.pp_texpr Texpr.equal_texpr) name expected result)

let init_abstraction_testcases =
  [
    ("text", tree_const (String "foo"), Texpr.Val (Const (String "foo")));
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
      Texpr.Elem
        {
          name = "div";
          attrs =
            [
              ( "className",
                AttrConst (Const (String "flex flex-col items-center")) );
            ];
          children =
            Texpr.Fixed
              [
                Texpr.Elem
                  {
                    name = "span";
                    attrs =
                      [
                        ( "className",
                          AttrConst (Const (String "font-semibold text-lg")) );
                      ];
                    children = Texpr.Fixed [ Texpr.Val (Const (String "0")) ];
                  };
                Texpr.Elem
                  {
                    name = "button";
                    attrs =
                      [
                        ( "className",
                          AttrConst
                            (Const
                               (String
                                  "bg-stone-500 text-white px-2 py-1 rounded"))
                        );
                        ("onClick", AttrFunc 1);
                      ];
                    children =
                      Texpr.Fixed [ Texpr.Val (Const (String "Increment")) ];
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
              (Demo.Click 1, [ ([ 0; 0 ], Demo.Replace (String "1")) ]);
              (Demo.Click 1, [ ([ 0; 0 ], Demo.Replace (String "2")) ]);
              (Demo.Click 2, [ ([ 0; 0 ], Demo.Replace (String "1")) ]);
            ];
        },
      Abstract.
        {
          sketch =
            Texpr.Elem
              {
                name = "div";
                attrs =
                  [
                    ( "className",
                      AttrConst (Const (String "flex flex-col items-center")) );
                  ];
                children =
                  Texpr.Fixed
                    [
                      Texpr.Elem
                        {
                          name = "span";
                          attrs =
                            [
                              ( "className",
                                AttrConst
                                  (Const (String "font-semibold text-lg")) );
                            ];
                          children = Texpr.Fixed [ Texpr.Val (Var 0) ];
                        };
                      Texpr.Elem
                        {
                          name = "button";
                          attrs =
                            [
                              ( "className",
                                AttrConst
                                  (Const
                                     (String
                                        "bg-stone-500 text-white px-2 py-1 \
                                         rounded")) );
                              ("onClick", AttrFunc 1);
                            ];
                          children =
                            Texpr.Fixed
                              [ Texpr.Val (Const (String "Increment")) ];
                        };
                      Texpr.Elem
                        {
                          name = "button";
                          attrs =
                            [
                              ( "className",
                                AttrConst
                                  (Const
                                     (String
                                        "bg-stone-500 text-white px-2 py-1 \
                                         rounded")) );
                              ("onClick", AttrFunc 2);
                            ];
                          children =
                            Texpr.Fixed
                              [ Texpr.Val (Const (String "Decrement")) ];
                        };
                    ];
              };
          init = [ (0, Texpr.Const (String "0")) ];
          steps =
            [
              (Demo.Click 1, [ (0, Texpr.Const (String "1")) ]);
              (Demo.Click 1, [ (0, Texpr.Const (String "2")) ]);
              (Demo.Click 2, [ (0, Texpr.Const (String "1")) ]);
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
