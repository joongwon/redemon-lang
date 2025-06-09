open Alcotest
open Redemon_lang.Tree.Syntax
open Redemon_lang.Tree
open Redemon_lang
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type demo_step_list = Demo.demo_step list [@@deriving yojson]

let test_demo (name, input, expected) =
  test_case name `Quick (fun () ->
      let result = input |> yojson_of_demo_step_list |> Yojson.Safe.to_string in
      check string name expected result)

let demo_testcases =
  let open Demo in
  [
    ( "increment",
      [
        {
          action = { label = Label 0; action_type = Click; arg = None };
          edits = [ ([ Index 0; Index 0 ], ConstReplace (String "1")) ];
        };
      ],
      {|[{"action":{"label":["Label",0],"action_type":["Click"]},"edits":[[[["Index",0],["Index",0]],["ConstReplace",["String","1"]]]]}]|}
    );
  ]

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
          ("f", AttrFunc (Label 1));
        ]
        [] );
    ( "counter example",
      {|<div className="flex flex-col items-center">
  <div className="font-semibold text-lg">
    0
  </div>
  <button
    className="border-none bg-stone-500 text-white px-2 py-1 rounded"
    onClick={$1}
  >
    Increment
  </button>
</div>|},
      tree_elem "div"
        [ ("className", AttrConst (String "flex flex-col items-center")) ]
        [
          tree_elem "div"
            [ ("className", AttrConst (String "font-semibold text-lg")) ]
            [ tree_const (String "0") ];
          tree_elem "button"
            [
              ( "className",
                AttrConst
                  (String
                     "border-none bg-stone-500 text-white px-2 py-1 rounded") );
              ("onClick", AttrFunc (Label 1));
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
    ("basic", tree_const (String "foo"), (Const (String "foo") : Texpr.expr));
    ( "counter",
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
              ("onClick", AttrFunc (Label 1));
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
                        ("onClick", HandlerHole (Label 1));
                      ];
                    children = List [ Const (String "Increment") ];
                  };
              ];
        } );
  ]

let test_abstract_demo_single (name, input, expected) =
  test_case name `Quick (fun () ->
      let result =
        Abstract.abstract_demo_multi input
        |> Abstract.multi_to_singles |> List.hd
      in
      check
        (testable Abstract.pp_abstraction Abstract.equal_abstraction)
        name expected result)

let counter_demo =
  Demo.
    {
      init =
        tree_elem "div"
          [ ("className", AttrConst (String "flex flex-col items-center")) ]
          [
            tree_elem "span"
              [ ("className", AttrConst (String "font-semibold text-lg")) ]
              [ tree_const (Int 0) ];
            tree_elem "button"
              [
                ( "className",
                  AttrConst (String "bg-stone-500 text-white px-2 py-1 rounded")
                );
                ("onClick", AttrFunc (Label 1));
              ]
              [ tree_const (String "Increment") ];
            tree_elem "button"
              [
                ( "className",
                  AttrConst (String "bg-stone-500 text-white px-2 py-1 rounded")
                );
                ("onClick", AttrFunc (Label 2));
              ]
              [ tree_const (String "Decrement") ];
          ];
      timelines =
        [
          [
            {
              action = { label = Label 1; action_type = Demo.Click; arg = None };
              edits = [ ([ Index 0; Index 0 ], Demo.ConstReplace (Int 1)) ];
            };
            {
              action = { label = Label 1; action_type = Demo.Click; arg = None };
              edits = [ ([ Index 0; Index 0 ], Demo.ConstReplace (Int 2)) ];
            };
            {
              action = { label = Label 2; action_type = Demo.Click; arg = None };
              edits = [ ([ Index 0; Index 0 ], Demo.ConstReplace (Int 1)) ];
            };
          ];
        ];
    }

let abstract_demo_single_testcases =
  let open Demo in
  [
    ( "counter demo",
      counter_demo,
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
                          children = List [ Access (Var 1) ];
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
                              ("onClick", HandlerHole (Label 1));
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
                              ("onClick", HandlerHole (Label 2));
                            ];
                          children = List [ Const (String "Decrement") ];
                        };
                    ];
              };
          init = [ (Var 1, Const (Int 0)) ];
          steps =
            [
              ( { label = Label 1; action_type = Click; arg = None },
                [ (Var 1, Const (Int 1)) ] );
              ( { label = Label 1; action_type = Click; arg = None },
                [ (Var 1, Const (Int 2)) ] );
              ( { label = Label 2; action_type = Click; arg = None },
                [ (Var 1, Const (Int 1)) ] );
            ];
        } );
    ( "todo list demo",
      {
        init =
          tree_elem "div" []
            [
              tree_elem "input"
                [
                  ("onChange", AttrFunc (Label 1));
                  ("value", AttrConst (String ""));
                ]
                [];
              tree_elem "button"
                [ ("onClick", AttrFunc (Label 2)) ]
                [ tree_const (String "Add Task") ];
              tree_elem "ul" [] [];
            ];
        timelines =
          [
            [
              {
                action =
                  { label = Label 1; action_type = Input; arg = Some "Task 1" };
                edits =
                  [
                    ( [ Index 0 ],
                      AttributeReplace ("value", Some (String "Task 1")) );
                  ];
              };
              {
                action = { label = Label 2; action_type = Click; arg = None };
                edits =
                  [
                    ( [ Index 2 ],
                      NodeInsert
                        ( Index 0,
                          tree_elem "li" [] [ tree_const (String "Task 1") ] )
                    );
                    ([ Index 0 ], AttributeReplace ("value", Some (String "")));
                  ];
              };
              {
                action =
                  {
                    label = Label 1;
                    action_type = Input;
                    arg = Some "New Task";
                  };
                edits =
                  [
                    ( [ Index 0 ],
                      AttributeReplace ("value", Some (String "New Task")) );
                  ];
              };
              {
                action = { label = Label 2; action_type = Click; arg = None };
                edits =
                  [
                    ([ Index 2 ], NodeCopy (Index 0));
                    ( [ Index 2; Index 1; Index 0 ],
                      ConstReplace (String "New Task") );
                    ([ Index 0 ], AttributeReplace ("value", Some (String "")));
                  ];
              };
            ];
          ];
      },
      Abstract.
        {
          sketch =
            Elem
              {
                name = "div";
                attrs = [];
                children =
                  List
                    [
                      Elem
                        {
                          name = "input";
                          attrs =
                            [
                              ("value", Access (Var 1));
                              ("onChange", HandlerHole (Label 1));
                            ];
                          children = List [];
                        };
                      Elem
                        {
                          name = "button";
                          attrs = [ ("onClick", HandlerHole (Label 2)) ];
                          children = List [ Const (String "Add Task") ];
                        };
                      Elem
                        {
                          name = "ul";
                          attrs = [];
                          children =
                            ListMap
                              {
                                lst = Var 2;
                                body =
                                  Elem
                                    {
                                      name = "li";
                                      attrs = [];
                                      children = List [ Access (Var 1) ];
                                    };
                              };
                        };
                    ];
              };
          init = [ (Var 2, List []); (Var 1, Const (String "")) ];
          steps =
            [
              ( { label = Label 1; action_type = Input; arg = Some "Task 1" },
                [ (Var 2, List []); (Var 1, Const (String "Task 1")) ] );
              ( { label = Label 2; action_type = Click; arg = None },
                [
                  (Var 2, List [ Record [ (Var 1, Const (String "Task 1")) ] ]);
                  (Var 1, Const (String ""));
                ] );
              ( { label = Label 1; action_type = Input; arg = Some "New Task" },
                [
                  (Var 2, List [ Record [ (Var 1, Const (String "Task 1")) ] ]);
                  (Var 1, Const (String "New Task"));
                ] );
              ( { label = Label 2; action_type = Click; arg = None },
                [
                  (Var 1, Const (String ""));
                  ( Var 2,
                    List
                      [
                        Record [ (Var 1, Const (String "Task 1")) ];
                        Record [ (Var 1, Const (String "New Task")) ];
                      ] );
                ] );
            ];
        } );
  ]

let test_abstract_demo_multi (name, input, expected) =
  test_case name `Quick (fun () ->
      let result = Abstract.abstract_demo_multi input in
      check
        (testable Abstract.pp_abstraction_multi Abstract.equal_abstraction_multi)
        name expected result)

let abstract_demo_multi_testcases =
  let open Demo in
  [
    ( "counter demo",
      Demo.
        {
          init =
            tree_elem "div" []
              [
                tree_elem "span" [] [ tree_const (Int 0) ];
                tree_elem "span" [] [ tree_const (Int 0) ];
                tree_elem "button"
                  [ ("onClick", AttrFunc (Label 1)) ]
                  [ tree_const (String "Increment 1") ];
                tree_elem "button"
                  [ ("onClick", AttrFunc (Label 2)) ]
                  [ tree_const (String "Increment 2") ];
              ];
          timelines =
            [
              [
                {
                  action = { label = Label 1; action_type = Click; arg = None };
                  edits = [ ([ Index 0; Index 0 ], ConstReplace (Int 1)) ];
                };
              ];
              [
                {
                  action = { label = Label 2; action_type = Click; arg = None };
                  edits = [ ([ Index 1; Index 0 ], ConstReplace (Int 2)) ];
                };
              ];
            ];
        },
      Abstract.
        {
          sketch =
            Elem
              {
                name = "div";
                attrs = [];
                children =
                  List
                    [
                      Elem
                        {
                          name = "span";
                          attrs = [];
                          children = List [ Access (Var 1) ];
                        };
                      Elem
                        {
                          name = "span";
                          attrs = [];
                          children = List [ Access (Var 2) ];
                        };
                      Elem
                        {
                          name = "button";
                          attrs = [ ("onClick", HandlerHole (Label 1)) ];
                          children = List [ Const (String "Increment 1") ];
                        };
                      Elem
                        {
                          name = "button";
                          attrs = [ ("onClick", HandlerHole (Label 2)) ];
                          children = List [ Const (String "Increment 2") ];
                        };
                    ];
              };
          init = [ (Var 2, Const (Int 0)); (Var 1, Const (Int 0)) ];
          timelines =
            [
              [
                ( { label = Label 1; action_type = Click; arg = None },
                  [ (Var 2, Const (Int 0)); (Var 1, Const (Int 1)) ] );
              ];
              [
                ( { label = Label 2; action_type = Click; arg = None },
                  [ (Var 2, Const (Int 2)); (Var 1, Const (Int 0)) ] );
              ];
            ];
        } );
  ]

let test_synthesis_1 () =
  let abs = Abstract.abstract_demo_multi counter_demo in
  let result =
    (* abs |> Abstract.multi_to_singles |> List.hd |> Synthesis.synthesize *)
    abs |> Synthesis.synthesize |> Synthesis.translate_synthesized_rules
  in
  let prog =
    Codegen.
      {
        view = abs.sketch;
        data = Record (List.map (fun (v, _) -> (v, Texpr.Access v)) abs.init);
        handlers = result;
        states = abs.init;
      }
  in
  check
    (testable Codegen.pp_prog Codegen.equal_prog)
    "Synthesis Test"
    {
      view = abs.sketch;
      data = Record (List.map (fun (v, _) -> (v, Texpr.Access v)) abs.init);
      handlers =
        [
          {
            label = Label 2;
            action_type = Click;
            state = Var 1;
            func =
              Fun { func = "divide"; args = [ Access (Var 1); Const (Int 2) ] };
          };
          {
            label = Label 1;
            action_type = Click;
            state = Var 1;
            func =
              Fun { func = "plus"; args = [ Access (Var 1); Const (Int 1) ] };
          };
        ];
      states = abs.init;
    }
    prog

let test_synthesis_2 () =
  let abs =
    Abstract.abstract_demo_multi
      Demo.
        {
          init =
            tree_elem "div"
              [ ("className", AttrConst (String "flex flex-col items-center")) ]
              [
                tree_elem "span"
                  [ ("className", AttrConst (String "font-semibold text-lg")) ]
                  [ tree_const (Int 0) ];
                tree_elem "button"
                  [
                    ( "className",
                      AttrConst
                        (String "bg-stone-500 text-white px-2 py-1 rounded") );
                    ("onClick", AttrFunc (Label 1));
                  ]
                  [ tree_const (String "Increment") ];
                tree_elem "button"
                  [
                    ( "className",
                      AttrConst
                        (String "bg-stone-500 text-white px-2 py-1 rounded") );
                    ("onClick", AttrFunc (Label 2));
                  ]
                  [ tree_const (String "Decrement") ];
              ];
          timelines = [ [] ];
        }
  in
  let result =
    (* abs |> Abstract.multi_to_singles |> List.hd |> Synthesis.synthesize *)
    abs |> Synthesis.synthesize |> Synthesis.translate_synthesized_rules
  in
  let prog =
    Codegen.
      {
        view = abs.sketch;
        data = Record (List.map (fun (v, _) -> (v, Texpr.Access v)) abs.init);
        handlers = result;
        states = abs.init;
      }
  in
  check
    (testable Codegen.pp_prog Codegen.equal_prog)
    "Synthesis Test"
    {
      view = abs.sketch;
      data = Record (List.map (fun (v, _) -> (v, Texpr.Access v)) abs.init);
      handlers = [];
      states = abs.init;
    }
    prog

let () =
  run "Redemon Lang Tests"
    [
      ("Parse demo", List.map test_demo demo_testcases);
      ("Parse Tree", List.map test_parse_tree parse_tree_testcases);
      ( "Init Abstraction",
        List.map test_init_abstraction init_abstraction_testcases );
      ( "Abstract Demo Single",
        List.map test_abstract_demo_single abstract_demo_single_testcases );
      ( "Abstract Demo Multi",
        List.map test_abstract_demo_multi abstract_demo_multi_testcases );
      ( "Synthesis",
        [
          test_case "Synthesize Counter Demo" `Quick test_synthesis_1;
          test_case "Synthesize Empty Demo" `Quick test_synthesis_2;
        ] );
    ]
