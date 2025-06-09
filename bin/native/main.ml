open Redemon_lang
open Redemon_lang.Tree.Syntax

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
            tree_elem "span"
              [ ("className", AttrConst (String "font-semibold text-lg")) ]
              [ tree_const (Int 1) ];
            tree_elem "button"
              [
                ( "className",
                  AttrConst (String "bg-stone-500 text-white px-2 py-1 rounded")
                );
                ("onClick", AttrFunc (Label 1));
              ]
              [ tree_const (String "Increment") ];
          ];
      timelines =
        [
          {
            action = { label = Label 1; action_type = Click; arg = None };
            edits =
              [
                ([ Index 0; Index 0 ], ConstReplace (Int 1));
                ([ Index 1; Index 0 ], ConstReplace (Int 2));
              ];
          };
          {
            action = { label = Label 1; action_type = Click; arg = None };
            edits =
              [
                ([ Index 0; Index 0 ], ConstReplace (Int 2));
                ([ Index 1; Index 0 ], ConstReplace (Int 4));
              ];
          };
        ];
    }

let () =
  let abs = Abstract.abstract_demo counter_demo in
  let result =
    Synthesis.synthesize abs |> Synthesis.translate_synthesized_rules
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
  Codegen.show_prog prog |> print_endline;
  Codegen.js_of_prog prog |> print_endline
