open Redemon_lang
open Redemon_lang.Tree.Syntax

let counter_demo = Demo.
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
          steps =
            [
              {
                action =
                  { label = Label 1; action_type = Demo.Click; arg = None };
                edits = [ ([ Index 0; Index 0 ], Demo.Replace (Int 1)) ];
              };
              {
                action =
                  { label = Label 1; action_type = Demo.Click; arg = None };
                edits = [ ([ Index 0; Index 0 ], Demo.Replace (Int 2)) ];
              };
              {
                action =
                  { label = Label 2; action_type = Demo.Click; arg = None };
                edits = [ ([ Index 0; Index 0 ], Demo.Replace (Int 1)) ];
              };
            ];
        }

let pairs_to_assoc_list pairs =
  List.fold_left (fun acc (k, v) ->
    match List.assoc_opt k acc with
    | Some existing_v -> (k, existing_v @ [v]) :: List.remove_assoc k acc
    | None -> (k, [v]) :: acc) [] pairs

let () =
  let abs = Abstract.abstract_demo counter_demo in
  let result = Synthesis.synthesize abs |> Synthesis.translate_synthesized_rules |>
    List.map (fun Synthesis.{ state; label; action_type; func } ->
      ((label, action_type), (state, func))) 
    |> pairs_to_assoc_list in
  let prog = Codegen.{
    view = abs.sketch;
    data = Record (List.map (fun (v, _) -> (v, Texpr.Access v)) abs.init);
    handlers = result;
    states = abs.init;
  } in
  Codegen.show_prog prog |> print_endline;
  Codegen.js_of_prog prog |> print_endline
