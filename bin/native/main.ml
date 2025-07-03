open Redemon_lang
open Redemon_lang.Tree.Syntax
open Lwt.Infix

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
        ];
    }

let synthesis_test () =
  let open Synthesis in
  let open Tree.Syntax in
  let open Texpr in
  let open Demo in
  let open Abstract in
  let synthesize_with_llm =
    synthesize_with_llm (module Redemon_with_llm_unix.Api)
  in
  let counter_abstraction =
    {
      sketch = expr_of_tree (Const (Int 0));
      init = [ (Var 1, Const (Int 0)) ];
      timelines =
        [
          [
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 1)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 2)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 3)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 2)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 1)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 0)) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for Counter Example:\n";
  let rules = synthesize counter_abstraction in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules;
  Printf.printf "\n";
  Printf.printf "Synthesizing for Counter Example with llm:\n";
  synthesize_with_llm counter_abstraction >>= fun rules_llm ->
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules_llm;
  Printf.printf "\n";
  let counter_abstraction2 =
    {
      sketch = expr_of_tree (Const (Int 0));
      init = [ (Var 1, Const (Int 4)) ];
      timelines =
        [
          [
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 5)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 6)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 7)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 6)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 5)) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for Counter Example 2:\n";
  let rules = synthesize counter_abstraction2 in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules;
  Printf.printf "\n";

  let counter_abstraction3 =
    {
      sketch = expr_of_tree (Const (Int 0));
      init = [ (Var 1, Const (Int 4)) ];
      timelines =
        [
          [
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 8)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 16)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 32)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 16)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 8)) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for Counter Example 3:\n";
  let rules = synthesize counter_abstraction3 in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules;
  Printf.printf "\n";

  let counter_abstraction_timeline =
    {
      sketch = expr_of_tree (Const (Int 0));
      init = [ (Var 1, Const (Int 4)) ];
      timelines =
        [
          [
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 8)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 16)) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 1, Const (Int 32)) ] );
          ];
          [
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 2)) ] );
            ( { label = Label 2; action_type = Click; arg = None },
              [ (Var 1, Const (Int 1)) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for Counter Example with timelines:\n";
  let rules = synthesize counter_abstraction_timeline in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules;
  Printf.printf "\n";

  let string_input_abstraction =
    {
      sketch = expr_of_tree (Const (String "initial"));
      init = [ (Var 10, Const (String "initial")) ];
      timelines =
        [
          [
            ( { label = Label 5; action_type = Input; arg = Some "world" },
              [ (Var 10, Const (String "world")) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 10, Const (String "hello")) ] );
            ( { label = Label 5; action_type = Input; arg = Some "changed" },
              [ (Var 10, Const (String "changed")) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for String Input Example:\n";
  let rules_str = synthesize string_input_abstraction in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules_str;
  Printf.printf "\n";

  let string_concat_abstraction =
    {
      sketch = expr_of_tree (Const (String "initial"));
      init = [ (Var 10, Const (String "initial")) ];
      timelines =
        [
          [
            ( { label = Label 5; action_type = Input; arg = Some "world" },
              [ (Var 10, Const (String "initial world")) ] );
            ( { label = Label 1; action_type = Click; arg = None },
              [ (Var 10, Const (String "hello")) ] );
            ( { label = Label 5; action_type = Input; arg = Some "changed" },
              [ (Var 10, Const (String "hello changed")) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for String concat Input Example:\n";
  let rules_str = synthesize string_concat_abstraction in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules_str;
  Printf.printf "\n";

  let list_push_pop_abstraction =
    {
      sketch = expr_of_tree (Const (Int 0));
      init = [ (Var 20, List []) ];
      timelines =
        [
          [
            ( { label = Label 100; action_type = Click; arg = None },
              [ (Var 20, List []) ] );
            ( { label = Label 50; action_type = Click; arg = None },
              [ (Var 20, List [ Record [ (Var 1, Const (Int 1)) ] ]) ] );
          ];
        ];
    }
  in
  Printf.printf "Synthesizing for List Push/Pop Example:\n";
  let rules_list = synthesize list_push_pop_abstraction in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules_list;
  Lwt.return_unit

let () =
  Lwt_main.run (synthesis_test ());

  let abs = Abstract.abstract_demo_multi counter_demo in
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
