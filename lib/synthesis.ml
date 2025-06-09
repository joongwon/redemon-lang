open Tree.Syntax
open Texpr
open Demo
open Abstract

(* list function:
  map, concat, length
  TODO: filter(find)
*)

exception TypeError of string
exception LengthError of string
exception NotFound of string
exception InvalidOperation of string
exception SynthesisConflict of string
exception SynthesisFailed of string
exception ParameterLengthError of string

(* Fuction candidate *)
let map (l : value) (f : value -> value) : value =
  match l with
  | List l -> List (List.map f l)
  | _ -> raise (TypeError "Map: Expected a list")

let concat_list (l1 : value) (l2 : value) : value =
  match (l1, l2) with
  | List l1, List l2 -> List (l1 @ l2)
  | _ -> raise (TypeError "Concat: Expected two lists")

let length (l : value) : value =
  match l with
  | List l -> Const (Int (List.length l))
  | _ -> raise (TypeError "Length: Expected a list")

let filter (l : value) (f : value -> bool) : value =
  match l with
  | List l -> List (List.filter f l)
  | _ -> raise (TypeError "Filter: Expected a list")

let find (l : value) (f : value -> bool) : value =
  match l with
  | List l ->
      let rec aux = function
        | [] -> raise (NotFound "Find: Not found")
        | x :: xs -> if f x then x else aux xs
      in
      aux l
  | _ -> raise (TypeError "Find: Expected a list")

(* record function *)

let push (v1 : value) (v2 : value) : value =
  match v1 with
  | List l -> List (v2 :: l)
  | _ -> raise (TypeError "Push: Expected a list and a record")

let pop (v1 : value) : value =
  match v1 with
  | List [] -> raise (LengthError "Pop: Cannot pop from an empty list")
  | List (r :: l) -> List l
  | _ -> raise (TypeError "Pop: Expected a list")

(* interger function *)

let plus (v1 : value) (v2 : value) : value =
  match (v1, v2) with
  | Const (Int i1), Const (Int i2) -> Const (Int (i1 + i2))
  | _ -> raise (TypeError "Plus: Expected two integers")

let minus (v1 : value) (v2 : value) : value =
  match (v1, v2) with
  | Const (Int i1), Const (Int i2) -> Const (Int (i1 - i2))
  | _ -> raise (TypeError "Minus: Expected two integers")

let times (v1 : value) (v2 : value) : value =
  match (v1, v2) with
  | Const (Int i1), Const (Int i2) -> Const (Int (i1 * i2))
  | _ -> raise (TypeError "Times: Expected two integers")

let divide (v1 : value) (v2 : value) : value =
  match (v1, v2) with
  | Const (Int i1), Const (Int i2) ->
      if i2 = 0 then raise (InvalidOperation "divide: divide with 0")
      else Const (Int (i1 / i2))
  | _ -> raise (TypeError "Divide: Expected two integers")

(* string function *)

let change_string_to (v_target_type : value) (s_new_val : string) : value =
  match v_target_type with
  | Const (String _) -> Const (String s_new_val)
  | _ -> raise (TypeError "ChangeStringTo: Expected a string constant")

(* 값을 특정 상수 문자열(구성 요소)로 설정하기 위함 *)
let set_to_const_string (_v_old : value) (v_new_string_const : value) : value =
  match v_new_string_const with
  | Const (String s) -> Const (String s)
  | _ -> raise (TypeError "SetToConstString: Expected a string constant")

let change_string (v1 : value) (s : string) : value =
  match v1 with
  | Const (String _) -> Const (String s)
  | _ -> raise (TypeError "ChangeString: Expected a string constant")

let concat_str (v1 : value) (v2 : value) : value =
  match (v1, v2) with
  | Const (String s1), Const (String s2) -> Const (String (s1 ^ s2))
  | _ -> raise (TypeError "ConcatStr: Expected two string constants")

(* Fuction candidate  end *)
let lookup_val (v_id : var) (r : record) : value =
  try List.assoc v_id r
  with Not_found ->
    raise
      (NotFound
         (Printf.sprintf "Variable %s not found in record" (show_var v_id)))

type parameterizable_action = P_Click of label | P_Input of label

let show_parameterizable_action (p_act : parameterizable_action) : string =
  match p_act with
  | P_Click l -> Printf.sprintf "Click(%s)" (show_label l)
  | P_Input l -> Printf.sprintf "Input(%s)" (show_label l)

let to_param_action (act : action) : parameterizable_action =
  match act with l, Click, _ -> P_Click l | l, Input, _ -> P_Input l

type synthesized_function = string * value list

let synthesize (abstraction_data : abstraction) :
    (var * parameterizable_action, synthesized_function) Hashtbl.t =
  let { init; steps } = abstraction_data in
  let steps_chronological = steps in
  (* 시간 순으로 스텝 정렬 *)

  (* 1. 모든 고유 변수 수집 *)
  let all_vars =
    List.sort_uniq compare
      (List.map fst init
      @ List.flatten
          (List.map (fun (_, r) -> List.map fst r) steps_chronological))
  in
  let all_vars = List.sort_uniq compare all_vars in
  (* 중복 제거 및 정렬 *)

  let components = ref [] in
  let add_const_to_components v =
    match v with
    | Const c -> components := Const c :: !components
    | Record r_val -> components := Record r_val :: !components
    | _ -> ()
  in
  List.iter (fun (_, v) -> add_const_to_components v) init;
  (* 초기 상태의 값들 *)
  List.iter
    (fun (_, r) -> List.iter (fun (_, v) -> add_const_to_components v) r)
    steps_chronological;
  (* 모든 스텝의 값들 *)
  let unique_components = List.sort_uniq compare !components in

  (* 중복 제거 및 정렬 *)
  (* Printf.printf "Unique components: %s\n" (String.concat "; " (List.map show_value unique_components)); *)

  (* 3. 관찰 결과 수집: (var_id, p_action, old_val, new_val, actual_action) *)
  (* 키: (var_id * p_action), 값: (old_val, new_val, action) 리스트 *)
  let observations = Hashtbl.create (List.length all_vars * 5) in

  let current_s = ref init in
  (* 현재 상태, 초기 상태로 시작 *)
  List.iter
    (fun (act, next_s) ->
      let prev_s = !current_s in
      let p_act = to_param_action act in
      (* 액션을 매개변수화된 액션으로 변환 *)
      List.iter
        (fun v_id ->
          try
            let old_val = lookup_val v_id prev_s in
            (* 이전 상태에서 변수 값 조회 *)
            let new_val = lookup_val v_id next_s in
            (* 다음 상태에서 변수 값 조회 *)
            if not (equal_value old_val new_val) then
              (* 값이 변경된 경우에만 *)
              let key = (v_id, p_act) in
              let existing_obs =
                try Hashtbl.find observations key with Not_found -> []
              in
              Hashtbl.replace observations key
                ((old_val, new_val, act) :: existing_obs)
            (* 관찰 결과 추가 *)
          with NotFound _ -> () (* 변수가 상태 중 하나에 없거나 추가/제거된 경우 무시 *))
        all_vars;
      current_s := next_s)
    steps_chronological;

  (* 4. 규칙 합성 *)
  let synthesized_rules = Hashtbl.create (Hashtbl.length observations) in

  Hashtbl.iter
    (fun (key : var * parameterizable_action)
         (transitions_rev : (value * value * action) list) ->
      let transitions = List.rev transitions_rev in
      (* 
    Printf.printf "Synthesizing for key: (%s, %s) with %d transitions\n"
      (show_var (fst key)) (show_parameterizable_action (snd key)) (List.length transitions);
    List.iter (fun (ov,nv,a) -> Printf.printf "  %s -> %s (action: %s)\n" (show_value ov) (show_value nv) (show_action a)) transitions;
    *)

      let v_id_checking, p_action_checking = key in

      (* 시도할 후보 연산 정의 *)
      (* (연산자 이름, (이전값 -> 새값 -> bool) 검사 함수, [인자 리스트] 옵션) *)
      let candidate_ops :
          (string * (value -> value -> bool) * value list option) list =
        (* 하나의 구성 요소 인자를 받는 연산들 *)
        List.fold_left
          (fun acc comp_arg ->
            ( "plus",
              (fun old new_val ->
                try equal_value (plus old comp_arg) new_val
                with TypeError _ | InvalidOperation _ -> false),
              Some [ comp_arg ] )
            :: ( "minus",
                 (fun old new_val ->
                   try equal_value (minus old comp_arg) new_val
                   with TypeError _ | InvalidOperation _ -> false),
                 Some [ comp_arg ] )
            :: ( "times",
                 (fun old new_val ->
                   try equal_value (times old comp_arg) new_val
                   with TypeError _ | InvalidOperation _ -> false),
                 Some [ comp_arg ] )
            :: ( "divide",
                 (fun old new_val ->
                   try equal_value (divide old comp_arg) new_val
                   with TypeError _ | InvalidOperation _ -> false),
                 Some [ comp_arg ] )
            :: ( "concat_str",
                 (fun old new_val ->
                   try equal_value (concat_str old comp_arg) new_val
                   with TypeError _ -> false),
                 Some [ comp_arg ] )
            :: (* change_string v1 s는 v1이 s가 됨. 즉, new_val = comp_arg *)
               ( "set_to_const_string",
                 (fun _old new_val ->
                   try equal_value (set_to_const_string _old comp_arg) new_val
                   with TypeError _ -> false),
                 Some [ comp_arg ] )
            :: ( "push",
                 (fun old new_val ->
                   try equal_value (push old comp_arg) new_val
                   with TypeError _ | LengthError _ -> false),
                 Some [ comp_arg ] )
            :: acc)
          [] unique_components
        @
        (* 구성 요소 인자를 받지 않는 연산들 *)
        [
          ( "pop",
            (fun old new_val ->
              try equal_value (pop old) new_val
              with TypeError _ | LengthError _ -> false),
            Some [] );
        ]
      in

      let found_rule = ref None in

      (if
         !found_rule = None
         && match p_action_checking with P_Input _ -> true | _ -> false
       then
         let all_match_input_set =
           List.for_all
             (fun (old_val, new_val, actual_act) ->
               match actual_act with
               | _, Input, Some input_str -> (
                   (* 실제 Input 액션에서 문자열 가져옴 *)
                   try
                     equal_value new_val (Const (String input_str))
                     (* 새 값이 입력 문자열과 같은지 *)
                     && equal_value
                          (change_string_to old_val input_str)
                          new_val (* 이전 값 타입에 입력 문자열을 설정한 결과가 새 값과 같은지 *)
                   with TypeError _ -> false)
               | _ -> false (* p_action_checking이 P_Input이면 발생하지 않아야 함 *))
             transitions
         in
         if all_match_input_set && transitions <> [] then
           (* Printf.printf "  SUCCESS with set_to_input_string for key (%s, %s)\n" (show_var v_id_checking) (show_parameterizable_action p_action_checking); *)
           found_rule := Some ("set_to_input_string", []));

      (* 다른 후보 연산 시도 *)
      List.iter
        (fun (op_name, op_check, op_args_opt) ->
          if !found_rule = None then
            (* 아직 규칙을 찾지 못한 경우에만 시도 *)
            let all_match_this_op =
              List.for_all
                (fun (old_val, new_val, _actual_act) ->
                  op_check old_val new_val (* 모든 전환에 대해 현재 연산이 성립하는지 확인 *))
                transitions
            in

            if all_match_this_op && transitions <> [] then
              (* 모든 전환에 대해 성립하고, 전환이 비어있지 않으면 *)
              match op_args_opt with
              | Some op_args ->
                  (* 이 연산의 (상수) 인자들 *)
                  (* Printf.printf "  SUCCESS with %s %s for key (%s, %s)\n" op_name (String.concat " " (List.map show_value op_args)) (show_var v_id_checking) (show_parameterizable_action p_action_checking); *)
                  found_rule := Some (op_name, op_args)
              | None ->
                  Printf.printf "  WARNING: op_args_opt was None for %s\n"
                    op_name)
        candidate_ops;

      match !found_rule with
      | Some (fname, fargs) ->
          Hashtbl.add synthesized_rules key (fname, fargs) (* 찾은 규칙 저장 *)
      | None ->
          if transitions <> [] then
            Printf.eprintf
              "Warning: SynthesisFailed for key (%s, %s): No single function \
               explained all %d transitions.\n"
              (show_var v_id_checking)
              (show_parameterizable_action p_action_checking)
              (List.length transitions)
      (* 또는 예외 발생:
        raise (SynthesisFailed (Printf.sprintf "For key (%s, %s), no single function explained all transitions."
          (show_var v_id_checking) (show_parameterizable_action p_action_checking)))
        *))
    observations;

  synthesized_rules

type state = var

(* translate synthesie_rule to expr *)
type synthesized_rule = {
  state : state;
  label : label;
  event : event;
  func : expr; (* Fun of ... *)
}

let rec value_to_expr (v : value) : expr =
  match v with
  | Tree t -> failwith "Tree cannot be converted to expr directly"
  | Const c -> Const c
  | Record r ->
      Record (List.map (fun (v_id, v_val) -> (v_id, value_to_expr v_val)) r)
  | List l -> List (List.map value_to_expr l)
  | Null -> failwith "Null cannot be converted to expr"
  | HandlerHole l -> HandlerHole l

let translate_synthesized_rule (var_id : var)
    (p_action : parameterizable_action) (fname : string) (args : value list) :
    synthesized_rule =
  let label, event =
    match p_action with P_Click l -> (l, Click) | P_Input l -> (l, Input)
  in
  let args_expr = List.map value_to_expr args in
  let args = [ Access var_id ] @ args_expr in
  let func_expr = Fun { func = fname; args } in
  { state = var_id; label; event; func = func_expr }

let translate_synthesized_rules
    (synthesized_rules :
      (var * parameterizable_action, synthesized_function) Hashtbl.t) :
    synthesized_rule list =
  Hashtbl.fold
    (fun (var_id, p_action) (fname, args) acc ->
      let rule = translate_synthesized_rule var_id p_action fname args in
      rule :: acc)
    synthesized_rules []

(* JavaScript로 변환 *)

let func_to_js (fname : string) (expr_l : string list) : string =
  match fname with
  | "map" -> (
      match expr_l with
      | [ l_expr; f_expr ] -> Printf.sprintf "(%s).map(%s)" l_expr f_expr
      | _ ->
          raise
            (ParameterLengthError
               "Map: Expected 2 arguments (list_expr, function_expr)"))
  | "concat_list" -> (
      match expr_l with
      | [ l1_expr; l2_expr ] -> Printf.sprintf "[...%s, ...%s]" l1_expr l2_expr
      | _ ->
          raise
            (ParameterLengthError
               "ConcatList: Expected 2 arguments (list1_expr, list2_expr)"))
  | "length" -> (
      match expr_l with
      | [ l_expr ] -> Printf.sprintf "(%s).length" l_expr
      | _ ->
          raise (ParameterLengthError "Length: Expected 1 argument (list_expr)")
      )
  | "filter" -> (
      match expr_l with
      | [ l_expr; f_expr ] -> Printf.sprintf "(%s).filter(%s)" l_expr f_expr
      | _ ->
          raise
            (ParameterLengthError
               "Filter: Expected 2 arguments (list_expr, function_expr)"))
  | "find" -> (
      match expr_l with
      | [ l_expr; f_expr ] -> Printf.sprintf "(%s).find(%s)" l_expr f_expr
      | _ ->
          raise
            (ParameterLengthError
               "Find: Expected 2 arguments (list_expr, function_expr)"))
  | "push" -> (
      match expr_l with
      | [ l_expr; item_expr ] -> Printf.sprintf "[%s, ...%s]" item_expr l_expr
      | _ ->
          raise
            (ParameterLengthError
               "Push: Expected 2 arguments (list_expr, item_expr)"))
  | "pop" -> (
      match expr_l with
      | [ l_expr ] -> Printf.sprintf "(%s).slice(1)" l_expr
      | _ -> raise (ParameterLengthError "Pop: Expected 1 argument (list_expr)")
      )
  | "plus" -> (
      match expr_l with
      | [ v1_expr; v2_expr ] -> Printf.sprintf "(%s + %s)" v1_expr v2_expr
      | _ ->
          raise
            (ParameterLengthError
               "Plus: Expected 2 arguments (val1_expr, val2_expr)"))
  | "minus" -> (
      match expr_l with
      | [ v1_expr; v2_expr ] -> Printf.sprintf "(%s - %s)" v1_expr v2_expr
      | _ ->
          raise
            (ParameterLengthError
               "Minus: Expected 2 arguments (val1_expr, val2_expr)"))
  | "times" -> (
      match expr_l with
      | [ v1_expr; v2_expr ] -> Printf.sprintf "(%s * %s)" v1_expr v2_expr
      | _ ->
          raise
            (ParameterLengthError
               "Times: Expected 2 arguments (val1_expr, val2_expr)"))
  | "divide" -> (
      match expr_l with
      | [ v1_expr; v2_expr ] -> Printf.sprintf "((%s / %s) | 0)" v1_expr v2_expr
      | _ ->
          raise
            (ParameterLengthError
               "Divide: Expected 2 arguments (val1_expr, val2_expr)"))
  | "change_string_to" -> (
      match expr_l with
      | [ _target_type_expr; new_s_expr ] -> new_s_expr
      | _ ->
          raise
            (ParameterLengthError
               "ChangeStringTo: Expected 2 arguments (target_type_expr, \
                new_string_expr)"))
  | "set_to_const_string" -> (
      match expr_l with
      | [ _v_old_expr; new_s_const_expr ] -> new_s_const_expr
      | _ ->
          raise
            (ParameterLengthError
               "SetToConstString: Expected 2 arguments (old_val_expr, \
                new_string_const_expr)"))
  | "change_string" -> (
      match expr_l with
      | [ _v1_expr; s_expr ] -> s_expr
      | _ ->
          raise
            (ParameterLengthError
               "ChangeString: Expected 2 arguments (val1_expr, new_string_expr)")
      )
  | "concat_str" -> (
      match expr_l with
      | [ s1_expr; s2_expr ] -> Printf.sprintf "(%s + %s)" s1_expr s2_expr
      | _ ->
          raise
            (ParameterLengthError
               "ConcatStr: Expected 2 arguments (string1_expr, string2_expr)"))
  | _ -> failwith ("Unknown function for JS translation: " ^ fname)

let test () =
  let counter_abstraction =
    {
      sketch = expr_of_tree (Const (Int 0));
      init = [ (Var 1, Const (Int 0)) ];
      steps =
        [
          ((Label 1, Click, None), [ (Var 1, Const (Int 1)) ]);
          ((Label 1, Click, None), [ (Var 1, Const (Int 2)) ]);
          ((Label 1, Click, None), [ (Var 1, Const (Int 3)) ]);
          ((Label 2, Click, None), [ (Var 1, Const (Int 2)) ]);
          ((Label 2, Click, None), [ (Var 1, Const (Int 1)) ]);
          ((Label 2, Click, None), [ (Var 1, Const (Int 0)) ]);
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

  let string_input_abstraction =
    {
      sketch = expr_of_tree (Const (String "initial"));
      init = [ (Var 10, Const (String "initial")) ];
      steps =
        [
          ((Label 5, Input, Some "world"), [ (Var 10, Const (String "world")) ]);
          ((Label 1, Click, None), [ (Var 10, Const (String "hello")) ]);
          ( (Label 5, Input, Some "changed"),
            [ (Var 10, Const (String "changed")) ] );
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
      steps =
        [
          ( (Label 5, Input, Some "world"),
            [ (Var 10, Const (String "initial world")) ] );
          ((Label 1, Click, None), [ (Var 10, Const (String "hello")) ]);
          ( (Label 5, Input, Some "changed"),
            [ (Var 10, Const (String "hello changed")) ] );
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
      steps =
        [
          ((Label 100, Click, None), [ (Var 20, List []) ]);
          ( (Label 50, Click, None),
            [ (Var 20, List [ Record [ (Var 1, Const (Int 1)) ] ]) ] );
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
    rules_list
