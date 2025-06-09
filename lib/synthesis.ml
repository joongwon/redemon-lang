open Tree.Syntax
open Texpr
open Demo
open Abstract
open Lwt.Syntax
open Api

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
  | List (_ :: l) -> List l
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
  match act with
  | { label = l; action_type = Click; _ } -> P_Click l
  | { label = l; action_type = Input; _ } -> P_Input l

type synthesized_function = string * value list

(* value 변화도 부품 *)

let extract_related_components (old_val : value) (new_val : value) : value list
    =
  match (old_val, new_val) with
  (* 1. 정수 관계 *)
  | Const (Int i1), Const (Int i2) ->
      let components = ref [] in
      (* 덧셈/뺄셈 관계 *)
      if i1 <> i2 then components := Const (Int (i2 - i1)) :: !components;
      (* 곱셈/나눗셈 관계 *)
      if i1 <> 0 && i2 mod i1 = 0 then
        components := Const (Int (i2 / i1)) :: !components;
      !components
  (* 2. 문자열 관계 *)
  | Const (String s1), Const (String s2) ->
      let components = ref [] in
      let len1 = String.length s1 in
      let len2 = String.length s2 in
      (* 접미사(suffix)가 추가된 경우 *)
      (if len2 > len1 && String.starts_with ~prefix:s1 s2 then
         let suffix = String.sub s2 len1 (len2 - len1) in
         components := Const (String suffix) :: !components);
      (* 접두사(prefix)가 추가된 경우 *)
      (if len2 > len1 && String.ends_with ~suffix:s1 s2 then
         let prefix = String.sub s2 0 (len2 - len1) in
         components := Const (String prefix) :: !components);
      !components
  (* 3. 리스트 관계 *)
  | List l1, List l2 ->
      let components = ref [] in
      let len1 = List.length l1 in
      let len2 = List.length l2 in
      (* 원소가 맨 앞에 추가(push)된 경우 *)
      (if len2 = len1 + 1 then
         match l2 with
         | hd :: tl when equal_value (List tl) (List l1) ->
             components := hd :: !components
         | _ -> ());
      (* 원소가 맨 뒤에 추가된 경우  *)
      (if len2 = len1 + 1 then
         let rec get_last_and_init = function
           | [] -> None
           | [ x ] -> Some ([], x)
           | h :: t -> (
               match get_last_and_init t with
               | Some (init, last) -> Some (h :: init, last)
               | None -> None)
         in
         match get_last_and_init l2 with
         | Some (init, last) when equal_value (List init) (List l1) ->
             components := last :: !components
         | _ -> ());

      !components
  | _, _ -> []

let format_prompt_for_gemini (key : var * parameterizable_action)
    (transitions : (value * value * action) list) : string =
  let var_id, p_action = key in
  let action_desc =
    match p_action with
    | P_Click l ->
        Printf.sprintf "user CLICKS the element with label '%s'" (show_label l)
    | P_Input l ->
        Printf.sprintf "user INPUTS text into the element with label '%s'"
          (show_label l)
  in
  let transitions_str =
    transitions
    |> List.map (fun (old_val, new_val, act) ->
           let input_info =
             match act with
             | { action_type = Input; arg = Some s; _ } ->
                 Printf.sprintf " (with input: \"%s\")" s
             | _ -> ""
           in
           Printf.sprintf "  - From '%s' to '%s'%s" (show_value old_val)
             (show_value new_val) input_info)
    |> String.concat "\n"
  in
  let function_library =
    "Available functions:\n\
     - plus(val, num): Returns val + num.\n\
     - minus(val, num): Returns val - num.\n\
     - pop(list): Removes the first element of a list.\n\
     - push(list, item): Adds an item to the front of a list.\n\
     - concat_str(str1, str2): Concatenates two strings.\n\
     - set_to_const_string(old_val, new_str_const): Sets the value to a new \
     string constant.\n\
     - set_to_input_string(old_val): Sets the value to the string provided by \
     the user's input action. The user input is provided in the examples.\n"
  in

  (* Gemini는 OpenAI의 'system' 역할과 같은 강력한 역할 지정 기능이 덜 명확하므로,
     모든 지시사항을 user 프롬프트 안에 명확하게 포함하는 것이 중요. *)
  Printf.sprintf
    "You are a program synthesis expert. Your task is to find a single \
     function call that explains a series of state changes.\n\n\
     A variable '%s' changes its value when the %s.\n\
     Here are the observed transitions:\n\
     %s\n\n\
     %s\n\
     Based on these examples, what single function call updates the old value \
     to the new value?\n\
     The first argument to the function is always the old value of the \
     variable '%s'.\n\
     For 'set_to_input_string', the function takes only one argument (the old \
     value), and you must infer its usage from the examples.\n\
     For other functions, if they need a constant argument (like a number or a \
     string), provide that constant value.\n\n\
     Your response MUST be a single, valid JSON object in the following \
     format, and nothing else:\n\
     {\"function\": \"<function_name>\", \"args\": [<arg1>, <arg2>, ...]}"
    (show_var var_id) action_desc transitions_str function_library
    (show_var var_id)

let synthesize (abstraction_data : abstraction_multi) :
    (var * parameterizable_action, synthesized_function) Hashtbl.t =
  let { init; timelines } = abstraction_data in

  (* 1. 모든 고유 변수 및 컴포넌트(상수) 수집 *)
  let all_vars_list = ref (List.map fst init) in
  let components = ref [] in

  let add_const_to_components v =
    match v with
    | Const c ->
        if not (List.mem (Const c) !components) then
          components := Const c :: !components
    | Record r_val ->
        if not (List.mem (Record r_val) !components) then
          components := Record r_val :: !components
    | _ -> ()
  in

  List.iter (fun (_, v) -> add_const_to_components v) init;

  List.iter
    (fun timeline ->
      List.iter
        (fun (_, r) ->
          all_vars_list := List.map fst r @ !all_vars_list;
          List.iter (fun (_, v) -> add_const_to_components v) r)
        timeline)
    timelines;

  let all_vars = List.sort_uniq compare !all_vars_list in

  (* 모든 타임라인에서 관찰 결과 수집 *)
  (* key: (var_id, p_action), value: (old_val, new_val, actual_action) list *)
  let observations = Hashtbl.create (List.length all_vars * 5) in

  (* 각 타임라인은 독립적으로 초기 상태(init)에서 시작 *)
  List.iter
    (fun timeline ->
      let current_s = ref init in
      List.iter
        (fun (act, next_s) ->
          let prev_s = !current_s in
          let p_act = to_param_action act in
          List.iter
            (fun v_id ->
              try
                let old_val = lookup_val v_id prev_s in
                let new_val = lookup_val v_id next_s in
                if not (equal_value old_val new_val) then
                  let key = (v_id, p_act) in
                  let existing_obs =
                    try Hashtbl.find observations key with Not_found -> []
                  in
                  (* 모든 타임라인의 관찰 결과를 하나의 키 아래에 누적 *)
                  Hashtbl.replace observations key
                    ((old_val, new_val, act) :: existing_obs)
              with NotFound _ -> ())
            all_vars;
          current_s := next_s)
        timeline)
    timelines;

  (* add default value *)
  add_const_to_components (Const (Int 0));
  add_const_to_components (Const (Int 1));
  add_const_to_components (Const (Int (-1)));
  add_const_to_components (Const (String ""));

  (* helper function *)
  let add_const_to_components v =
    match v with
    | Const c -> components := Const c :: !components
    | Record r_val -> components := Record r_val :: !components
    | _ -> ()
  in
  let add_components_from_list l = List.iter add_const_to_components l in
  (* step 2.5: value 변화 추출 *)
  Hashtbl.iter
    (fun _key transitions ->
      List.iter
        (fun (old_val, new_val, _) ->
          let related_components = extract_related_components old_val new_val in
          add_components_from_list related_components)
          (* (MODIFIED) 반환된 리스트 전체를 부품에 추가 *)
        transitions)
    observations;

  let unique_components = List.sort_uniq compare !components in

  (* debug : print all components*)
  (* Printf.printf "All components: %s\n"
    (String.concat "; " (List.map show_value unique_components)); *)

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
               | { action_type = Input; arg = Some input_str; _ } -> (
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
                  Printf.eprintf "  WARNING: op_args_opt was None for %s\n"
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

let rec value_of_yojson (json : Yojson.Basic.t) : value =
  match json with
  | `Int i -> Const (Int i)
  | `Float f ->
      (* JSON은 보통 숫자를 float으로 다루므로, 정수로 변환해줍니다. *)
      Const (Int (int_of_float f))
  | `String s -> Const (String s)
  | `List l ->
      (* JSON 리스트의 각 원소에 대해 재귀적으로 변환을 적용합니다. *)
      List (List.map value_of_yojson l)
  | `Assoc o ->
      (* JSON 객체는 우리 시스템의 'Record' 타입으로 변환합니다. *)
      (* 키(key)는 문자열 형태이므로, 이를 'Var' 타입으로 변환해야 합니다. *)
      let record_items =
        List.map
          (fun (key_str, v_json) ->
            try
              (* "1", "2"와 같은 문자열 키를 Var 1, Var 2로 변환 *)
              let var_id = Var (int_of_string key_str) in
              let value_item = value_of_yojson v_json in
              (var_id, value_item)
            with Failure _ ->
              failwith
                (Printf.sprintf
                   "Invalid record key in LLM response: '%s'. Key must be a \
                    string representing an integer."
                   key_str))
          o
      in
      Record record_items
  | `Null -> Null
  | `Bool _ -> failwith "Boolean to 'value' conversion is not implemented."

let synthesize_with_llm (abstraction_data : abstraction_multi) :
    (var * parameterizable_action, synthesized_function) Hashtbl.t =
  let { init; timelines } = abstraction_data in
  let all_vars_list = ref (List.map fst init) in
  let components = ref [] in

  let add_const_to_components v =
    match v with
    | Const c ->
        if not (List.mem (Const c) !components) then
          components := Const c :: !components
    | Record r_val ->
        if not (List.mem (Record r_val) !components) then
          components := Record r_val :: !components
    | _ -> ()
  in

  List.iter (fun (_, v) -> add_const_to_components v) init;

  List.iter
    (fun timeline ->
      List.iter
        (fun (_, r) ->
          all_vars_list := List.map fst r @ !all_vars_list;
          List.iter (fun (_, v) -> add_const_to_components v) r)
        timeline)
    timelines;

  let all_vars = List.sort_uniq compare !all_vars_list in

  (* 1. 관찰 결과 수집 (기존과 동일) *)
  let observations = Hashtbl.create (List.length all_vars * 5) in

  (* 각 타임라인은 독립적으로 초기 상태(init)에서 시작 *)
  List.iter
    (fun timeline ->
      let current_s = ref init in
      List.iter
        (fun (act, next_s) ->
          let prev_s = !current_s in
          let p_act = to_param_action act in
          List.iter
            (fun v_id ->
              try
                let old_val = lookup_val v_id prev_s in
                let new_val = lookup_val v_id next_s in
                if not (equal_value old_val new_val) then
                  let key = (v_id, p_act) in
                  let existing_obs =
                    try Hashtbl.find observations key with Not_found -> []
                  in
                  (* 모든 타임라인의 관찰 결과를 하나의 키 아래에 누적 *)
                  Hashtbl.replace observations key
                    ((old_val, new_val, act) :: existing_obs)
              with NotFound _ -> ())
            all_vars;
          current_s := next_s)
        timeline)
    timelines;

  let synthesized_rules = Hashtbl.create (Hashtbl.length observations) in

  (* 2. 각 관찰에 대해 Gemini API 호출 및 결과 파싱 *)
  let process_observation (key, transitions_rev) =
    let transitions = List.rev transitions_rev in
    (* (MODIFIED) Gemini용 프롬프트 포맷터 호출 *)
    let prompt = format_prompt_for_gemini key transitions in
    Lwt_main.run
      ((* (MODIFIED) Gemini API 호출 함수 사용 *)
       let* result = call_gemini_api prompt in
       match result with
       | Error e ->
           Printf.eprintf "Gemini API Error for key %s: %s\n"
             (show_parameterizable_action (snd key))
             e;
           Lwt.return_unit
       | Ok response_str -> (
           match parse_llm_response response_str with
           | Error e ->
               Printf.eprintf "Gemini Response Parsing Error for key %s: %s\n"
                 (show_parameterizable_action (snd key))
                 e;
               Lwt.return_unit
           | Ok response -> (
               try
                 let args =
                   if response.func_name = "set_to_input_string" then []
                   else List.map value_of_yojson response.args
                 in
                 Hashtbl.add synthesized_rules key (response.func_name, args);
                 Lwt.return_unit
               with
               | Failure msg ->
                   Printf.eprintf "Error converting LLM args to values: %s\n"
                     msg;
                   Lwt.return_unit
               | e ->
                   Printf.eprintf
                     "An unexpected error occurred during arg conversion: %s\n"
                     (Printexc.to_string e);
                   Lwt.return_unit)))
  in

  Hashtbl.iter
    (fun key transitions -> process_observation (key, transitions))
    observations;
  synthesized_rules

type state = var [@@deriving show, eq]

(* translate synthesie_rule to expr *)
type synthesized_rule = {
  state : state;
  label : label;
  action_type : action_type;
  func : expr; (* Fun of ... *)
}
[@@deriving show, eq]

let rec value_to_expr (v : value) : expr =
  match v with
  | Tree _ -> failwith "Tree cannot be converted to expr directly"
  | Const c -> Const c
  | Record r ->
      Record (List.map (fun (v_id, v_val) -> (v_id, value_to_expr v_val)) r)
  | List l -> List (List.map value_to_expr l)
  | Null -> failwith "Null cannot be converted to expr"
  | HandlerHole l -> HandlerHole l

let translate_synthesized_rule (var_id : var)
    (p_action : parameterizable_action) (fname : string) (args : value list) :
    synthesized_rule =
  let label, action_type =
    match p_action with P_Click l -> (l, Click) | P_Input l -> (l, Input)
  in
  let args_expr = List.map value_to_expr args in
  let args = [ Access var_id ] @ args_expr in
  let func_expr = Fun { func = fname; args } in
  { state = var_id; label; action_type; func = func_expr }

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
  let rules = synthesize_with_llm counter_abstraction in
  Hashtbl.iter
    (fun (var_id, p_action) (fname, args) ->
      Printf.printf "Var %s, Action %s: Func: %s, Args: [%s]\n"
        (show_var var_id)
        (show_parameterizable_action p_action)
        fname
        (String.concat ", " (List.map show_value args)))
    rules;
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
    rules_list
