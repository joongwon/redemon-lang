open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

(* Gemini API 엔드포인트와 키 (환경 변수에서 읽기) *)
let api_key =
  try Sys.getenv "GEMINI_API_KEY" with Not_found -> "YOUR_GEMINI_API_KEY_HERE"

(* Gemini Pro 모델을 위한 엔드포인트 *)
let api_url =
  Printf.sprintf
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=%s"
    api_key

(* LLM 응답을 파싱하기 위한 타입 (OpenAI와 동일하게 유지 가능) *)
type llm_synthesis_response = { func_name : string; args : Yojson.Basic.t list }

let parse_llm_response (json_str : string) :
    (llm_synthesis_response, string) result =
  try
    let open Yojson.Basic.Util in
    (* Gemini는 JSON 출력 형식을 보장하는 기능이 덜 안정적일 수 있으므로, JSON 문자열 안에 마크다운 블록이 포함될 경우를
       대비한 추가 처리 *)
    let clean_json_str =
      if String.starts_with ~prefix:"```json" json_str then
        let s = String.sub json_str 7 (String.length json_str - 10) in
        String.trim s
      else json_str
    in
    let json = Yojson.Basic.from_string clean_json_str in
    let func_name = json |> member "function" |> to_string in
    let args = json |> member "args" |> to_list in
    Ok { func_name; args }
  with e ->
    Error
      (Printf.sprintf "Failed to parse LLM response: %s. \nRaw response: %s"
         (Printexc.to_string e) json_str)

(* 실제 Gemini API를 호출하는 함수 *)
let call_gemini_api (prompt : string) : (string, string) result Lwt.t =
  let headers =
    Header.init () |> fun h -> Header.add h "Content-Type" "application/json"
  in
  (* Gemini의 요청 본문 구조 *)
  let body_str =
    `Assoc
      [
        ( "contents",
          `List
            [
              `Assoc
                [
                  ("role", `String "user");
                  ("parts", `List [ `Assoc [ ("text", `String prompt) ] ]);
                ];
            ] );
        (* 시스템 프롬프트는 contents의 첫 부분에 넣거나, 튜닝된 모델을 사용해야 함. 여기서는 간단하게 user 프롬프트에
           모두 포함시킴 *)
        ( "generationConfig",
          `Assoc
            [
              ("temperature", `Float 0.0);
              ("responseMimeType", `String "application/json");
              (* JSON 응답 요청 *)
            ] );
      ]
    |> Yojson.Safe.to_string
  in
  let body = Cohttp_lwt.Body.of_string body_str in
  Client.post (Uri.of_string api_url) ~headers ~body >>= fun (resp, body) ->
  let status = Response.status resp in
  if Code.is_success (Code.code_of_status status) then
    Cohttp_lwt.Body.to_string body >|= fun body_str ->
    try
      (* Gemini의 응답 구조 파싱 *)
      let json = Yojson.Basic.from_string body_str in
      let content =
        Yojson.Basic.Util.(
          json |> member "candidates" |> index 0 |> member "content"
          |> member "parts" |> index 0 |> member "text" |> to_string)
      in
      Ok content
    with e ->
      Error
        (Printf.sprintf
           "Failed to extract content from Gemini response: %s. \nRaw Body: %s"
           (Printexc.to_string e) body_str)
  else
    Cohttp_lwt.Body.to_string body >|= fun err_body ->
    Error
      (Printf.sprintf "Gemini API call failed with status %s: %s"
         (Code.string_of_status status)
         err_body)
