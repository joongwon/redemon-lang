open Js_of_ocaml
open Lwt.Infix

let call_gemini_api (prompt : string) : (string, string) result Lwt.t =
  let api_key = "" in
  let url =
    Printf.sprintf
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key=%s"
      api_key
  in
  let headers =
    object%js
      val contentType = (Js.string "Content-Type", Js.string "application/json")
    end
  in
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
  let init = Js.Unsafe.obj [||] in

  Js.Unsafe.set init (Js.string "method") (Js.string "POST");
  Js.Unsafe.set init (Js.string "headers") headers;
  Js.Unsafe.set init (Js.string "body") body_str;
  let url_js = Js.string url in
  let init_js = init in

  Lwt.catch
    (fun () ->
      let promise =
        Js.Unsafe.meth_call Js.Unsafe.global "fetch"
          [| Js.Unsafe.inject url_js; Js.Unsafe.inject init_js |]
      in
      Promise_lwt.of_promise promise >>= fun response ->
      if not (Js.to_bool response##.ok) then
        Lwt.return
          (Error (Printf.sprintf "API call failed: %d" response##.status))
      else
        Promise_lwt.of_promise (response##text ()) >>= fun body_text ->
        let body_text_str = Js.to_string body_text in
        try
          let json = Yojson.Basic.from_string body_text_str in
          let content =
            Yojson.Basic.Util.(
              json |> member "candidates" |> index 0 |> member "content"
              |> member "parts" |> index 0 |> member "text" |> to_string)
          in
          Lwt.return (Ok content)
        with e ->
          Lwt.return
            (Error
               (Printf.sprintf "Failed to parse JSON: %s" (Printexc.to_string e))))
    (fun ex ->
      Lwt.return
        (Error
           (Printf.sprintf "JS exception: %s"
              (Js.to_string (Js.Unsafe.get ex "message")))))
