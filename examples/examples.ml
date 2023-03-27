open Oopenai

(* Configure autherization thru environment variables *)
module Config : Request.Auth = struct
  let api_key = Sys.getenv "OPENAI_API_KEY"
  let org_id = None
end

module API = Open_ai_api.Make (Config)

(* A utility function *)
let print_option = function
  | Some v -> Lwt_io.printl v
  | None -> Lwt.return_unit

let main () =
  let open Lwt.Syntax in
  (* List models *)
  let* resp = API.list_models () in
  let* () =
    Lwt_list.iter_s (fun (m : Model.t) -> Lwt_io.printl m.id) resp.data
  in

  (* Create completion *)
  let create_completion_request_t =
    let req = Create_completion_request.create "ada" in
    let prompt = Some [ "Give me dogs"; "Give me some cats" ] in
    let n = Some 5l in
    { req with prompt; n }
  in
  let* resp = API.create_completion ~create_completion_request_t in
  let+ () =
    Lwt_list.iter_s
      (fun (choice:Create_completion_response_choices_inner.t) ->
        print_option choice.text)
      resp.choices
  in
  ()

let () = Lwt_main.run (main ())
