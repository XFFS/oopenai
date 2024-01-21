module Ooo = Oooapi_lib
open Oopenai.Data

(* Configure autherization thru environment variables *)
module Config : Ooo.Config = struct
  let bearer_token = Sys.getenv_opt "OPENAI_API_KEY"
  let default_headers = None
end

module API = Oopenai.Make (Ooo.Cohttp_client) (Config)

(* A utility function *)
let print_option = function
  | Some v -> Lwt_io.printl v
  | None -> Lwt.return_unit

open Lwt_result.Syntax

let main =
  (* List models *)
  let* resp = API.list_models () in
  let* () =
    resp.data
    |> Lwt_list.iter_s (fun (m : Model.t) -> Lwt_io.printl m.id)
    |> Lwt_result.ok
  in

  (* Create completion *)
  let* resp =
    API.create_completion
    @@ CreateCompletionRequest.make
         ~model:"davinci-002"
         ~prompt:"Give me dogs and cats"
         ~n:5
         ()
  in
  let+ () =
    resp.choices
    |> Lwt_list.iter_s (fun (choice : CreateCompletionResponse.choices_item) ->
           Lwt_io.printl choice.choices_item_text)
    |> Lwt_result.ok
  in
  ()

let _ =
  match Lwt_main.run main with
  | Ok () -> ()
  | Error (`Deseriaization (resp, error)) ->
      Printf.eprintf "Error %s when deserializing response %s" error resp;
      exit 1
  | Error (`Request (code, msg)) ->
      Printf.eprintf
        "Error respnose %d from server with message %s"
        (Cohttp.Code.code_of_status code)
        msg;
      exit 1
