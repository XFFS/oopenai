(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

 (* TODO Move? *)
module Config : Request.Auth = struct
    let api_key = "todo"
    let org_id = None
end

module Request = Request.Make (Config)

let cancel_fine_tune ~fine_tune_id =
    let open Lwt.Infix in
    let uri = Request.build_uri "/fine-tunes/{fine_tune_id}/cancel" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "fine_tune_id" (fun x -> x) fine_tune_id in
    Cohttp_lwt_unix.Client.call `POST uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Fine_tune.of_yojson) resp body

let create_completion ~create_completion_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/completions" in
    let headers = Request.default_headers in
    let body = Request.write_as_json_body Create_completion_request.to_yojson create_completion_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Create_completion_response.of_yojson) resp body

let create_edit ~create_edit_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/edits" in
    let headers = Request.default_headers in
    let body = Request.write_as_json_body Create_edit_request.to_yojson create_edit_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Create_edit_response.of_yojson) resp body

let create_embedding ~create_embedding_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/embeddings" in
    let headers = Request.default_headers in
    let body = Request.write_as_json_body Create_embedding_request.to_yojson create_embedding_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Create_embedding_response.of_yojson) resp body

let create_file ~file ~purpose =
    let open Lwt.Infix in
    let uri = Request.build_uri "/files" in
    let headers = Request.default_headers in
    let body = Request.init_form_encoded_body () in
    let body = Request.add_form_encoded_body_param body "file" (fun x -> x) file in
    let body = Request.add_form_encoded_body_param body "purpose" (fun x -> x) purpose in
    let body = Request.finalize_form_encoded_body body in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Open_ai_file.of_yojson) resp body

let create_fine_tune ~create_fine_tune_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/fine-tunes" in
    let headers = Request.default_headers in
    let body = Request.write_as_json_body Create_fine_tune_request.to_yojson create_fine_tune_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Fine_tune.of_yojson) resp body

let create_image ~create_image_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/images/generations" in
    let headers = Request.default_headers in
    let body = Request.write_as_json_body Create_image_request.to_yojson create_image_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Images_response.of_yojson) resp body

let create_image_edit ~image ~prompt ?mask ?(n = 1l) ?(size = `_1024x1024) ?(response_format = `Url) ?user () =
    let open Lwt.Infix in
    let uri = Request.build_uri "/images/edits" in
    let headers = Request.default_headers in
    let body = Request.init_form_encoded_body () in
    let body = Request.add_form_encoded_body_param body "image" (fun x -> x) image in
    let body = Request.maybe_add_form_encoded_body_param body "mask" (fun x -> x) mask in
    let body = Request.add_form_encoded_body_param body "prompt" (fun x -> x) prompt in
    let body = Request.add_form_encoded_body_param body "n" Int32.to_string n in
    let body = Request.add_form_encoded_body_param body "size" Enums.show_size size in
    let body = Request.add_form_encoded_body_param body "response_format" Enums.show_response_format response_format in
    let body = Request.maybe_add_form_encoded_body_param body "user" (fun x -> x) user in
    let body = Request.finalize_form_encoded_body body in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Images_response.of_yojson) resp body

let create_image_variation ~image ?(n = 1l) ?(size = `_1024x1024) ?(response_format = `Url) ?user () =
    let open Lwt.Infix in
    let uri = Request.build_uri "/images/variations" in
    let headers = Request.default_headers in
    let body = Request.init_form_encoded_body () in
    let body = Request.add_form_encoded_body_param body "image" (fun x -> x) image in
    let body = Request.add_form_encoded_body_param body "n" Int32.to_string n in
    let body = Request.add_form_encoded_body_param body "size" Enums.show_size size in
    let body = Request.add_form_encoded_body_param body "response_format" Enums.show_response_format response_format in
    let body = Request.maybe_add_form_encoded_body_param body "user" (fun x -> x) user in
    let body = Request.finalize_form_encoded_body body in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Images_response.of_yojson) resp body

let create_moderation ~create_moderation_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/moderations" in
    let headers = Request.default_headers in
    let body = Request.write_as_json_body Create_moderation_request.to_yojson create_moderation_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Create_moderation_response.of_yojson) resp body

let create_search ~engine_id ~create_search_request_t =
    let open Lwt.Infix in
    let uri = Request.build_uri "/engines/{engine_id}/search" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "engine_id" (fun x -> x) engine_id in
    let body = Request.write_as_json_body Create_search_request.to_yojson create_search_request_t in
    Cohttp_lwt_unix.Client.call `POST uri ~headers ~body >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Create_search_response.of_yojson) resp body

let delete_file ~file_id =
    let open Lwt.Infix in
    let uri = Request.build_uri "/files/{file_id}" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "file_id" (fun x -> x) file_id in
    Cohttp_lwt_unix.Client.call `DELETE uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Delete_file_response.of_yojson) resp body

let delete_model ~model =
    let open Lwt.Infix in
    let uri = Request.build_uri "/models/{model}" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "model" (fun x -> x) model in
    Cohttp_lwt_unix.Client.call `DELETE uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Delete_model_response.of_yojson) resp body

let download_file ~file_id =
    let open Lwt.Infix in
    let uri = Request.build_uri "/files/{file_id}/content" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "file_id" (fun x -> x) file_id in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.to_string) resp body

let list_files () =
    let open Lwt.Infix in
    let uri = Request.build_uri "/files" in
    let headers = Request.default_headers in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap List_files_response.of_yojson) resp body

let list_fine_tune_events ~fine_tune_id ?(stream = false) () =
    let open Lwt.Infix in
    let uri = Request.build_uri "/fine-tunes/{fine_tune_id}/events" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "fine_tune_id" (fun x -> x) fine_tune_id in
    let uri = Request.add_query_param uri "stream" string_of_bool stream in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap List_fine_tune_events_response.of_yojson) resp body

let list_fine_tunes () =
    let open Lwt.Infix in
    let uri = Request.build_uri "/fine-tunes" in
    let headers = Request.default_headers in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap List_fine_tunes_response.of_yojson) resp body

let list_models () =
    let open Lwt.Infix in
    let uri = Request.build_uri "/models" in
    let headers = Request.default_headers in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap List_models_response.of_yojson) resp body

let retrieve_file ~file_id =
    let open Lwt.Infix in
    let uri = Request.build_uri "/files/{file_id}" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "file_id" (fun x -> x) file_id in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Open_ai_file.of_yojson) resp body

let retrieve_fine_tune ~fine_tune_id =
    let open Lwt.Infix in
    let uri = Request.build_uri "/fine-tunes/{fine_tune_id}" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "fine_tune_id" (fun x -> x) fine_tune_id in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Fine_tune.of_yojson) resp body

let retrieve_model ~model =
    let open Lwt.Infix in
    let uri = Request.build_uri "/models/{model}" in
    let headers = Request.default_headers in
    let uri = Request.replace_path_param uri "model" (fun x -> x) model in
    Cohttp_lwt_unix.Client.call `GET uri ~headers >>= fun (resp, body) ->
    Request.read_json_body_as (JsonSupport.unwrap Model.of_yojson) resp body

