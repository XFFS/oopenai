module Ooo = Oooapi_lib
open Lwt_result.Syntax

(* Configure test verbosity *)
let verbosity : [ `Quiet | `Verbose | `Debug ] =
  Sys.getenv_opt "VERBOSITY" |> Option.value ~default:"quiet" |> function
  | "quiet" -> `Quiet
  | "verbose" -> `Verbose
  | "debug" -> `Debug
  | invalid -> failwith ("Invalid test VERBOSITY: " ^ invalid)

module Config : Ooo.Config = struct
  let bearer_token = Sys.getenv_opt "OPENAI_API_KEY"
  let default_headers = None
end

module Data = Oopenai.Data
module API = Oopenai.Make (Ooo.Cohttp_client) (Config)

let free () =
  (* print_endline "freeing all resources"; *)
  Lwt.return ()

type request_err =
  [ `Request of Cohttp.Code.status_code * string
  | `Deseriaization of string * string
  ]

type 'a request_result = ('a, request_err) result Lwt.t

(** 
    The generated library's endpoint function always evaluate to a value of type
    [('resp, 'err) result Lwt.t]. Which is a promise for a result which is either,
    
    - [Ok 'resp], where ['resp] is the response data returned by the server 
       responded as expected, or
    - [Error 'err], where ['err] is either a deserialization error or an 
       error response from the server. *)
let ok_or_fail (res: 'ok request_result): 'ok Lwt.t =
  let f v = match (v: ('ok, request_err) result) with
    | Ok ok -> Lwt.return ok
    | Error err -> 
    match err with 
    | `Request (status_code, message) -> Alcotest.failf "Test failed with request error: status code %s and error message %s" (Cohttp.Code.string_of_status status_code) message
    | `Deseriaization (data, message) -> Alcotest.failf "Test failed with deseriaization: data %s and error message %s" data message
  in
  Lwt.bind res f
  
let test_lwt (test_case : unit -> (bool, 'err) result Lwt.t) switch () =
  Lwt_switch.add_hook (Some switch) free;
  let result = ok_or_fail (test_case ()) in
  Lwt.map (Alcotest.(check bool) "should be true" true) result

let test name (test_case : unit -> (bool, 'err) result Lwt.t) : unit Alcotest_lwt.test_case =
  Alcotest_lwt.test_case name `Quick (test_lwt test_case)

(** Enrich the error  messga ewith the specific name of the endpoint, used when many tests are grouped together. *)
let add_endpoint_to_error_message (endpoint:string) result =
  let prefix = "on endpoint " ^ endpoint ^ " "in
  result 
  |> Lwt_result.map_error (function 
    | `Request (code, msg) -> `Request (code, prefix ^ msg) 
    | `Deseriaization (data, msg) ->  `Deseriaization (data, prefix ^ msg))

let model = "babbage-002"

let list_fine_tune_tests =
  Alcotest_lwt.test_case
    "can do all them fine tunes - except canceling it, or deleting the fine \
     tuned model"
    (* The cancel fine_tune endpoint succeeds only if the fine tune creation is still pending.
       Therefore it is tested separately. *)
    (* The delete_model endpoint is also tested sperately,
       since it might take minutes or hours for a fine tuned model to finish processing.
    *)
    `Quick
    begin
      fun _switch () ->
        let file_path = "./test_files/fine_tune.jsonl" in
        let purpose = "fine-tune" in

        (* Create the file for fine tune. *)
        let result = 
          let create_file_request = 
            Data.CreateFileRequest.make  ~file:(`File file_path) ~purpose in
          let* resp = 
            create_file_request
            |> API.create_file 
            |> add_endpoint_to_error_message "create_file"
          in
          let file_id = resp.id in

          (* Create fine tune. *)
          let create_fine_tuning_job_request =
            Data.CreateFineTuningJobRequest.make ~model ~training_file:file_id ()
          in
          let* resp = 
            API.create_fine_tuning_job create_fine_tuning_job_request 
            |> add_endpoint_to_error_message "create_fine_tuning_job"
          in
          let fine_tune_id = resp.id in

          (* Sleep for 10 sec, otherwise deleting file might fail due to the file is still being processed. *)
          Unix.sleep 10;

          (* Test retrieve_fine_tune. *)
          let* resp = 
            API.retrieve_fine_tuning_job ~fine_tuning_job_id:fine_tune_id () 
            |> add_endpoint_to_error_message "retrieve_fine_tuning_job"
          in
          Alcotest.(check string) "fine tune id is same" fine_tune_id resp.id;

          (* Test list_fine_tunes. *)
          let* resp = API.list_paginated_fine_tuning_jobs () in
          Alcotest.(check bool)
            "fine tune id is present"
            true
            (List.exists
              (fun Data.FineTuningJob.{ id; _ } -> String.equal fine_tune_id id)
              resp.data);

          (* Test list_fine_tune_events. *)
          let* resp = API.list_fine_tuning_events ~fine_tuning_job_id:fine_tune_id () in
          Alcotest.(check bool)
            "event list is not empty"
            true
            (List.length resp.data > 0);

          (* Tear down. *)
          let* _ = API.delete_file ~file_id () in
          Lwt_result.return ()
      in
      ok_or_fail result
    end

let cancel_fine_tune_tests =
  Alcotest_lwt.test_case
    "can cancel fine tune test"
    `Quick
    begin
      fun _swtich () ->
        let file_path = "./test_files/fine_tune.jsonl" in
        let purpose = "fine-tune" in

        let result =
        (* Create the file for fine tune. *)
          let create_file_request = 
            Data.CreateFileRequest.make  ~file:(`File file_path) ~purpose in
          let* resp = API.create_file create_file_request in
          let file_id = resp.id in

          (* Create fine tune. *)
          let create_fine_tuning_job_request =
            Data.CreateFineTuningJobRequest.make ~model ~training_file:file_path ()
          in
          let* resp = API.create_fine_tuning_job create_fine_tuning_job_request in
          let fine_tune_id = resp.id in

          (* Test cancel fine tune*)
          let* resp = API.cancel_fine_tuning_job ~fine_tuning_job_id:fine_tune_id () in
          Alcotest.(check string) "status is cancelled" "cancelled" resp.status;

          (* Sleep for 10 sec, otherwise deleting file might fail due to the file is still being processed. *)
          Unix.sleep 10;

          (* Tear down. *)
          let _ = API.delete_file ~file_id () in
          Lwt_result.return ()
        in
        ok_or_fail result

    end

let fine_tune_tests =
  ( "fine tune endpoint tests"
  , [ `Enabled, list_fine_tune_tests; `Disabled, cancel_fine_tune_tests ] )

let file_tests =
  Alcotest_lwt.test_case
    "can do all them file tests - except download_file"
    (* download_file is not included since it can requires paid accounts. *)
    `Quick
    begin
      fun _swith () ->
        let file_path = "./test_files/fine_tune.jsonl" in
        (* The only value allowed in purpose is "fine-tune". *)
        let purpose = "fine-tune" in

        let result = 
          (* Create file. *)
          let create_file_request = 
            Data.CreateFileRequest.make  ~file:(`File file_path) ~purpose in
          let* resp = API.create_file create_file_request in
          let file_id = resp.id in

          (* Sleep for 10 sec, otherwise deleting file might fail due to the file is still being processed. *)
          Unix.sleep 10;

          (* Test retrieve_file *)
          let* resp = API.retrieve_file ~file_id () in
          Alcotest.(check string) "file id is present" file_id resp.id;

          (* Test list_files *)
          let* resp = API.list_files () in
          Alcotest.(check bool)
            "file list is not empty"
            true
            (List.length resp.data > 0);

          (* Test delete_file *)
          let* resp = API.delete_file ~file_id () in
          Alcotest.(check bool) "file is deleted" true resp.deleted;
          Lwt_result.return ()
        in
        ok_or_fail result
    end

let file_tests = "file endpoint tests", [ `Disabled, file_tests ]

let other_endpoint_tests =
  ( "other endpoint tests"
  , [ ( `Disabled
      , test
          "can create_completion"
          begin
            fun () ->
              let create_completion_request =
                Data.CreateCompletionRequest.make 
                  ~model:model 
                  ~prompt:"Give me dogs"
                  ~n:5
                  ()
              in
              let+ resp = API.create_completion create_completion_request in
              List.length resp.choices = 10
          end )
    ; ( `Disabled
      , test
          "can create_embedding"
          begin
            fun () ->
              let create_embedding_request =
                Data.CreateEmbeddingRequest.make 
                  ~model:"text-embedding-ada-002" 
                  ~input:"Are cats cool or cooler?"
                  ()
              in
              let+ resp = API.create_embedding create_embedding_request in
              List.length resp.data != 0
          end )
    ; ( `Disabled
      , test
          "can create_image"
          begin
            fun () ->
              let create_image_request =
                Data.CreateImageRequest.make
                  ~prompt:"The actor Nicolas Cage standing on a table asking why it \
                   can be misfiled."
                   ()
              in
              let+ resp = API.create_image create_image_request in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can create_image_edit"
          begin
            fun () ->
              let create_image_edit_request = 
                Data.CreateImageEditRequest.make
                  ~image:(`File "./test_files/image_edit_original.png")
                  ~prompt:"Add a flamingo to the pool"
                  ~mask:(`File "./test_files/image_edit_mask.png")
              ()
              in
              let+ resp = API.create_image_edit create_image_edit_request in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can create_image_variation"
          begin
            fun () ->
              let create_image_variation_request = 
                Data.CreateImageVariationRequest.make 
                  ~image:(`File "./test_files/image_edit_original.png")
                  ()
              in
              let+ resp = API.create_image_variation create_image_variation_request in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can create_moderation"
          begin
            fun () ->
              let create_moderation_request =
                Data.CreateModerationRequest.make
                  ~input:"I want to kill them or give them cake!"
                  ()
              in
              let+ resp = API.create_moderation create_moderation_request in
              List.length resp.results != 0
          end )
    ; ( `Disabled
      , test
          (* The delete_model endpoint is tested sperately here,
             since it might take minutes or hours for a fine tuned model to finish processing. *)
          "can delete_model"
          begin
            fun () ->
              let+ resp =
                API.delete_model ~model:"curie:ft-synechist-2023-03-22-15-16-01"
                ()
              in
              resp.deleted
          end )
    ; ( `Disabled
      , test
          (* Cannot test without paid account. *)
          "can download_file"
          begin
            fun () ->
              let+ _ =
                API.download_file ~file_id:"file-0iKqm72yADJazJ74oROFKx9v"
                ()
              in
              true
          end )
    ; ( `Disabled
      , test
          "can list_models"
          begin
            fun () ->
              let+ resp = API.list_models () in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can retrieve_model"
          begin
            fun () ->
              let+ resp = API.retrieve_model ~model:"gpt-3.5-turbo-instruct" ()in
              String.equal resp.id "gpt-3.5-turbo-instruct"
          end )
    ] )

let filter_out_disabled (suite_name, tests) =
  let filter_enabled =
    match Sys.getenv_opt "RUN_DISABLED" with
    | Some ("true" | "1") -> List.map snd (* just accept all tests *)
    | _ ->
        List.filter_map (function
            | `Enabled, t -> Some t
            | `Disabled, _ -> None)
  in
  suite_name, filter_enabled tests

let test_suites = [ fine_tune_tests; file_tests; other_endpoint_tests ]

let configure_logging () =
  let all_log_level =
    match verbosity with
    | `Quiet
    | `Verbose ->
        None
    | `Debug -> Some Logs.Debug
  in
  let application_log_level =
    match verbosity with
    | `Quiet -> None
    | `Verbose
    | `Debug ->
        Some Logs.Debug
  in
  if not @@ Cohttp_lwt_unix.Debug.debug_active () then (
    Fmt_tty.setup_std_outputs ();
    Logs.set_level ~all:true all_log_level;
    Logs.Src.list ()
    |> List.iter (fun src ->
           match Logs.Src.name src with
           (* Enable just cohttp-lwt and cohttp-lwt-unix logs *)
           | "cohttp.lwt.io"
           | "cohttp.lwt.server"
           | "application" ->
               Logs.Src.set_level src application_log_level
           | _ -> ());
    Logs.set_reporter Cohttp_lwt_unix.Debug.default_reporter
  )

let run_tests () =
  let argv =
    match verbosity with
    | `Quiet -> None
    | _ -> Some [| "tests"; "--verbose" |]
  in
  test_suites
  |> List.map filter_out_disabled
  |> Alcotest_lwt.run ?argv "oopenai tests"
  |> Lwt_main.run

let () =
  configure_logging ();
  run_tests ()
