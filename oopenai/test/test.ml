open Oopenai
open Lwt.Syntax

(* Configure test verbosity *)
let verbosity : [ `Quiet | `Verbose | `Debug ] =
  Sys.getenv_opt "VERBOSITY" |> Option.value ~default:"quiet" |> function
  | "quiet" -> `Quiet
  | "verbose" -> `Verbose
  | "debug" -> `Debug
  | invalid -> failwith ("Invalid test VERBOSITY: " ^ invalid)

module Config : Request.Auth = struct
  let api_key =
    Sys.getenv_opt "OPENAI_API_KEY"
    |> Option.value ~default:"OPENAI_API_KEY not set"

  let org_id = None
end

module API = Open_ai_api.Make (Config)

let free () =
  (* print_endline "freeing all resources"; *)
  Lwt.return ()

let test_lwt (test_case : unit -> bool Lwt.t) switch () =
  Lwt_switch.add_hook (Some switch) free;
  let+ result = test_case () in
  Alcotest.(check bool) "should be true" true result

let test name (test_case : unit -> bool Lwt.t) : unit Alcotest_lwt.test_case =
  Alcotest_lwt.test_case name `Quick (test_lwt test_case)

let list_fine_tune_tests =
  Alcotest_lwt.test_case
    "can do all them fine tunes - except canceling it, or deleting the fine tuned model"
    (* The cancel fine_tune endpoint succeeds only if the fine tune creation is still pending.
       Therefore it is tested separately. *)
    (* The delete_model endpoint is also tested sperately,
       since it might take minutes or hours for a fine tuned model to finish processing.
    *)
    `Quick
    begin
      fun _switch () ->
        let file = "./test_files/fine_tune.jsonl" in
        let purpose = "fine-tune" in

        (* Create the file for fine tune. *)
        let* resp = API.create_file ~file ~purpose in
        let file_id = resp.id in

        (* Create fine tune. *)
        let create_fine_tune_request_t =
          Create_fine_tune_request.create file_id
        in
        let* resp = API.create_fine_tune ~create_fine_tune_request_t in
        let fine_tune_id = resp.id in

        (* Sleep for 10 sec, otherwise deleting file might fail due to the file is still being processed. *)
        Unix.sleep 10;

        (* Test retrieve_fine_tune. *)
        let* resp = API.retrieve_fine_tune ~fine_tune_id in
        Alcotest.(check string) "fine tune id is same" fine_tune_id resp.id;

        (* Test list_fine_tunes. *)
        let* resp = API.list_fine_tunes () in
        Alcotest.(check bool)
          "fine tune id is present"
          true
          (List.exists
             (fun Fine_tune.{ id; _ } -> String.equal fine_tune_id id)
             resp.data);

        (* Test list_fine_tune_events. *)
        let* resp = API.list_fine_tune_events ~fine_tune_id () in
        Alcotest.(check bool)
          "event list is not empty"
          true
          (List.length resp.data > 0);

        (* Tear down. *)
        let* _ = API.delete_file ~file_id in
        Lwt.return_unit
    end

let canel_fine_tune_tests =
  Alcotest_lwt.test_case
    "can canel fine tune test"
    `Quick
    begin
      fun _swtich () ->
        let file = "./test_files/fine_tune.jsonl" in
        let purpose = "fine-tune" in

        (* Create the file for fine tune. *)
        let* resp = API.create_file ~file ~purpose in
        let file_id = resp.id in

        (* Create fine tune. *)
        let create_fine_tune_request_t =
          Create_fine_tune_request.create file_id
        in
        let* resp = API.create_fine_tune ~create_fine_tune_request_t in
        let fine_tune_id = resp.id in

        (* Test cancel fine tune*)
        let* resp = API.cancel_fine_tune ~fine_tune_id in
        Alcotest.(check string) "status is cancelled" "cancelled" resp.status;

        (* Sleep for 10 sec, otherwise deleting file might fail due to the file is still being processed. *)
        Unix.sleep 10;

        (* Tear down. *)
        let* _ = API.delete_file ~file_id in
        Lwt.return_unit
    end


let fine_tune_tests =
  ( "fine tune endpoint tests"
  , [ `Enabled, list_fine_tune_tests; `Disabled, canel_fine_tune_tests ] )


let file_tests =
  Alcotest_lwt.test_case
    "can do all them file tests - except download_file"
    (* download_file is not included since it can requires paid accounts. *)
    `Quick
    begin
      fun _swith () ->
        let file = "./test_files/fine_tune.jsonl" in
        (* The only value allowed in purpose is "fine-tune". *)
        let purpose = "fine-tune" in

        (* Create file. *)
        let* resp = API.create_file ~file ~purpose in
        let file_id = resp.id in

        (* Sleep for 10 sec, otherwise deleting file might fail due to the file is still being processed. *)
        Unix.sleep 10;

        (* Test retrieve_file *)
        let* resp = API.retrieve_file ~file_id in
        Alcotest.(check string) "file id is present" file_id resp.id;

        (* Test list_files *)
        let* resp = API.list_files () in
        Alcotest.(check bool)
          "file list is not empty"
          true
          (List.length resp.data > 0);

        (* Test delete_file *) 
        let+ resp = API.delete_file ~file_id in
        Alcotest.(check bool) "file is deleted" true resp.deleted
    end

let file_tests = "file endpoint tests", [ `Disabled, file_tests ]


let other_endpoint_tests =
  ( "other endpoint tests"
  , [ ( `Disabled
      , test
          "can create_completion"
          begin
            fun () ->
              let create_completion_request_t =
                let req = Create_completion_request.create "ada" in
                let prompt = Some [ "Give me dogs"; "Give me some cats" ] in
                let n = Some 5l in
                { req with prompt; n }
              in
              let+ resp = API.create_completion ~create_completion_request_t in
              List.length resp.choices = 10
          end )
    ; ( `Disabled
      , test
          "can create_edit"
          begin
            fun () ->
              let create_edit_request_t =
                let req =
                  Create_edit_request.create
                    "text-davinci-edit-001"
                    "Fix the spelling mistakes"
                in
                let input = Some "What day of the wek is it?" in
                { req with input }
              in
              let+ resp = API.create_edit ~create_edit_request_t in
              match resp.choices with
              | corrected :: _ ->
                  corrected.text != Some "What day of the wek is it?"
              | [] -> false
          end )
    ; ( `Disabled
      , test
          "can create_embedding"
          begin
            fun () ->
              let create_embedding_request_t =
                let input = [ "Are cats cool or cooler?" ] in
                Create_embedding_request.create "text-embedding-ada-002" input
              in
              let+ resp = API.create_embedding ~create_embedding_request_t in
              List.length resp.data != 0
          end )
    ; ( `Disabled
      , test
          "can create_image"
          begin
            fun () ->
              let create_image_request_t =
                Create_image_request.create
                  "The actor Nicolas Cage standing on a table asking why it \
                   can be misfiled."
              in
              let+ resp = API.create_image ~create_image_request_t in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can create_image_edit"
          begin
            fun () ->
              let image = "./test_files/image_edit_original.png" in
              let prompt = "Add a flamingo to the pool" in
              let mask = "./test_files/image_edit_mask.png" in
              let+ resp = API.create_image_edit ~image ~prompt ~mask () in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can create_image_variation"
          begin
            fun () ->
              let image = "./test_files/image_edit_original.png" in
              let+ resp = API.create_image_variation ~image () in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can create_moderation"
          begin
            fun () ->
              let create_moderation_request_t =
                Create_moderation_request.create
                  [ "I want to kill them or give them cake!" ]
              in
              let+ resp = API.create_moderation ~create_moderation_request_t in
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
              in
              true
          end )
    ; ( `Disabled
      , test
          "can list_models"
          begin
            fun () ->
              let* resp = API.list_models () in
              let* () = Lwt_io.printl "Model ids:" in
              let* () =
                Lwt_list.iter_s
                  (fun (m : Model.t) -> Lwt_io.printl m.id)
                  resp.data
              in
              let+ () = Lwt_io.(flush stdout) in
              List.length resp.data > 0
          end )
    ; ( `Disabled
      , test
          "can retrieve_model"
          begin
            fun () ->
              let+ resp = API.retrieve_model ~model:"text-davinci-003" in
              String.equal resp.id "text-davinci-003"
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
