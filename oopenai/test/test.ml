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

let tests =
  [ ( `Disabled
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
        "can create_file"
        begin
          fun () ->
            let file = "./test_files/test_file.jsonl" in
            let purpose = "fine-tune" in
            let+ resp = API.create_file ~file ~purpose in
            String.equal resp.purpose purpose
        end )
  ; ( `Disabled
    , test
        "can create_image"
        begin
          fun () ->
            let create_image_request_t =
              Create_image_request.create
                "The actor Nicolas Cage standing on a table asking why it can \
                 be misfiled."
            in
            let+ resp = API.create_image ~create_image_request_t in
            List.length resp.data > 0
        end )
  ; ( `Enabled
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
        "can delete_file"
        begin
          fun () ->
            let+ resp =
              API.delete_file ~file_id:"file-rkx6H7X8OVgFkmDYDPGBbgOh"
            in
            resp.deleted
        end )
  ; ( `Disabled (* cannot test until using paid account *)
    , test
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
        "can list_files"
        begin
          fun () ->
            let+ resp = API.list_files () in
            List.length resp.data != 0
        end )
  ; ( `Disabled
    , test
        "can retrieve_file"
        begin
          fun () ->
            let+ resp =
              API.retrieve_file ~file_id:"file-rkx6H7X8OVgFkmDYDPGBbgOh"
            in
            String.equal resp.id "file-rkx6H7X8OVgFkmDYDPGBbgOh"
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
  ]

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

let test_suites = [ "end to end tests", tests ]

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
