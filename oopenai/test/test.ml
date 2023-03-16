open Oopenai
open Lwt.Syntax

module Config : Request.Auth = struct
  let api_key = Sys.getenv "OPENAI_API_KEY"
  let org_id = None
end

module API = Open_ai_api.Make(Config)

let free () = print_endline "freeing all resources"; Lwt.return ()

let test_lwt (test_case : bool Lwt.t) switch () =
  Lwt_switch.add_hook (Some switch) free;
  let+ result = test_case in
  Alcotest.(check bool) "should be true" true result

let test name (test_case : bool Lwt.t) : unit Alcotest_lwt.test_case =
  Alcotest_lwt.test_case name `Quick (test_lwt test_case)

let tests =
  [
    test "can create_completion" begin
      let create_completion_request_t = 
        let req = Create_completion_request.create "ada" in 
        let prompt = Some ["Give me dogs"; "Give me some cats"] in
        let n = Some 5l in
        {req with prompt; n}
      in
      let+ resp = API.create_completion ~create_completion_request_t in
      List.length resp.choices = 10
    end;

    test "can create_edit" begin
      let create_edit_request_t =
        let req = Create_edit_request.create "text-davinci-edit-001" "Fix the spelling mistakes" in
        let input = Some "What day of the wek is it?" in 
        {req with input}
      in
      let+ resp = API.create_edit ~create_edit_request_t in 
      match resp.choices with 
      | corrected :: _  -> corrected.text != Some "What day of the wek is it?"
      | [] -> false
    end;

    test "can create_embedding" begin
      let create_embedding_request_t =
        let input = ["Are cats cool or cooler?"] in 
        Create_embedding_request.create "text-embedding-ada-002" input
      in 
      let+ resp = API.create_embedding ~create_embedding_request_t in 
      List.length resp.data != 0
    end;

    test "can create_moderation" begin
      let create_moderation_request_t =
        Create_moderation_request.create ["I want to kill them or give them cake!"]
      in 
      let+ resp =  API.create_moderation ~create_moderation_request_t in 
      List.length resp.results = 0
    end;

  ]

let () =
  Lwt_main.run @@ Alcotest_lwt.run "oopenai tests" [
    "end to end tests", tests
  ]