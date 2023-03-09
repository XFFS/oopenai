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
        let prompt = Some ["Give me dogs"] in
        let n = Some 5l in
        {req with prompt; n}
      in
      let+ resp = API.create_completion ~create_completion_request_t in
      List.length resp.choices = 5
    end;

  ]

let () =
  Lwt_main.run @@ Alcotest_lwt.run "oopenai tests" [
    "end to end tests", tests
  ]