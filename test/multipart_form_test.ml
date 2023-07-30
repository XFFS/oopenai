module Expect_lwt = struct

  module Lwt_io_run = struct
    type 'a t = 'a Lwt.t
  end

  module Lwt_io_flush = struct
    type 'a t = 'a Lwt.t

    let return x = Lwt.return x
    let bind x ~f = Lwt.bind x f
    let to_run x = x
  end

  module Expect_test_config :
    Expect_test_config_types.S
      with module IO_run = Lwt_io_run
       and module IO_flush = Lwt_io_flush = struct
    module IO_run = Lwt_io_run
    module IO_flush = Lwt_io_flush

    let run x = Lwt_main.run (x ())
    let flushed () = Lwt_io.(buffered stdout = 0)
    let upon_unreleasable_issue = `CR
    let sanitize x = x
  end

  include Lwt.Infix
  include Lwt.Syntax
end

open Oopenai
open Expect_lwt


let example_form =
  let purpose = "fine-tune" in
  let file = "./test_files/test_file.jsonl" in
  let filename = String.split_on_char '/' file |> List.rev |> List.hd in
  let+ file_part = Multipart.part_of_file ~name:"file" ~source:file ~filename in
  Multipart.form
    ~boundary:"TEST_BOUNDARY"
    [ Multipart.part_of_string ~name:"purpose" ~content:purpose; file_part ]

let%expect_test "can generate multipart-form data" =
  let* form = example_form in
  let headers, body = form |> Multipart_form_cohttp.Client.multipart_form in
  let* () =
    print_endline (Cohttp.Header.to_string headers);
    [%expect {|
      Content-Type: multipart/form-data; boundary=TEST_BOUNDARY
       |}]
  in
  let* body = Cohttp_lwt.Body.to_string body in
  print_endline body;
  [%expect
    {|
    --TEST_BOUNDARY
    Content-Disposition: form-data; name="purpose"; 
    
    fine-tune
    --TEST_BOUNDARY
    Content-Disposition: form-data; filename=test_file.jsonl; name="file"; 
    
    {"prompt": "foo", "completion": "bong"}
    {"prompt": "bar", "completion": "bong"}
    {"prompt": "baz", "completion": "bong"}
    {"prompt": "box", "completion": "bong"}
    
    --TEST_BOUNDARY-- |}]
