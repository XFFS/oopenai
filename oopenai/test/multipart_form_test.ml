module Expect_lwt = struct
  (* Config for expect tests of Lwt
     see  https://github.com/janestreet/ppx_expect#lwt *)

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

let%expect_test "can generate multipart-form part" =
  Multipart_form.Part.(
    v ~name:"example-part" ~typ:"text/html" "<i>example content</i>"
    |> to_string)
  |> print_endline;
  [%expect
    {|
    Content-Disposition: form-data; name="example-part"
    Content-Type: text/html

    <i>example content</i> |}]

let example_form =
  let purpose = "fine-tune" in
  let file = "./test_files/test_file.jsonl" in
  let filename = String.split_on_char '/' file |> List.rev |> List.hd in
  let+ content = Lwt_io.(with_file ~mode:Input file read) in
  Multipart_form.(
    v
      ~boundary:"TEST_BOUNDARY"
      Part.
        [ v ~name:"purpose" purpose
        ; v ~name:"file" ~typ:"application/json" ~filename content
        ])

let%expect_test "can generate multipart-form data" =
  let* form = example_form in
  form |> Multipart_form.to_string |> print_endline;
  [%expect
    {|
    --TEST_BOUNDARY
    Content-Disposition: form-data; name="purpose"

    fine-tune
    --TEST_BOUNDARY
    Content-Disposition: form-data; name="file"; filename="test_file.jsonl"
    Content-Type: application/json

    {"prompt": "foo", "completion": "bong"}
    {"prompt": "bar", "completion": "bong"}
    {"prompt": "baz", "completion": "bong"}
    {"prompt": "box", "completion": "bong"}
    --TEST_BOUNDARY-- |}]

let%expect_test "can add header to Cohttp headers" =
  let* form = example_form in
  Cohttp.Header.init ()
  |> Multipart_form.add_header form
  |> Cohttp.Header.to_string
  |> print_endline;
  [%expect
    {|
    Content-type: multipart/form-data; boundary=TEST_BOUNDARY
     |}]
