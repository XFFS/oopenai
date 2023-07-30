open Multipart_form

let stream_of_file : string -> (string * int * int) Multipart_form.stream =
  let chunk_size = 0x1000 in
  fun filename ->
    let ic = In_channel.open_bin filename in
    let buf = Bytes.create chunk_size in
    fun () ->
      match In_channel.input ic buf 0 chunk_size with
      | exception exn ->
          In_channel.close ic;
          raise exn
      | 0 ->
          In_channel.close ic;
          None
      | len ->
          let str = Bytes.sub_string buf 0 len in
          Some (str, 0, len)

let part_of_file ~name ~source ~filename =
  let disposition = Content_disposition.v name ~filename in
  Lwt.return (part ~disposition (stream_of_file source))

let stream_of_string x =
  let once = ref false in
  let go () =
    if !once then
      None
    else (
      once := true;
      Some (x, 0, String.length x)
    )
  in
  go

let part_of_string ~name ~content =
  let disposition = Content_disposition.v name in
  part ~disposition (stream_of_string content)

let form ?header ?boundary parts =
  let rng ?g:_ _ = Uuidm.(v5 ns_X500 "boundary" |> to_string) in
  Multipart_form.multipart ~rng ?header ?boundary parts
