module Part = struct
  type t =
    { disposition : string
    ; typ : string option
    ; content : string
    }

  let v ~name ?filename ?typ content =
    let disposition =
      let filename =
        match filename with
        | None -> []
        | Some n -> [ Printf.sprintf {|filename="%s"|} n ]
      in
      [ "Content-Disposition: form-data"; Printf.sprintf {|name="%s"|} name ]
      @ filename
      |> String.concat "; "
    in
    let typ = Option.map (Printf.sprintf "Content-Type: %s") typ in
    { disposition; typ; content }

  (* Add a new line to a buffer *)
  let ends_with_nl s =
    let last_char = String.(get s (length s - 1)) in
    Char.(equal last_char '\n')

  let nl buf = Buffer.add_char buf '\n'

  let add_nl buf s =
    Buffer.add_string buf s;
    nl buf

  let add_to_buffer buf v =
    let add_nl = add_nl buf in
    v.disposition |> add_nl;
    (* only added if v.typ is `Some t` *)
    v.typ |> Option.iter add_nl;
    (* an empty line to separate *)
    "" |> add_nl;
    (* ensure our content ends with a newline *)
    if ends_with_nl v.content then
      v.content |> Buffer.add_string buf
    else
      v.content |> add_nl

  let to_string v =
    let buf = Buffer.create 64 in
    add_to_buffer buf v;
    Buffer.contents buf
end

type t =
  { boundary : string
  ; parts : Part.t list
  }

let v ?boundary parts =
  let boundary =
    match boundary with
    | Some b -> b
    | None -> Uuidm.(v5 ns_X500 "PartBoundary" |> to_string)
  in
  { boundary; parts }

let to_string v =
  let boundary = "--" ^ v.boundary in
  let terminal = boundary ^ "--" in
  let buf = Buffer.create 64 in
  let add = Buffer.add_string buf in
  v.parts
  |> List.iter (fun p ->
         boundary |> add;
         "\n" |> add;
         p |> Part.add_to_buffer buf);
  terminal |> add;
  Buffer.contents buf

let add_header v headers =
  let typ = Printf.sprintf "multipart/form-data; boundary=%s" v.boundary in
  Cohttp.Header.add headers "Content-type" typ
