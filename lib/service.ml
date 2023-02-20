(* https://platform.openai.com/docs/api-reference/authentication *)
module type Auth = sig
  val api_key : string
  val organization : string option
end

type api_response = (Yojson.Basic.t, int * string) Result.t

module type HttpClient = sig
  val request :
       headers:(string * string) list
    -> uri:string
    -> Yojson.Basic.t
    -> api_response Lwt.t
end

module type S = sig
  val get :
       ?url:string
    -> string list
    -> Yojson.Basic.t
    -> api_response Lwt.t
end

module Make (C : HttpClient) (A : Auth) : S = struct
  let headers =
    let org =
      match A.organization with
      | None -> []
      | Some o -> [ ("OpenAI-Organization", o) ]
    in
    ("Authorization", "Bearer " ^ A.api_key) :: org

  let get ?(url = "https://api.openai.com/v1") endpoint data =
    let uri = url ^ "/" ^ (String.concat "/" endpoint) in
    C.request ~headers ~uri data
end

module DefaultClient : HttpClient = struct
  let request ~headers ~uri data =
    let body = Yojson.Basic.to_string data in
    let buff = Buffer.create 256 in
    let receive _req () part = Lwt.return @@ Buffer.add_string buff part in
    let open Lwt.Syntax in
    let config = `HTTP_1_1 Httpaf.Config.default in
    let+ result = Http_lwt_client.request ~config  ~headers ~body uri receive () in
    match result with
    | Error (`Msg m) -> Error (-1, m)
    | Ok (resp, ())  ->
    match resp.status with
    | `OK -> Ok (Yojson.Basic.from_string (Buffer.contents buff))
    | _   -> Error (Http_lwt_client.Status.to_code resp.status, resp.reason)
end

module Default = Make (DefaultClient)
