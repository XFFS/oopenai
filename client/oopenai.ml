module Ooo = Oooapi_lib
include Oopenai_gen

module Make (Client : Oooapi_lib.Client) (Config : Oooapi_lib.Config) = struct
  include Make (Client) (Config)
  module Client = Client (Config)
  (* TODO Remove after nicer interface is exposed *)

  (* Override to account for inaccuracy in spec:

     The spec says the media type application/json of type "string",
     but it returns raw, unquoted json data. *)
  let download_file ~file_id () =
    let path = [ "files"; file_id; "content" ] in
    let params = [] in
    let headers = [] in
    (* Leave the data as a string *)
    let decode s = Ok s in
    Client.make_request `GET ~base_url ~path ~params ~headers ~decode
end
