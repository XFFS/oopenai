(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    _object: string [@key "object"];
    data: Model.t list;
} [@@deriving yojson { strict = false }, show ];;

let create (_object : string) (data : Model.t list) : t = {
    _object = _object;
    data = data;
}

