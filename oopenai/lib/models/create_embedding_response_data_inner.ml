(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    index: int32;
    _object: string;
    embedding: float list;
} [@@deriving yojson { strict = false }, show ];;

let create (index : int32) (_object : string) (embedding : float list) : t = {
    index = index;
    _object = _object;
    embedding = embedding;
}
