(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    tokens: string list;
    token_logprobs: float list;
    top_logprobs: Yojson.Safe.t list;
    text_offset: int32 list;
} [@@deriving yojson { strict = false }, show ];;

let create () : t = {
    tokens = [];
    token_logprobs = [];
    top_logprobs = [];
    text_offset = [];
}

