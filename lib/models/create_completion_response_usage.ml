(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    prompt_tokens: int32;
    completion_tokens: int32;
    total_tokens: int32;
} [@@deriving yojson { strict = false }, show ];;

let create (prompt_tokens : int32) (completion_tokens : int32) (total_tokens : int32) : t = {
    prompt_tokens = prompt_tokens;
    completion_tokens = completion_tokens;
    total_tokens = total_tokens;
}

