(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    id: string;
    model: string;
    results: Create_moderation_response_results_inner.t list;
} [@@deriving yojson { strict = false }, show ];;

let create (id : string) (model : string) (results : Create_moderation_response_results_inner.t list) : t = {
    id = id;
    model = model;
    results = results;
}

