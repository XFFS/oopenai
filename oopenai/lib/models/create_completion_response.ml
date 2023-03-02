(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    id: string;
    _object: string [@key "object"];
    created: int32;
    model: string;
    choices: Create_completion_response_choices_inner.t list;
    usage: Create_completion_response_usage.t option [@default None];
} [@@deriving yojson { strict = false }, show ];;

let create (id : string) (_object : string) (created : int32) (model : string) (choices : Create_completion_response_choices_inner.t list) : t = {
    id = id;
    _object = _object;
    created = created;
    model = model;
    choices = choices;
    usage = None;
}

