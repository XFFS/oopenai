(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    document: int32 option [@default None];
    text: string option [@default None];
    label: string option [@default None];
} [@@deriving yojson { strict = false }, show ];;

let create () : t = {
    document = None;
    text = None;
    label = None;
}

