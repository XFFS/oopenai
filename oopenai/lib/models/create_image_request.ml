(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    (* A text description of the desired image(s). The maximum length is 1000 characters. *)
    prompt: string;
    (* The number of images to generate. Must be between 1 and 10. *)
    n: int32 [@default 1l];
    (* The size of the generated images. Must be one of `256x256`, `512x512`, or `1024x1024`. *)
    size: Enums.size [@default `_1024x1024];
    (* The format in which the generated images are returned. Must be one of `url` or `b64_json`. *)
    response_format: Enums.response_format [@default `Url];
    (* A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. [Learn more](/docs/guides/safety-best-practices/end-user-ids).  *)
    user: string option [@default None];
} [@@deriving yojson { strict = false }, show ];;

let create ?(n=1l) ?(size=`_1024x1024) ?(response_format=`Url) ?user (prompt : string): t = {
    prompt;
    n;
    size;
    response_format;
    user;
}

