(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    (* ID of the model to use. You can use the [List models](/docs/api-reference/models/list) API to see all of your available models, or see our [Model overview](/docs/models/overview) for descriptions of them. *)
    model: string;
    (* The input text to use as a starting point for the edit. *)
    input: string option [@default None];
    (* The instruction that tells the model how to edit the prompt. *)
    instruction: string;
    (* How many edits to generate for the input and instruction. *)
    n: int32 option [@default None];
    (* What [sampling temperature](https://towardsdatascience.com/how-to-sample-from-language-models-682bceb97277) to use. Higher values means the model will take more risks. Try 0.9 for more creative applications, and 0 (argmax sampling) for ones with a well-defined answer.  We generally recommend altering this or `top_p` but not both.  *)
    temperature: float option [@default None];
    (* An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.  We generally recommend altering this or `temperature` but not both.  *)
    top_p: float option [@default None];
} [@@deriving yojson { strict = false }, show ];;

let create (model : string) (instruction : string) : t = {
    model = model;
    input = None;
    instruction = instruction;
    n = None;
    temperature = None;
    top_p = None;
}

