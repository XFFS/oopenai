# OOpenAI: An OCaml Client Library for OpenAI API

Authors: Shon Feder, Freda Xin  
<br/>
## About 
This is an unofficial client library for the [OpenAI API](https://platform.openai.com/docs/api-reference) implemented in OCaml. The library was generated initially by the OCamlClientCodegen code generator for [openapi-generator](https://openapi-generator.tech). However, due to various errors in the code generation, a significant portion of this library was manually implemented.

<br/>

## Releases
The current release (V.1.0) covers the following endpoints.

| Endpoint                 | Release V.1.0 |
| ------------------------| -----------|
| Cancel fine tune        |      ✅     |
| Create chat completion  |             |
| Create completion       |      ✅     |
| Create edit             |      ✅     |
| Create embedding        |      ✅     |
| Create file             |      ✅     |
| Create fine tune        |      ✅     |
| Create image            |      ✅     |
| Create image edit       |      ✅     |
| Create image variation  |      ✅     |
| Create moderation       |      ✅     |
| Create translation      |             |
| Create transcription    |             |
| Delete file             |      ✅     |
| Delete model            |      ✅     |
| Download file           |      ✅     |
| List files              |      ✅     |
| List fine tune events   |      ✅     |
| List fine tunes         |      ✅     |
| List models             |      ✅     |
| Retrieve file           |      ✅     |
| Retrieve fine tune      |      ✅     |
| Retrieve model          |      ✅     |

<br/>

## Installation
```sh
opam install https://github.com/XFFS/oopenai.git
```

<br/>

## Usage 
Obtain your secret key from [OpenAI](https://platform.openai.com/account/api-keys). Set the key as environment variable `OPENAI_API_KEY`.


```ocaml
open Oopenai

module Config : Request.Auth = struct
  let api_key = Sys.getenv "OPENAI_API_KEY"
  let org_id = None
end

module API = Open_ai_api.Make (Config)


(* List models *)
let resp = API.list_models () in 
(*TODO: Print out model ids *) 


(* Create modereation *)
let create_completion_request_t =
    let req = Create_completion_request.create "ada" in
    let prompt = Some [ "Give me dogs"; "Give me some cats" ] in
    let n = Some 5l in
{ req with prompt; n }
in
let resp = API.create_completion ~create_completion_request_t in 
let () = Printf.printf resp.choices
```

For more examples of how to use the endpoints, refer to the end-to-end [tests](https://github.com/shonfeder/oopenai/blob/4606763928b37095cd9db353b18a178fc1810a2b/oopenai/test/test.ml). 