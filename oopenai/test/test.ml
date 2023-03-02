open Oopenai

let create_completion_request_t = 
  let req = Create_completion_request.create "ada" in 
  let prompt = Some ["Give me dogs"] in
  let n = Some 5l in
  {req with prompt; n}

let () =
  let resp = Lwt_main.run (Open_ai_api.create_completion ~create_completion_request_t) in
  assert ((List.length resp.choices) = 5)
