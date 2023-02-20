# OOpenAI: An OCaml Library for Open AI's API

## TODO: Document inclusion of Open API spec as git submodule

- How to update submodule
- How to regenerate client bindings

```sh
docker run --rm \
    -v $PWD:/local \
    openapitools/openapi-generator-cli \
    generate \
    -i /local/openai-openapi/openapi.yaml \
    -g ocaml \
    -o /local/oopenapi \
    --skip-validate-spec
```

NOTE: `--skip-validate-spec` required due to
https://github.com/openai/openai-openapi/issues/14
