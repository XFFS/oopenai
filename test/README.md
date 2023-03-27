# Tests

## End-to-end Tests

### Verbosity

The verbosity of test output can be set by the `VERBOSITY` flag, which supports
three values:

- `quiet`: just report test results
- `verbose`: show logging from the HTTP client and core business logic, and
  stdout for all tests
- `debug`: set all loggers to debug (this produces a lot of output!)

The default is `quiet`.

E.g.,

``` sh
VERBOSITY=verbose dune test
```


### Disabled/Enabled

To run all end-to-end tests, even those that are marked as `Disabled`, run with

``` sh
RUN_DISABLED=true dune test
```
