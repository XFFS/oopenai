open Util.Expect_lwt

let%expect_test "TODO" =
  let* () = Lwt_io.printlf "fii" >>= Lwt_io.flush_all in
  [%expect {| fii |}]
