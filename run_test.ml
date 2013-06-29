open OUnit
open Test

let suite =
  "All" >::: [
    Test_angle.suite;
  ]

let _ =
  run_test_tt suite
