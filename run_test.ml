open OUnit
open Test



let f a b =
  Printf.printf "bisect(%g°,%g°) = %g°\n"
    (Angle.to_degree a)
    (Angle.to_degree b)
    (Angle.to_degree (Angle.bisector a b))

let _ = f (Angle.of_radian 12.) Angle.(20*deg / 2)


let suite =
  "All" >::: [
    Test_angle.suite;
    Test_sector.suite;
  ]

let _ =
  run_test_tt suite
