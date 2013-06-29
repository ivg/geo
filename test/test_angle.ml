open OUnit
open Printf
open Angles_common

module A = Angle

let printer = string_of_angle

let compare f sf yesno a a' () =
  let msg = sprintf "Expression (%s %s %s) must %s hold"
    (string_of_angle a)
    sf
    (string_of_angle a')
    (if yesno then "" else "not") in
  assert_equal ~msg yesno (f a a')


let op f r a a' () =
  let r' = f a a' in
  assert_equal ~cmp:Angle.(=) ~printer r r'

let uop f r a () =
  let r' = f a in
  assert_equal ~cmp:Angle.(=) ~printer r r'

let trignometry f fs r a () =
  let r' = (f a) in
  let msg = sprintf  "fs(%s) = %4.4f" (string_of_angle a) r' in
  let d = abs_float (r -. r') in
  let eps = sqrt epsilon_float in
  assert_equal ~msg true (d < eps)

let sum = op A.sum
let difference = op A.difference
let bisector = op A.bisector
let sin = trignometry A.sin
let reverse = uop A.reverse


let suite =
  "Angle">::: [
    "compare:positive-<1"   >:: compare A.(<) "<" true  d0 d15;
    "compare:positive-<2"   >:: compare A.(<) "<" true  d0 d45;
    "compare:positive-<3"   >:: compare A.(<) "<" true  d45 d345;
    "compare:positive-<4"   >:: compare A.(<) "<" false d345 d345;
    "compare:positive-<5"   >:: compare A.(<) "<" false d15 d0;
    "compare:positive-<6"   >:: compare A.(<) "<" false d45 d0;
    "compare:positive-<7"   >:: compare A.(<) "<" false d345 d45;

    "compare:positive-<-eq1"   >:: compare A.(<=) "<=" true  d0 d15;
    "compare:positive-<-eq2"   >:: compare A.(<=) "<=" true  d0 d45;
    "compare:positive-<-eq3"   >:: compare A.(<=) "<=" true  d45 d345;
    "compare:positive-<-eq4"   >:: compare A.(<=) "<=" true  d345 d345;
    "compare:positive-<-eq5"   >:: compare A.(<=) "<=" false d15 d0;
    "compare:positive-<-eq6"   >:: compare A.(<=) "<=" false d45 d0;
    "compare:positive-<-eq7"   >:: compare A.(<=) "<=" false d345 d45;

    "compare:positive->1"   >:: compare A.(>) ">" false  d0 d15;
    "compare:positive->2"   >:: compare A.(>) ">" false  d0 d45;
    "compare:positive->3"   >:: compare A.(>) ">" false  d45 d345;
    "compare:positive->4"   >:: compare A.(>) ">" false  d345 d345;
    "compare:positive->5"   >:: compare A.(>) ">" true   d15 d0;
    "compare:positive->6"   >:: compare A.(>) ">" true   d45 d0;
    "compare:positive->7"   >:: compare A.(>) ">" true   d345 d45;

    "compare:positive->=1"   >:: compare A.(>=) ">=" false  d0 d15;
    "compare:positive->=2"   >:: compare A.(>=) ">=" false  d0 d45;
    "compare:positive->=3"   >:: compare A.(>=) ">=" false  d45 d345;
    "compare:positive->=4"   >:: compare A.(>=) ">=" true  d345 d345;
    "compare:positive->=5"   >:: compare A.(>=) ">=" true d15 d0;
    "compare:positive->=6"   >:: compare A.(>=) ">=" true d45 d0;
    "compare:positive->=7"   >:: compare A.(>=) ">=" true d345 d45;

    "compare:negative-<1"   >:: compare A.(<) "<" true  md0 md15;
    "compare:negative-<2"   >:: compare A.(<) "<" true  md0 md45;
    "compare:negative-<3"   >:: compare A.(<) "<" false md45 md345;
    "compare:negative-<4"   >:: compare A.(<) "<" false md345 md345;
    "compare:negative-<5"   >:: compare A.(<) "<" false md15 md0;
    "compare:negative-<6"   >:: compare A.(<) "<" false md45 md0;
    "compare:negative-<7"   >:: compare A.(<) "<" true  md345 md45;

    "compare:negative-<=1"   >:: compare A.(<=) "<=" true  md0 md15;
    "compare:negative-<=2"   >:: compare A.(<=) "<=" true  md0 md45;
    "compare:negative-<=3"   >:: compare A.(<=) "<=" false md45 md345;
    "compare:negative-<=4"   >:: compare A.(<=) "<=" true md345 md345;
    "compare:negative-<=5"   >:: compare A.(<=) "<=" false md15 md0;
    "compare:negative-<=6"   >:: compare A.(<=) "<=" false md45 md0;
    "compare:negative-<=7"   >:: compare A.(<=) "<=" true  md345 md45;

    "compare:negative->1"   >:: compare A.(>) ">" false  md0 md15;
    "compare:negative->2"   >:: compare A.(>) ">" false  md0 md45;
    "compare:negative->3"   >:: compare A.(>) ">" true   md45 md345;
    "compare:negative->4"   >:: compare A.(>) ">" false  md345 md345;
    "compare:negative->5"   >:: compare A.(>) ">" true   md15 md0;
    "compare:negative->6"   >:: compare A.(>) ">" true   md45 md0;
    "compare:negative->7"   >:: compare A.(>) ">" false   md345 md45;

    "compare:negative->=1"   >:: compare A.(>=) ">=" false  md0 md15;
    "compare:negative->=2"   >:: compare A.(>=) ">=" false  md0 md45;
    "compare:negative->=3"   >:: compare A.(>=) ">=" true   md45 md345;
    "compare:negative->=4"   >:: compare A.(>=) ">=" true   md345 md345;
    "compare:negative->=5"   >:: compare A.(>=) ">=" true   md15 md0;
    "compare:negative->=6"   >:: compare A.(>=) ">=" true   md45 md0;
    "compare:negative->=7"   >:: compare A.(>=) ">=" false  md345 md45;

    "compare:=1"   >:: compare A.(=) "=" true  d0 d0;
    "compare:=2"   >:: compare A.(=) "=" true  d270 md90;
    "compare:=3"   >:: compare A.(=) "=" true  md270 d90;
    "compare:=4"   >:: compare A.(=) "=" true  d0 md0;
    "compare:=5"   >:: compare A.(=) "=" true  md15 d345;
    "compare:=6"   >:: compare A.(=) "=" true  d345 md15;

    "sum1" >:: sum d15 d0 d15;
    "sum2" >:: sum d45 d30 d15;
    "sum3" >:: sum d300 d30 d270;
    "sum4" >:: sum d0 d345 d15;
    "sum5" >:: sum d15 d345 d30;

    "msum1" >:: sum d345 d0 md15;
    "msum2" >:: sum d0 md15 d15;
    "msum3" >:: sum d0 md360 d360;
    "msum4" >:: sum md300 md345 d45;
    "msum5" >:: sum d60 md345 d45;


    "difference1" >:: difference md15 d0 d15;
    "difference2" >:: difference d15 d30 d15;
    "difference3" >:: difference md270 d30 d300;
    "difference3" >:: difference d90 d30 d300;
    "difference4" >:: difference d330 d345 d15;

    "bisector1" >:: bisector d15  d0   d30;
    "bisector2" >:: bisector d315 d270 d0;
    "bisector3" >:: bisector d180 d90 d270;
    "bisector4" >:: bisector d0 d270 d90;
    "bisector5" >:: bisector d315 d270 d0;
    "bisector6" >:: bisector d15 md15 d45 ;
    "bisector6" >:: bisector d15 d345 d45 ;

    "sin" >:: sin "sin" 0.5 d30;
    "cos" >:: sin "cos" 0.5 d30;

    "$>-1" >:: reverse d270 d90;
    "$>-2" >:: reverse d180 d0;
    "$>-3" >:: reverse d90 d270;
  ]
