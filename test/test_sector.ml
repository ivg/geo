open OUnit
open Printf


open Angles_common

let string_of_sector s =
  let pretty = pretty_angle in
  let b = pretty (S.start s) in
  let e = pretty (S.finish s) in
  sprintf "(%.0f°, %.0f°)" b e


let contain cons yesno b d e() =
  let s = cons b e in
  let r = S.contains d s in
  let msg = sprintf "%s must%sbe contained in %s"
    (string_of_angle d)
    (if yesno then " " else " not ")
    (string_of_sector s) in
  assert_equal ~msg yesno r

let length cons r a a' () =
  let s = cons a a' in
  let msg = sprintf "%s = Length(%s,%s)"
      (string_of_angle r)
      (string_of_angle a)
      (string_of_angle a') in
  assert_equal ~cmp:A.(=) ~msg r (A.of_radian (S.radian_of_length s))

let bisect cons r a a' () =
  let s = cons a a' in
  let msg = sprintf "%s = Bisect(%s,%s)"
      (string_of_angle r)
      (string_of_angle a)
      (string_of_angle a') in
  assert_equal
    ~printer:string_of_angle
    ~cmp:A.(=) ~msg r (S.bisector s)

let of_bisect r r' a a' () =
  let s  = S.of_bisect a a' in
  let t  = S.start s in
  let t' = S.finish s in
  assert_equal
    ~printer:string_of_angle
    ~msg:"start"
    ~cmp:A.(=) r t;
  assert_equal
    ~printer:string_of_angle
    ~msg:"finish"
    ~cmp:A.(=) r' t'

let inc_or_int f rs  cons yesno i i' o o' () =
  let i = cons i i' in
  let o = cons o o' in
  let r = f i o in
  let msg = sprintf "%s must %s %s %s"
    (string_of_sector i)
    (if yesno then "" else "not")
    rs
    (string_of_sector o) in
  assert_equal ~msg yesno r

let includes = inc_or_int S.includes "be insinde"
let intersects = inc_or_int S.intersects "intersects"

type cons_t = A.t -> A.t -> S.t
let union_or_intersection f (cons:cons_t) r r' i i' o o' () =
  let is,os = (cons i i'), (cons o o') in
  let rs =  f is os in
  let p,p' = (S.start rs), (S.finish rs) in
  let printer = string_of_angle in
  let msg = string_of_sector rs in
  let cmp = A.(=) in
  assert_equal ~msg ~cmp ~printer r p;
  assert_equal ~msg ~cmp ~printer r' p'

let union = union_or_intersection S.union
let intersection = union_or_intersection S.intersection



let hor = S.create
let ver = S.create

let suite =
  "Sector">::: [
    "contain:simple-hor1"      >:: contain hor true  d0   d15  d45;
    "contain:simple-hor2"      >:: contain hor true  d15  d30  d45;
    "contain:simple-hor3"      >:: contain hor true  d0   d270 d345;
    "contain:simple-hor1-not"  >:: contain hor false d0   d345 d45;
    "contain:simple-hor2-not"  >:: contain hor false d15  d0   d45;
    "contain:simple-hor3-not"  >:: contain hor false d0   d330 d315;

    "contain:simple-ver1"      >:: contain ver true  d0   d15  d45;
    "contain:simple-ver2"      >:: contain ver true  d15  d30  d45;
    "contain:simple-ver3"      >:: contain ver true  d0   d45  d90;
    "contain:simple-ver1-not"  >:: contain ver false d0   d345 d45;
    "contain:simple-ver2-not"  >:: contain ver false d15  d0   d45;
    "contain:simple-ver3-not"  >:: contain ver false d30  d15  d90;

    "contain:~simple-hor1"     >:: contain hor false  md0   md15  md45;
    "contain:~simple-hor2"     >:: contain hor false  md15  md30  md45;
    "contain:~simple-hor3"     >:: contain hor false  md0   md270 md345;
    "contain:~simple-hor1-not" >:: contain hor true   md0   md345 md45;
    "contain:~simple-hor2-not" >:: contain hor true   md15  md0   md45;
    "contain:~simple-hor3-not" >:: contain hor true   md0   md330 md315;

    "contain:border-hor1"      >:: contain hor true  d0   d15  d15;
    "contain:border-hor1"      >:: contain hor true  d15  d15  d15;
    "contain:border-hor1"      >:: contain hor true  md0   md15  md15;
    "contain:border-hor1"      >:: contain hor true  md15  md15  md15;

    "contain:complex-hor1"      >:: contain hor true  d270  d15  d45;
    "contain:complex-hor2"      >:: contain hor true  d270  d0   d0;
    "contain:complex-hor3"      >:: contain hor true  d45   d15  d30;
    "contain:complex-hor3"      >:: contain hor true  d45   d270  d30;
    "contain:complex-hor3"      >:: contain hor false d45   d30  d15;

    "contain:neg-complex-hor1"  >:: contain hor true  md90  md345  md315;
    "contain:neg-complex-hor1"  >:: contain hor true  md90  md0  md0;
    "contain:neg-complex-hor1"  >:: contain hor true  md315 md345  md330;
    "contain:neg-complex-hor1"  >:: contain hor true  md315 md90  md330;
    "contain:neg-complex-hor1"  >:: contain hor false  md315 md330  md345;

    "length1"  >:: length hor d90 d0 d90;
    "length2"  >:: length hor d270 d90 d0;
    "length3"  >:: length hor d180 d180 d0;
    "length4"  >:: length hor d180 d0 d180;
    "mlength1" >:: length hor d270 md0 md90;
    "mlength2" >:: length hor d90 md90 md0;
    "length1-v"  >:: length ver d90 d0 d90;

    "bisect1"  >:: bisect hor d45  d0    d90;
    "bisect2"  >:: bisect hor d270 d180  d0;
    "bisect3"  >:: bisect hor d180 d90   d270;
    "bisect4"  >:: bisect hor d0   d270  d90;
    "mbisect1" >:: bisect hor d180 md270 d270;
    "mbisect2" >:: bisect hor d270 md90  md90;
    "bisect1-v"  >:: bisect ver d45  d0    d90;

    "of_bisect1"  >:: of_bisect d0 d90 d45 d90;
    "of_bisect2"  >:: of_bisect d270 d90 d0 d180;
    "of_bisect3"  >:: of_bisect d90 d270 d180 d180;
    "of_bisect4"  >:: of_bisect d0 d30 d15 d30;
    "of_bisect4"  >:: of_bisect d345 d45 d15 d60;
    "of_mbisect1"  >:: of_bisect md90 d0 md45 d90;
    "of_mbisect2"  >:: of_bisect md15 md315 md345 d60;

    "includes1" >:: includes hor true d15 d45 d0 d90;
    "includes2" >:: includes hor true d15 d45 d15 d45;
    "includes3" >:: includes hor false d0 d90 d15 d45;
    "includes4" >:: includes hor true d330 d45 d270 d90;
    "includes5" >:: includes hor true md30 d45 md90 d90;
    "includes1-v" >:: includes ver true d15 d45 d0 d90;
    "includes2-v" >:: includes ver true d15 d45 d15 d45;
    "includes3-v" >:: includes ver false d0 d90 d15 d45;

    "intersects1" >:: intersects hor true d15 d45 d0 d90;
    "intersects2" >:: intersects hor true d15 d45 d15 d45;
    "intersects3" >:: intersects hor true d0 d90 d15 d45;
    "intersects4" >:: intersects hor true d330 d45 d270 d90;
    "intersects5" >:: intersects hor true md30 d45 md90 d90;
    "intersects1-v" >:: intersects ver true d15 d45 d0 d90;
    "intersects2-v" >:: intersects ver true d15 d45 d15 d45;
    "intersects3-v" >:: intersects ver true d0 d90 d15 d45;

    "union1" >:: union hor d0 d90 d15 d45 d0 d90;
    "union2" >:: union hor d0 d90 d0 d90  d15 d45;
    "union3" >:: union hor d0 d90 d0 d45  d45 d90;
    "union4" >:: union hor d0 d90 d0 d45  d30 d90;
    "union5" >:: union hor d270 d90 d270 d45  d30 d90;
    "union6" >:: union hor md90 d90 md90 d45  d30 d90;
    "union1-v" >:: union ver d0 d90 d15 d45 d0 d90;
    "union2-v" >:: union ver d0 d90 d0 d90  d15 d45;
    "union3-v" >:: union ver d0 d90 d0 d45  d45 d90;
    "union4-v" >:: union ver d0 d90 d0 d45  d30 d90;

    "intersection1" >:: intersection hor d15 d45 d15 d45 d0 d90;
    "intersection2" >:: intersection hor d15 d45 d0 d90  d15 d45;
    "intersection3" >:: intersection hor d45 d45 d0 d45  d45 d90;
    "intersection4" >:: intersection hor d30 d45 d0 d45  d30 d90;
    "intersection5" >:: intersection hor d30 d45 d270 d45  d30 d90;
    "intersection6" >:: intersection hor d30 d45 md90 d45  d30 d90;

    "intersection1-v" >:: intersection ver d15 d45 d15 d45 d0 d90;
    "intersection2-v" >:: intersection ver d15 d45 d0 d90  d15 d45;
    "intersection3-v" >:: intersection ver d45 d45 d0 d45  d45 d90;
    "intersection4-v" >:: intersection ver d30 d45 d0 d45  d30 d90;
  ]
