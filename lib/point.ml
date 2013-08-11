module A = Angle
type a = A.t
type t = a * a

type datum = {
  radius : float;
  flattening: float;
}

let wgs84 = {
  radius = 6378137.0;
  flattening = 298.257223563;
}

let kras1940 = {
  radius = 6378245.0;
  flattening = 298.3;
}

let mean_earth_radius = 6371008.77141505945
let earth_radius = mean_earth_radius

let create ~lat ~lon = lat, lon

let of_radians ~lat ~lon =
  A.of_radian lat,
  A.of_radian lon

let of_degrees ~lat ~lon =
  A.of_degree lat,
  A.of_degree lon


let latitude  (lat, _) = lat
let longitude (_, lon) = lon

let degree_of_latitude  (lat, _) = A.to_degree lat
let degree_of_longitude (_, lon) = A.to_degree lon
let radian_of_latitude  (lat, _) = A.to_radian lat
let radian_of_longitude (_, lon) = A.to_radian lon


let round n = int_of_float (floor (n +. 1.))
let string_of_dms (d,m,s) = Printf.sprintf "% 02d°%d'%02.2f″" d m s
let string_of_dmS (d,m,s) = Printf.sprintf "% 02d°%02d'%02d″" d m (round s)


let strfmt fmt p =
  let b = Buffer.create 32 in
  let sub = function
    | "dlat" -> string_of_float (degree_of_latitude p)
    | "rlat" -> string_of_float (radian_of_latitude p)
    | "dlon" -> string_of_float (degree_of_longitude p)
    | "rlon" -> string_of_float (radian_of_longitude p)
    | "dmslat" -> string_of_dms (A.to_dms (latitude p))
    | "dmslon" -> string_of_dms (A.to_dms (longitude p))
    | "dmSlat" -> string_of_dmS (A.to_dms (latitude p))
    | "dmSlon" -> string_of_dmS (A.to_dms (longitude p))
    | s -> invalid_arg ("Geo.strfmt: unknown variable " ^ s) in
  Buffer.add_substitute b sub fmt;
  Buffer.contents b


let bearing f t =
  let flat = radian_of_latitude  f in
  let flon = radian_of_longitude f in
  let tlat = radian_of_latitude  t in
  let tlon = radian_of_longitude t in
  let ldif =  tlon -. flon in
  let y = (sin ldif) *. (cos tlat) in
  let x =
    (cos flat) *. (sin tlat) -.
      (sin flat) *. (cos tlat) *. (cos ldif) in
  A.of_radian (atan2 y x)

let distance f t =
  let flat = radian_of_latitude  f in
  let flon = radian_of_longitude f in
  let tlat = radian_of_latitude  t in
  let tlon = radian_of_longitude t in
  let lond = tlon -. flon in
  let latd = tlat -. flat in
  let t =
    ((sin (latd /. 2.)) ** 2.) +.
      ((sin (lond /. 2.)) ** 2.) *.
      (cos flat) *. (cos tlat) in
  2. *. mean_earth_radius *. (atan2 (sqrt t) (sqrt(1. -. t)))

let destination azth dist f =
  let flat = radian_of_latitude  f in
  let flon = radian_of_longitude f in
  let azth = A.to_radian azth in
  let ad = dist /. mean_earth_radius in
  let tlat =
    asin ((sin flat) *. (cos ad) +.
             (cos flat) *. (sin ad) *. (cos azth)) in
  let tlon =
    flon +. (atan2
               ((sin azth) *. (sin ad) *. (cos flat))
               ((cos ad) -. (sin flat) *. (sin tlat))) in
  of_radians tlat tlon


let to_mercator ?datum:(d=wgs84) (lat,lon) =
  let arth x = 0.5 *. log ((1. +. x) /. (1. -. x)) in
  let lat,lon = A.to_radian lat, A.to_radian lon in
  let f = d.flattening and r = d.radius in
  let b = (f *. r -. r) /. f in
  let e = sqrt (r**2. -. b**2.) /. r in
  let v1 = arth (sin lat) and v2 = e *. arth (e *. sin lat) in
  lon *. r, (v1 -. v2) *.r
