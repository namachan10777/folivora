module M = Scad_ml.Model
open Core
open Scad_ml.Util

let screwes = [
    (17.78, 31.115);
    (48.26, 45.72);
    (28.26, 2.54);
    (48.26, 2.54);
]

let w = 50.8
let d = 52.705
let pcb_t = 1.8
let plate_t = 2.6
let space_t = 2.0
let rj45 = M.cube (16.51, 18.451, 13.72) |>> (13.335, 33.655, plate_t +. space_t +. pcb_t)
let wire_hole = M.cube (50.8-.34.29, 41.91-.27.94, 50.) |>> (34.29, 27.94, plate_t +. space_t +. pcb_t)

let top =
    M.union []

let hollow =
    M.union ([
        M.cube (w, d, pcb_t +. plate_t +. 2.*.space_t +. 0.001) |>> (0., 0., -0.001);
        rj45;
        wire_hole;
    ] @
    List.map screwes ~f:(fun (x, y) -> M.cylinder 1.6 3.3 ~fn:30 |>> (x, y, pcb_t +. plate_t +. 2.*.space_t -. 0.001)))
