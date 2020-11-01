module M = Scad_ml.Model
open Core
open Scad_ml.Util

let screwes = [
    (2.54, 2.54, 0.);
    (10.795, 2.54, 0.);
    (41.91, 2.54, 0.);
    (2.54, 45.72, 0.);
    (10.795, 45.72, 0.);
    (2.54, 72.39, 0.);
    (41.91, 72.39, 0.);
]

let w = 44.45
let d = 74.93
let t = 6.
let pcb_t = 1.8
let plate_t = 2.0
let space_t = 2.0
let rj45 = M.cube (18.451, 16.51, 13.72) |>> (0., 48.26, plate_t +. space_t +. pcb_t)
let oled = M.cube (10.24, 2.54, 40.) |>> (1.27, 7.62, 0.)
let top_base = plate_t +. space_t
let wire_hole = M.cube (24.13, 39.38, 50.) |>> (20.32, 8.89, top_base)

let top =
    M.difference
        (M.union [
            M.cube (w, d, t) |>> (0., 0.,top_base +. pcb_t +. space_t);
            M.cube (4.445, 2.54, 5.8) |>> (27.94, 0., top_base);
            M.cube (4.445, 2.54, 5.8) |>> (27.94, d -. 2.54, top_base);
            screwes
            |> List.map
                ~f:(fun p -> M.cube (5.08, 5.08, 2.0) |>> (p <+> (-2.54, -2.54, top_base +. pcb_t)))
            |> M.union;
        ])
        [
            wire_hole;
            rj45;
            oled;
            M.cube (12.0, 38.0, 30.) |>> ((12.0 -. 10.16) /. 2., 7.62 -. 1.54, top_base +. space_t +. 4.5);
            M.cube (12.78, 53.34, 30.) |>> ((12.0 -. 10.16) /. 2., 0., top_base +. space_t +. 4.5 +. 2.6);
            screwes
            |> List.map ~f:(fun p -> M.cylinder 1.1 18.0 ~fn:30 |>> (p <+> (0., 0., top_base +. pcb_t)))
            |> M.union;
            screwes
            |> List.map ~f:(fun p -> M.cylinder 1.5 3.3 ~fn:30 |>> (p <+> (0., 0., top_base +. pcb_t)))
            |> M.union;
        ]

let hollow =
    M.union [
        M.cube (w, d, pcb_t +. plate_t +. 2. *. space_t);
        oled;
        rj45;
        wire_hole;
    ]
