module M = Scad_ml.Model
open Scad_ml.Util

let t = 2.2

let key_bottom (w, d) (dx, dy) =
    let cube = M.cube (w, d, t) in
    let t = t +. 0.02 in
    let center_hole = M.cylinder 2.4 t in
    let mounting_hole = M.cylinder 0.8 t in
    let pin_hole = M.cylinder 0.6 t in
    let hole_set = M.union [
        center_hole;
        mounting_hole |>> (-5.0, 5.15, 0.);
        pin_hole |>> (0., -5.9, 0.);
        pin_hole |>> (5., -3.8, 0.);
    ] in
    (cube, hole_set |>> (w /. 2. +. dx, d /. 2. +. dy, -0.01))
