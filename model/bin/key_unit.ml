module M = Scad_ml.Model
open Scad_ml.Util

let unit_size = 19.0

let cherry_mx =
    let centered (w, d, h) =
        M.cube (w, d, h) |>> (-.w/.2., -.d/.2., 0.0) in
    let plate_t = 1.4 in
    let offset_from_bottom = 5.0 in
    let plate_hole_s = 14.0 in
    let unit_height = 7.0 in
    let max_s = 15.6 in
    M.difference
        (centered (unit_size, unit_size, unit_height))
        [
            centered (plate_hole_s, plate_hole_s, plate_t)
                |>> (0., 0., (offset_from_bottom -. plate_t));
            centered (plate_hole_s, max_s, offset_from_bottom -. plate_t);
            centered (plate_hole_s, max_s, unit_height -. offset_from_bottom)
                |>> (0., 0., offset_from_bottom);
        ]
