module M = Scad_ml.Model
open Scad_ml.Util

let cherry_mx (dx, dy) (w, d, h) =
    let lump = M.cube (w, d, h) in
    M.difference lump [
        M.cube (14., 14., h +. 0.02)
        |>> ((w -. 14.)/.2. +. dx, (d -. 14.)/.2. +. dy, -0.01);
    ]

let dummy = M.cube
