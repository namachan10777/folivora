#use "./scad_ml/src/scad.ml"

let get_x = function (x, _, _) -> x
let get_y = function (_, y, _) -> y
let get_z = function (_, _, z) -> z

let ( |>> ) src p = Model.translate p src
let ( |@> ) src r = Model.rotate r src
let ( <+> ) = Math.Pos.add
let ( <-> ) = Math.Pos.sub
let ( <*> ) = Math.Pos.mul
let ( </> ) = Math.Pos.div
