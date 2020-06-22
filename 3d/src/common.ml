let get_x = function (x, _, _) -> x
let get_y = function (_, y, _) -> y
let get_z = function (_, _, z) -> z

let ( |>> ) src p = Scad.Model.translate p src
let ( |@> ) src r = Scad.Model.rotate r src
let ( <+> ) = Scad.Math.Vec.add
let ( <-> ) = Scad.Math.Vec.sub
let ( <*> ) = Scad.Math.Vec.mul
let ( </> ) = Scad.Math.Vec.div
