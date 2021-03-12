open Scad_ml.Util
module M = Scad_ml.Model
let pi = Scad_ml.Math.pi
let row (x, y) size d = List.init 7 (fun i ->
    let cube, hollow = Model.Key.key_bottom size d in
    let d = (x +. 19.05 *. float_of_int i, y, 0.) in
    (cube |>> d, hollow |>> d)
)

let r1_cube, r1_hollow = Core.List.unzip @@ row (0.0, 19.05 *. 2.) (19.05, 19.05) (0., 0.)
let r2_cube, r2_hollow = Core.List.unzip @@ row (0.0, 19.05 *. 1.) (19.05, 19.05) (0., 0.)
let r3_cube, r3_hollow = Core.List.unzip @@ row (0.0, 19.05 *. 0.) (19.05, 19.05) (0., 0.)
let tilt body =
    body |@> (pi/.40., pi/.40., 0.) |>> (0., 0., 19.05*.7.*. sin (pi/.40.))

let body =
    let cubes = List.flatten [r1_cube; r2_cube; r3_cube] in
    let hollows = List.flatten [r1_hollow; r2_hollow; r3_hollow] in
    let body = M.hull ((M.cube (1.0, 19.5, 2.2) |>> (-11.43, 0., 0.)) :: cubes) in
    let body = tilt body in
    let hollows = tilt @@ M.union hollows in
    let body = M.hull [M.linear_extrude (M.projection body) ~height:0.1; body] in
    M.difference body [hollows]

let build fname body =
    let oc = open_out fname in
    let openscad = Scad_ml.Core.string_of_scad body in
    output_string oc openscad;
    close_out oc

let () =
    build "assembly.scad" body
