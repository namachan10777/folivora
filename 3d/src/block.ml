let w = 17.78
let h = 4.
let d = 7.62

let hole_d = 10.16

let rec pow x = function
    | 0 -> 1.
    | n -> x *. pow x (n - 1)

let ( ** ) = pow

module Block = struct
    let block_thick = 5.08
    let slit_width = 2.54 *. 6.0 +. 0.5
    let block_width = 19.0
    let slit_thick = 1.6
    let min_thick = 2.0
    let min_thick_bottom = 1.0

    let slit =
        Model.cube (slit_width, block_thick, slit_thick)
        |> Model.translate (-.slit_width /. 2., 0., 0.)

    let top_cutter =
        Model.cube (50., block_thick, 30.)
        |> Model.translate (-.50. /. 2., 0., 0.)

    let anchorage height tilt =
        let slit_h = height +. slit_width *. (sin tilt) /. 2. +. min_thick_bottom in
        let cutter_h = slit_h +. (slit_thick +. min_thick) /. (cos tilt) +. min_thick_bottom in
        let block_h = height +. slit_width *. (sin tilt) +. slit_thick *. (cos tilt) +. min_thick +. min_thick_bottom in
        Model.difference
            (Model.union [
                (Model.difference
                    (Model.cube (block_width, block_thick, block_h))
                    [top_cutter
                    |> Model.rotate (0., tilt, 0.)
                    |> Model.translate (block_width /. 2., 0., cutter_h)]);
                    (Model.cylinder ~fn:100 ~fs:0.1 (block_thick /. 2.) block_h)
                |> Model.translate (block_width /. 2., block_thick /. 2., 0.)])
            [slit
            |> Model.rotate (0., tilt, 0.)
            |> Model.translate (block_width /. 2., 0., slit_h);
            Model.cylinder ~fn:100 1.6 block_h
            |> Model.translate (block_width /. 2., block_thick /. 2., 0.)]
end
