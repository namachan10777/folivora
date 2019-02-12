#use "./scad_ml/src/scad.ml";;

let w = 17.78
let h = 4.
let d = 7.62

let hole_d = 10.16

module Block = struct
    let block =
        Model.difference
            (Model.cube (d, w, h))
            [
                Model.translate (d /. 2., (w -. hole_d) /. 2., 0.) (Model.cylinder 1.7 4.);
                Model.translate (d /. 2., w -. (w -. hole_d) /. 2., 0.) (Model.cylinder 1.7 4.)
            ]
end
