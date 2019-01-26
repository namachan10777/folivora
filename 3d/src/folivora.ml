#use "./scad_ml/src/scad.ml"

let model = Model.cube (10.0, 10.0, 10.0)

let () =
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 "folivora.scad" in
    Scad.write oc model
