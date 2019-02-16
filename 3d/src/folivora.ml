#use "./scad_ml/src/scad.ml"
#use "./src/block.ml"

let alpha = pi/.12.

let () =
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 "folivora.scad" in
    Scad.write oc @@ Block.anchorage 0.0 (alpha *. 1.)
