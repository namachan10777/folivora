#use "./scad_ml/src/scad.ml"
#use "./src/key.ml"

let () =
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 "folivora.scad" in
    Scad.write oc Key.key_elm
