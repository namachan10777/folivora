#use "./src/common.ml"
#use "./src/key.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

let () =
    build (Key.kailh_lp (21., 18., 5.)) "key.scad"
