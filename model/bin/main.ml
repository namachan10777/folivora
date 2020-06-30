let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let () =
    build Key_unit.cherry_mx "cherry_mx.scad";
    build Key_unit.kailh_lp "kailh_lp.scad";
