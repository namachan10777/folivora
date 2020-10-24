let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Model.Key_unit.cherry_mx
let pi = Scad_ml.Core.pi
