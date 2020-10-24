let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Model.Key_unit.cherry_mx
let pi = Scad_ml.Core.pi

module P = Model.Pad

let k11 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (0., 0., 0.);
    P.size = (16., 16., 5.);
}

let k12 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (0., 17., 0.);
    P.size = (16., 16., 5.);
}

let k21 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (17., 0., 0.);
    P.size = (16., 16., 5.);
}

let k22 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (17., 17., 0.);
    P.size = (16., 16., 5.);
}

let k31 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (34., 0., 0.);
    P.size = (16., 16., 5.);
}

let k32 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (34., 17., 0.);
    P.size = (16., 16., 5.);
}

let mat = [
    [k11; k12];
    [k21; k22];
    [k31; k32];
]

let () =
    build (P.ortho mat) "pad.scad"
