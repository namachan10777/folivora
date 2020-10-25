let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Model.Key_unit.cherry_mx
let pi = Scad_ml.Core.pi

module P = Model.Pad
module M = Scad_ml.Model

let k10 = {
    P.a = (-.pi/.2., pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-1., -11., 27.5);
    P.size = (16., 16., 5.);
}

let k11 = {
    P.a = (0., pi/.12., pi/.30.);
    P.f = cherry_mx;
    P.p = (-3., -1., 4.5);
    P.size = (16., 16., 5.);
}

let k12 = {
    P.a = (pi/.12., pi/.12., pi/.30.);
    P.f = cherry_mx;
    P.p = (-5., 18., 4.5);
    P.size = (16., 16., 5.);
}

let k13 = {
    P.a = (pi/.6., pi/.12., pi/.30.);
    P.f = cherry_mx;
    P.p = (-6., 37., 9.5);
    P.size = (16., 16., 5.);
}

let k20 = {
    P.a = (-.pi/.2., 0., pi/.60.);
    P.f = cherry_mx;
    P.p = (18., -9., 25.);
    P.size = (16., 16., 5.);
}

let k21 = {
    P.a = (0., 0., pi/.60.);
    P.f = cherry_mx;
    P.p = (18., 1., 0.);
    P.size = (16., 16., 5.);
}

let k22 = {
    P.a = (pi/.12., 0., pi/.60.);
    P.f = cherry_mx;
    P.p = (17., 20., 0.);
    P.size = (16., 16., 5.);
}

let k23 = {
    P.a = (pi/.6., 0., pi/.60.);
    P.f = cherry_mx;
    P.p = (16., 39., 5.);
    P.size = (16., 16., 5.);
}

let k30 = {
    P.a = (-.pi/.2., 0., 0.);
    P.f = cherry_mx;
    P.p = (36., -4., 22.);
    P.size = (16., 16., 5.);
}

let k31 = {
    P.a = (0., 0., 0.);
    P.f = cherry_mx;
    P.p = (36., 6., -2.);
    P.size = (16., 16., 5.);
}

let k32 = {
    P.a = (pi/.12., 0., 0.);
    P.f = cherry_mx;
    P.p = (36., 25., -2.);
    P.size = (16., 16., 5.);
}

let k33 = {
    P.a = (pi/.6., 0., 0.);
    P.f = cherry_mx;
    P.p = (36., 44., 3.);
    P.size = (16., 16., 5.);
}

let k40 = {
    P.a = (-.pi/.2., 0., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (54., -8., 25.);
    P.size = (16., 16., 5.);
}


let k41 = {
    P.a = (0., 0., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (54., 2., 0.);
    P.size = (16., 16., 5.);
}

let k42 = {
    P.a = (pi/.12., 0., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (55., 21., 0.);
    P.size = (16., 16., 5.);
}

let k43 = {
    P.a = (pi/.6., 0., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (56., 40., 5.);
    P.size = (16., 16., 5.);
}

let k50 = {
    P.a = (-.pi/.2., 0., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., -13., 31.);
    P.size = (16., 16., 5.);
}

let k51 = {
    P.a = (0., 0., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (75., -3., 6.);
    P.size = (16., 16., 5.);
}

let k52 = {
    P.a = (pi/.12., 0., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (77., 16., 6.);
    P.size = (16., 16., 5.);
}

let k53 = {
    P.a = (pi/.6., 0., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (81., 35., 11.);
    P.size = (16., 16., 5.);
}

let k60 = {
    P.a = (-.pi/.2., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (90., -16., 32.);
    P.size = (16., 16., 5.);
}

let k61 = {
    P.a = (0., -.pi/.20., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (95., -6., 7.);
    P.size = (16., 16., 5.);
}

let k62 = {
    P.a = (pi/.12., -.pi/.20., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (101., 13., 7.);
    P.size = (16., 16., 5.);
}

let k63 = {
    P.a = (pi/.6., -.pi/.20., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (105., 32., 12.);
    P.size = (16., 16., 5.);
}

let mat = [
    [k10; k11; k12; k13];
    [k20; k21; k22; k23];
    [k30; k31; k32; k33];
    [k40; k41; k42; k43];
    [k50; k51; k52];
    [k60; k61; k62];
]

let sub = M.union [
    M.hull [
        P.rside k43;
        P.barfl k52;
    ];
]

let () =
    build (M.union [P.ortho mat; sub]) "pad.scad"
