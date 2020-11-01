let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad_ml.Util.write oc scad;
    close_out oc

let key_size = (19.0, 19.0, 5.0)

let cherry_mx = Model.Key_unit.cherry_mx
let pi = Scad_ml.Core.pi

module P = Model.Pad
module M = Scad_ml.Model
open Core
open Scad_ml.Util

let cylinder_top = M.cylinder 3.0 3.0 ~center:true
let cylinder_bottom = M.cylinder 3.0 3.0 ~center:true
    |>> (0., 0., -.3.)

let k100 = {
    P.a = (0., pi/.20. +. pi/.20., pi/.30.);
    P.f = Model.Key_unit.dummy;
    P.p = (1., -20., 47.5);
    P.size = (0., 0., 0.);
}

let k00 = {
    P.a = (-.pi/.5., pi/.20. +. pi/.20., pi/.30.);
    P.f = Model.Key_unit.dummy;
    P.p = (-6., -14., 31.5);
    P.size = (0.001, 6., 6.);
}

let k10 = {
    P.a = (-.pi/.5., pi/.20. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (1., -21., 35.5);
    P.size = (17., 17., 6.);
}

let k11 = {
    P.a = (0., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-7., -1., 24.5);
    P.size = (17., 17., 6.);
}

let k12 = {
    P.a = (pi/.12., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-9., 18., 24.5);
    P.size = (17., 17., 6.);
}

let k03 = {
    P.a = (pi/.6., pi/.12. +. pi/.20., pi/.30.);
    P.f = Model.Key_unit.dummy;
    P.p = (-16., 40., 29.5);
    P.size = (0.001, 5., 6.);
}

let k13 = {
    P.a = (pi/.6., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx;
    P.p = (-9., 37., 29.5);
    P.size = (17., 17., 6.);
}

let k200 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = Model.Key_unit.dummy;
    P.p = (22., -25., 30.5);
    P.size = (5., 5., 6.);
}


let k20 = {
    P.a = (-.pi/.5., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (18., -20., 31.);
    P.size = (17., 17., 6.);
}

let k21 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (14., 1., 16.);
    P.size = (17., 17., 6.);
}

let k22 = {
    P.a = (pi/.12., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (12.5, 20., 16.);
    P.size = (17., 17., 6.);
}

let k23 = {
    P.a = (pi/.6., pi/.20., pi/.60.);
    P.f = cherry_mx;
    P.p = (12., 39., 21.);
    P.size = (17., 17., 6.);
}

let kp = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (45., -67., 19.0);
    P.size = (17., 17., 6.);
}

let kp_sc = {
    P.a = (0., pi/.20., 0.);
    P.f = Model.Key_unit.dummy;
    P.p = (70., -57., 17.0);
    P.size = (0.001, 4., 6.);
}

let k300 = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (36., -37., 25.0);
    P.size = (17., 17., 6.);
}

let k30 = {
    P.a = (-.pi/.5., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (36., -16., 24.);
    P.size = (17., 17., 6.);
}

let k31 = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (31., 6., 10.);
    P.size = (17., 17., 6.);
}

let k32 = {
    P.a = (pi/.12., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (31., 25., 10.);
    P.size = (17., 17., 6.);
}

let k33 = {
    P.a = (pi/.6., pi/.20., 0.);
    P.f = cherry_mx;
    P.p = (31.5, 44., 15.);
    P.size = (17., 17., 6.);
}

let k400 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (54., -42., 23.5);
    P.size = (17., 17., 6.);
}

let k40 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (54., -20., 21.);
    P.size = (17., 17., 6.);
}


let k41 = {
    P.a = (0., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (50., 2., 8.);
    P.size = (17., 17., 6.);
}

let k42 = {
    P.a = (pi/.12., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (51., 21., 8.);
    P.size = (17., 17., 6.);
}

let k43 = {
    P.a = (pi/.6., pi/.20., -.pi/.60.);
    P.f = cherry_mx;
    P.p = (52., 40., 13.);
    P.size = (17., 17., 6.);
}

let k500 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., -39., 20.5);
    P.size = (17., 17., 6.);
}

let k50 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., -23., 22.);
    P.size = (17., 17., 6.);
}

let k51 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (71., -3., 10.);
    P.size = (17., 17., 6.);
}

let k52 = {
    P.a = (pi/.12., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (73., 16., 10.);
    P.size = (17., 17., 6.);
}

let k53 = {
    P.a = (pi/.6., pi/.20., -.pi/.30.);
    P.f = cherry_mx;
    P.p = (77., 35., 15.);
    P.size = (17., 17., 6.);
}

let k600 = {
    P.a = (0., pi/.20., -.pi/.10.);
    P.f = Model.Key_unit.dummy;
    P.p = (100., -36., 17.);
    P.size = (0.001, 5., 6.);
}
let k60 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (90., -26., 20.);
    P.size = (17., 17., 6.);
}
let k70 = {
    P.a = (-.pi/.10., pi/.20., -.pi/.10.);
    P.f = Model.Key_unit.dummy;
    P.p = (115., -26., 13.);
    P.size = (0.001, 5., 6.);
}

let k61 = {
    P.a = (0., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (95., -6., 7.);
    P.size = (17., 17., 6.);
}

let k62 = {
    P.a = (pi/.12., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (101., 13., 7.);
    P.size = (17., 17., 6.);
}

let k72 = {
    P.a = (pi/.12., 0., -.pi/.10.);
    P.f = Model.Key_unit.dummy;
    P.p = (126., 13., 7.);
    P.size = (0.001, 5., 6.);
}

let k63 = {
    P.a = (pi/.6., 0., -.pi/.10.);
    P.f = cherry_mx;
    P.p = (105., 32., 12.);
    P.size = (17., 17., 6.);
}

open Scad_ml.Util

let under_cover d t f k =
    let open Model.Pad in
    {
        P.a = k.a;
        P.f = f;
        P.size = (get_x k.size, get_y k.size, t);
        P.p = (k.p <+> (Model.Matrix.trans (Model.Matrix.rot k.a) (0., 0., -.d -. t)));
    }


let gen_cover t conf =
    conf
    |> List.map ~f:(List.map ~f:(Option.map ~f:(under_cover 3.5 t Model.Key_unit.dummy)))

let sub = M.union [
    M.hull [
        P.rside k43;
        P.barfl k52;
    ];
]

module Patch = Model.Patch
let screw1 = {
    Patch.a = (pi/.6., pi/.15., pi/.60.);
    Patch.p = (9.2, 39., 21.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw2 = {
    Patch.a = (0., pi/.20., pi/.60.);
    Patch.p = (11.2, 4., 16.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw3 = {
    Patch.a = (pi/.12., pi/.20., -.pi/.60.);
    Patch.p = (50., 32., 13.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
}

let screw4 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (69.5, 10., 9.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
}


let screw5 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (92., -3., 9.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
}


let screw6 = {
    Patch.a = (pi/.12., pi/.20., -.pi/.30.);
    Patch.p = (97., 24., 12.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =4.5;
}

let screw7 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (35., -57., 20.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw8 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (35., -47., 20.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =7.5;
}

let screw9 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (46., -17., 23.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =5.5;
}


let screw10 = {
    Patch.a = (0., pi/.20., -.pi/.30.);
    Patch.p = (64., -22., 20.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =5.5;
}

let screw_set = [
    Patch.Screw screw1; Patch.Screw screw2;
    Patch.Screw screw3;
    Patch.Screw screw4;
    Patch.Screw screw5; Patch.Screw screw6;
    Patch.Screw screw7;
    Patch.Screw screw8;
    Patch.Screw screw9;
    Patch.Screw screw10;
]


let kt02 = {
    P.a = (0., -.1.*.pi/.2., pi/.6.);
    P.f = cherry_mx;
    P.p = (20., -37., 0.);
    P.size = (0.001, 8., 6.);
}

let kt10 = {
    P.a = (0., -.1.*.pi/.2., 0.);
    P.f = cherry_mx;
    P.p = (21., -66., 2.);
    P.size = (5., 0.001, 6.);
}

let kt11 = {
    P.a = (0., -.1.*.pi/.2., 0.);
    P.f = cherry_mx;
    P.p = (21., -61., 2.);
    P.size = (17., 17., 6.);
}

let kt12 = {
    P.a = (0., -.1.*.pi/.2.,  pi/.6.);
    P.f = cherry_mx;
    P.p = (20., -37., 6.);
    P.size = (17., 17., 6.);
}

let kt20 = {
    P.a = (0., -.4.*.pi/.7., 0.);
    P.f = cherry_mx;
    P.p = (21., -66., 32.);
    P.size = (5., 0.001, 6.);
}

let kt21 = {
    P.a = (0., -.4.*.pi/.7., 0.);
    P.f = cherry_mx;
    P.p = (21., -61., 22.);
    P.size = (17., 17., 6.);
}

let kt22 = {
    P.a = (0., -.4.*.pi/.7., pi/.6.);
    P.f = cherry_mx;
    P.p = (20., -37., 26.);
    P.size = (17., 17., 6.);
}

let kt32 = {
    P.a = (0., -.4.*.pi/.7., pi/.6.);
    P.f = cherry_mx;
    P.p = (15., -37., 46.);
    P.size = (00.01, 5., 6.);
}

let tmat = [
    [None; None; Some(kt02); None;       ];
    [Some(kt10); Some(kt11); Some(kt12); None];
    [Some(kt20); Some(kt21); Some(kt22); None];
    [None; None; Some(kt32);       None];
]

let mat = [
    [None;        None;      Some(k00); None;      None;      Some(k03); None];
    [None;        None;      Some(k10); Some(k11); Some(k12); Some(k13); None];
    [None;        Some(k200);Some(k20); Some(k21); Some(k22); Some(k23); None];
    [None;        Some(k300);Some(k30); Some(k31); Some(k32); Some(k33); None];
    [Some(kp);    Some(k400);Some(k40); Some(k41); Some(k42); Some(k43); None];
    [Some(kp_sc); Some(k500);Some(k50); Some(k51); Some(k52); None;      None];
    [None;        Some(k600);Some(k60); Some(k61); Some(k62); None;      None];
    [None;        None;      Some(k70); None;      Some(k72); None;      None];
]

let thumb = 
    Patch.apply_patches
        { Patch.target = Patch.Bottom; }
        (M.union [P.ortho tmat])
        []

let thumb_cover = gen_cover 3.5 tmat
let idx pad c r = List.nth_exn (List.nth_exn pad c) r
let tcover11 = idx thumb_cover 1 1
let tcover12 = idx thumb_cover 1 2
let tcover21 = idx thumb_cover 2 1
let tcover22 = idx thumb_cover 2 2

let top =
    Patch.apply_patches
        { Patch.target = Patch.Top; }
        (M.union [
            P.ortho mat;
            P.ortho tmat;
            sub;
            P.ortho @@ gen_cover 3.5 [[Some(kt21);Some(kt22);];[None; Some(kt32)];];
            M.hull [
                P.bnside @@ P.lhalf @@ Option.value_exn tcover22;
                P.bfside @@ P.lhalf @@ Option.value_exn tcover21;
                P.barnl k300;
            ];
            M.hull [
                P.bottom @@ P.lhalf @@ Option.value_exn tcover21;
                P.barnl k300;
                P.barfl kp;
            ];
            M.hull [
                P.bnside @@ P.lhalf @@ Option.value_exn tcover21;
                P.lside kp;
            ];
            M.hull [
                P.barfl kp;
                P.nside k300;
            ];
            M.hull [
                P.barnr k300;
                P.barnl k400;
                P.fside kp;
            ];
        ])
        screw_set


let dummy_key = {
    P.a = (0., 0., 0.);
    P.f = Model.Key_unit.dummy;
    P.p = (0., 0., 0.);
    P.size = (0., 0., 0.);
}

let bottom =
    let mcover c r = idx (gen_cover 0.5 mat) c r |> Option.value ~default:dummy_key in
    let tcover c r = idx (gen_cover 3.5 tmat) c r |> Option.value ~default:dummy_key in
    let base = Patch.apply_patches
        { Patch.target = Patch.Bottom; }
        (M.union [
            P.ortho @@ gen_cover 0.5 mat;
            P.proj @@ gen_cover 0.5 mat;
            M.hull [
                P.pbarfl @@ mcover 4 0;
                P.pnside @@ mcover 3 1;
                P.pbarnl @@ mcover 4 1;
                P.barfl @@ mcover 4 0;
                P.nside @@ mcover 3 1;
                P.barnl @@ mcover 4 1;
            ];
            M.hull[
                P.barnr @@ mcover 2 2;
                P.barnl @@ mcover 3 2;
                P.barfl @@ mcover 3 1;
            ];
            M.hull[
                P.pbarnr @@ mcover 2 2;
                P.pbarnl @@ mcover 3 2;
                P.pbarfl @@ mcover 3 1;
            ];
            M.hull [
                P.plside @@ mcover 3 1;
                P.pnside @@ mcover 2 2;
                P.barfl @@ mcover 3 1;
                P.ortho @@ gen_cover 3.5 [[Some(kt12);]];
            ];
            M.hull [
                P.ortho @@ gen_cover 3.5 [[Some(kt11);]];
                P.plside @@ mcover 4 0;
                P.lside @@ mcover 4 0;
            ];
            M.hull [
                P.fside @@ tcover 1 1;
                P.nside @@ tcover 1 2;
                P.pbarnl @@ mcover 3 1;
                P.pbarfl @@ mcover 4 0;
            ];
        ])
        screw_set
    in M.union [
        M.difference
            (M.union [
                base;
            ]) [
            Pcbmod.hollow |>> (-24.5, -17., 0.);
            ];
        Pcbmod.top |>> (-24.5, -17., 0.);
    ]

let () =
    build (M.union [bottom; top]) "pad.scad";
    build (M.union [Pcbmod.top]) "pcbmod.scad";
