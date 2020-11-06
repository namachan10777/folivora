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

let k11 = {
    P.a = (-.pi/.5., pi/.20. +. pi/.20., pi/.30.);
    P.f = cherry_mx (1.5, 1.5);
    P.p = (-2., -23., 37.5);
    P.size = (20., 20., 6.);
}

let k12 = {
    P.a = (0., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx (1.5, 0.);
    P.p = (-10., -1., 25.5);
    P.size = (20., 17., 6.);
}

let k13 = {
    P.a = (pi/.12., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx (1.5, 0.);
    P.p = (-12., 18., 25.5);
    P.size = (20., 17., 6.);
}

let k14 = {
    P.a = (pi/.6., pi/.12. +. pi/.20., pi/.30.);
    P.f = cherry_mx (1.5, -1.5);
    P.p = (-12., 37., 30.5);
    P.size = (20., 20., 6.);
}

let k20 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (18., -37., 30.5);
    P.size = (17., 17., 6.);
}


let k21 = {
    P.a = (-.pi/.5., pi/.20., pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (18., -20., 31.);
    P.size = (17., 17., 6.);
}

let k22 = {
    P.a = (0., pi/.20., pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (14., 1., 16.);
    P.size = (17., 17., 6.);
}

let k23 = {
    P.a = (pi/.12., pi/.20., pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (12.5, 20., 16.);
    P.size = (17., 17., 6.);
}

let k24 = {
    P.a = (pi/.6., pi/.20., pi/.60.);
    P.f = cherry_mx (0., -1.5);
    P.p = (12., 39., 21.);
    P.size = (17., 20., 6.);
}

let kp = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx (-2., 0.);
    P.p = (45., -67., 19.0);
    P.size = (21., 17., 6.);
}

let k30 = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx (0., 0.);
    P.p = (36., -37., 25.0);
    P.size = (17., 17., 6.);
}

let k31 = {
    P.a = (-.pi/.5., pi/.20., 0.);
    P.f = cherry_mx (0., 0.);
    P.p = (36., -16., 24.);
    P.size = (17., 17., 6.);
}

let k32 = {
    P.a = (0., pi/.20., 0.);
    P.f = cherry_mx (0., 0.);
    P.p = (31., 6., 10.);
    P.size = (17., 17., 6.);
}

let k33 = {
    P.a = (pi/.12., pi/.20., 0.);
    P.f = cherry_mx (0., 0.);
    P.p = (31., 25., 10.);
    P.size = (17., 17., 6.);
}

let k34 = {
    P.a = (pi/.6., pi/.20., 0.);
    P.f = cherry_mx (0., -1.5);
    P.p = (31.5, 44., 15.);
    P.size = (17., 20., 6.);
}

let k40 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx (0., 0.);
    P.p = (54., -42., 23.5);
    P.size = (17., 17., 6.);
}

let k41 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (54., -20., 21.);
    P.size = (17., 17., 6.);
}


let k42 = {
    P.a = (0., pi/.20., -.pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (50., 2., 8.);
    P.size = (17., 17., 6.);
}

let k43 = {
    P.a = (pi/.12., pi/.20., -.pi/.60.);
    P.f = cherry_mx (0., 0.);
    P.p = (51., 21., 8.);
    P.size = (17., 17., 6.);
}

let k44 = {
    P.a = (pi/.6., pi/.20., -.pi/.60.);
    P.f = cherry_mx (-1.5, -1.5);
    P.p = (52., 40., 13.);
    P.size = (20., 20., 6.);
}

let k50 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx (-1., 0.);
    P.p = (73., -39., 20.5);
    P.size = (19., 17., 6.);
}

let k51 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.30.);
    P.f = cherry_mx (0., 0.);
    P.p = (73., -23., 22.);
    P.size = (17., 17., 6.);
}

let k52 = {
    P.a = (0., pi/.20., -.pi/.30.);
    P.f = cherry_mx (0., 0.);
    P.p = (71., -3., 10.);
    P.size = (17., 17., 6.);
}

let k53 = {
    P.a = (pi/.12., pi/.20., -.pi/.30.);
    P.f = cherry_mx (-1.5, -1.5);
    P.p = (73., 16., 10.);
    P.size = (20., 20., 6.);
}

let k60 = {
    P.a = (0., pi/.20., -.pi/.10.);
    P.f = Model.Key_unit.dummy;
    P.p = (100., -36., 17.);
    P.size = (0.001, 5., 6.);
}
let k61 = {
    P.a = (-.pi/.5., pi/.20., -.pi/.10.);
    P.f = cherry_mx (-1.5, 0.);
    P.p = (90., -26., 20.);
    P.size = (20., 17., 6.);
}

let k62 = {
    P.a = (0., 0., -.pi/.10.);
    P.f = cherry_mx (-1.5, 0.);
    P.p = (95., -6., 7.);
    P.size = (20., 17., 6.);
}

let k63 = {
    P.a = (pi/.12., 0., -.pi/.10.);
    P.f = cherry_mx (-1.5, -1.5);
    P.p = (101., 13., 7.);
    P.size = (20., 20., 6.);
}

let k64 = {
    P.a = (pi/.6., 0., -.pi/.10.);
    P.f = cherry_mx (-3., 0.);
    P.p = (105., 32., 12.);
    P.size = (20., 17., 6.);
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
        P.barfl k53;
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

let screw11 = {
    Patch.a = (0., pi/.20., 0.);
    Patch.p = (65., -57., 14.0);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}


let screw12 = {
    Patch.a = (0., pi/.20., -.pi/.10.);
    Patch.p = (92., -33., 14.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw13 = {
    Patch.a = (0., -.1.*.pi/.2., pi/.6.);
    Patch.p = (5., -32., 3.5);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw14 = {
    Patch.a = (0., -.4.*.pi/.7., pi/.6.);
    Patch.p = (5., -42., 46.);
    Patch.out_r = 2.5;
    Patch.in_r = 1.1;
    Patch.top_h =8.5;
}

let screw_top_bottom = [
    Patch.Screw screw1; Patch.Screw screw2;
    Patch.Screw screw3;
    Patch.Screw screw4;
    Patch.Screw screw5; Patch.Screw screw6;
    Patch.Screw screw7;
    Patch.Screw screw8;
    Patch.Screw screw9;
    Patch.Screw screw10;
    Patch.Screw screw11;
    Patch.Screw screw12;
]

let screw_thumb_top = [
    Patch.Screw screw14;
]

let screw_thumb_bottom = [
    Patch.Screw screw13;
]

let kt11 = {
    P.a = (0., -.1.*.pi/.2., 0.);
    P.f = cherry_mx (1., 1.5);
    P.p = (11., -74., 0.);
    P.size = (19., 20., 6.);
}

let kt12 = {
    P.a = (0., -.1.*.pi/.2.,  pi/.6.);
    P.f = cherry_mx (3., 0.);
    P.p = (10., -47., 0.);
    P.size = (23., 17., 6.);
}

let kt21 = {
    P.a = (0., -.4.*.pi/.7., 0.);
    P.f = cherry_mx (-1.5, 1.5);
    P.p = (11., -74., 22.);
    P.size = (20., 20., 6.);
}

let kt22 = {
    P.a = (0., -.4.*.pi/.7., pi/.6.);
    P.f = cherry_mx (-1.5, 0.);
    P.p = (10., -47., 26.);
    P.size = (20., 17., 6.);
}

let tmat = [
    [None; None;       None;       None];
    [None; Some(kt11); Some(kt12); None];
    [None; Some(kt21); Some(kt22); None];
    [None; None;       None;       None];
]

let mat = [
    [None;    None;     None;     None;     None;     None;     None];
    [None;    None;     Some(k11);Some(k12);Some(k13);Some(k14);None];
    [None;    Some(k20);Some(k21);Some(k22);Some(k23);Some(k24);None];
    [None;    Some(k30);Some(k31);Some(k32);Some(k33);Some(k34);None];
    [Some(kp);Some(k40);Some(k41);Some(k42);Some(k43);Some(k44);None];
    [None;    Some(k50);Some(k51);Some(k52);Some(k53);None;     None];
    [None;    None;     Some(k61);Some(k62);Some(k63);None;     None];
    [None;    None;     None;     None;     None;     None;     None];
]

let thumb = 
    Patch.apply_patches
        (M.union [P.ortho tmat])
        ((screw_thumb_top |> List.map ~f:(fun p -> (p, Patch.Top)))
        @ (screw_thumb_bottom |> List.map ~f:(fun p -> (p, Patch.Top))))

let thumb_cover = gen_cover 3.5 tmat
let idx pad c r = List.nth_exn (List.nth_exn pad c) r
let tcover11 = idx thumb_cover 1 1
let tcover12 = idx thumb_cover 1 2
let tcover21 = idx thumb_cover 2 1
let tcover22 = idx thumb_cover 2 2

let top =
    Patch.apply_patches
        (M.union [
            P.ortho mat;
            sub;
            P.ortho @@ gen_cover 3.5 [[Some(kt21);Some(kt22);]];
            M.hull [
                P.bottom @@ P.lhalf @@ Option.value_exn tcover22;
                P.lside @@ k20;
            ];
            M.hull [
                P.nside @@ k20;
                P.bnside @@ P.lhalf @@ Option.value_exn tcover22;
                P.bfside @@ P.lhalf @@ Option.value_exn tcover21;
                P.barnl k30;
            ];
            M.hull [
                P.bottom @@ P.lhalf @@ Option.value_exn tcover21;
                P.barnl k30;
                P.barfl kp;
            ];
            M.hull [
                P.bnside @@ P.lhalf @@ Option.value_exn tcover21;
                P.lside kp;
            ];
            M.hull [
                P.barfl kp;
                P.nside k30;
            ];
            M.hull [
                P.barnr k30;
                P.barnl k40;
                P.fside kp;
            ];
        ])
        ((screw_top_bottom |> List.map ~f:(fun p -> (p, Patch.Top)))
        @ (screw_thumb_top |> List.map ~f:(fun p -> (p, Patch.Bottom))))


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
            M.hull [
                P.bnside @@ tcover 1 2;
                P.nside @@ mcover 2 1;
                P.pnside @@ mcover 2 1;
                P.barnl @@ mcover 3 1;
                P.pbarnl @@ mcover 3 1;
            ];
            M.hull [
                P.bottom @@ tcover 1 2;
                P.lside @@ mcover 2 1;
                P.plside @@ mcover 2 1;
            ];
            M.hull [
                P.ortho @@ gen_cover 3.5 [[Some(kt12)]];
            ];
        ])
        ((screw_top_bottom |> List.map ~f:(fun p -> (p, Patch.Bottom)))
        @ (screw_thumb_bottom |> List.map ~f:(fun p -> (p, Patch.Bottom))))
    in M.union [
        M.difference
            (M.union [
                base;
            ]) [
            Pcbmod.hollow |>> (-27.5, -17., 0.);
            ];
        Pcbmod.top |>> (-27.5, -17., 0.);
    ]

let () =
    build (M.union [bottom]) "bottom.scad";
    build (M.union [top]) "top.scad";
    build (M.union [thumb]) "thumb.scad";
