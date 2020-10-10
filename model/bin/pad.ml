module M = Scad_ml.Model
module Mat = Model.Matrix
open Core
open Scad_ml.Util

type col_t = {
    w : float;
    pos : Scad_ml.Math.t;
    angle: Scad_ml.Math.t;
    offset: int;
    keys : (float * float * (Scad_ml.Math.t -> Scad_ml.Core.scad_t)) list;
}

type pad_conf_t = {
    key_t : float;
    cols : col_t list;
}

let calc_pos col n =
    let rec f a p = function
    | [] -> (a, p)
    | (_, bend, _) :: [] -> (a +. bend, p)
    | (d, bend, _) :: last ->
        let (x, y, z) = p in
        let a = a +. bend in
        f a (x, y +. d *. Float.cos a, z +. d *. Float.sin a) last
    in
    let (ax, p) = f 0.0 (0., 0., 0.) (List.take col.keys (n+1)) in
    let a = (ax, 0., 0.) <+> col.angle in
    let p = Mat.trans (Mat.rot (get_x col.angle, get_y col.angle, get_z col.angle)) p in
    (a, p <+> col.pos)

let gen_col t col =
    List.init (List.length col.keys) ~f:(fun i ->
        let (a, p) = calc_pos col i in
        let (d, _, gen) = List.nth_exn col.keys i in
        let key = gen (col.w, d, t) |>> (0., 0., -.t) in
        let key = key |@> a |>> p in
        if i >= (List.length col.keys) - 1
        then [key]
        else
            let plate = M.cube (col.w, 0.01, t) |>> (0., 0., -.t) in
            let near = plate |@> a |>> (p <+> Mat.trans (Mat.rot a) (0., d, 0.) ) in
            let (a, p) = calc_pos col (i+1) in
            let far = plate |@> a |>> p in
            [key; M.hull [near; far]])
    |> List.concat

let gen_joint t coll colr =
    let nr = (List.length colr.keys) - colr.offset in
    let nl = (List.length coll.keys) - coll.offset in
    let take_n = min nr nl in
    let needle = M.cube (0.01, 0.01, t) |>> (0., 0., -.t) in
    let jointl_far = List.init (abs (nr - nl)) ~f:(fun i -> 
        let coll = (coll, (coll.w, 0., 0.)) in
        let colr = (colr, (0., 0., 0.)) in
        let ((coll, colld), (colr, colrd))
            = if nr > nl then (coll, colr) else (colr, coll) in
        let nl = min nl nr in
        let il = coll.offset + nl - 1 in
        let ir = colr.offset + nl + i in
        let (dl, _, _) = List.nth_exn coll.keys il in
        let (dr, _, _) = List.nth_exn colr.keys ir in
        let (al, pl) = calc_pos coll il in
        let (ar, pr) = calc_pos colr (ir-1) in
        let (ar', pr') = calc_pos colr ir in
        [
            M.hull [
                needle |>> colld  |>> (0., dl, 0.)|@> al |>> pl;
                needle |>> colrd |>> (0., dr, 0.) |@> ar |>> pr;
                needle |>> colrd |@> ar' |>> pr';
            ];
            M.hull [
                needle |>> colld |>> (0., dl, 0.) |@> al |>> pl;
                needle |>> colrd |@> ar' |>> pr';
                needle |>> colrd |>> (0., dr, 0.) |@> ar' |>> pr';
            ];
        ]
    ) |> List.concat in
    let jointl_near = List.init (max colr.offset coll.offset) ~f:(fun i -> 
        let coll' = (coll, (coll.w, 0., 0.)) in
        let colr' = (colr, (0., 0., 0.)) in
        let ((coll, colld), (colr, colrd))
            = if colr.offset > coll.offset then (coll', colr') else (colr', coll') in
        let il = 0 in
        let ir = i + 1 in
        let (dr, _, _) = List.nth_exn colr.keys ir in
        let (al, pl) = calc_pos coll il in
        let (ar, pr) = calc_pos colr ir in
        let (ar', pr') = calc_pos colr (ir-1) in
        [
            M.hull [
                needle |>> colld |@> al |>> pl;
                needle |>> colrd |@> ar |>> pr;
                needle |>> colrd |>> (0., dr, 0.) |@> ar' |>> pr';
            ];
            M.hull [
                needle |>> colld |@> al |>> pl;
                needle |>> colrd |@> ar' |>> pr';
                needle |>> colrd |>> (0., dr, 0.) |@> ar' |>> pr';
            ];
        ]
    ) |> List.concat in
    let join_center = List.init take_n ~f:(fun i ->
        let il = i + coll.offset in
        let ir = i + colr.offset in
        let (dr, _, _) = List.nth_exn colr.keys ir in
        let (dl, _, _) = List.nth_exn coll.keys il in
        let sidel = M.cube (0.01, dl, t) |>> (coll.w, 0., -.t) in
        let sider = M.cube (0.01, dr, t) |>> (0., 0., -.t) in
        let (ar, pr) = calc_pos colr ir in
        let (al, pl) = calc_pos coll il in
        let joint = M.hull [sider |@> ar |>> pr; sidel |@> al |>> pl] in
        if i >= take_n-1
        then [joint]
        else
            let needle = M.cube (0.01, 0.01, t) |>> (0., 0., -.t) in
            let (ar', pr') = calc_pos colr (ir+1) in
            let (al', pl') = calc_pos coll (il+1) in
            [joint;M.hull [
                needle |@> ar' |>> pr';
                needle |>> (coll.w, 0., 0.) |@> al' |>> pl';
                needle |>> (0., dr, 0.) |@> ar |>> pr;
                needle |>> (coll.w, 0., 0.) |>> (0., dl, 0.) |@> al |>> pl;
            ]])
    |> List.concat
    in
    join_center @ jointl_far @ jointl_near


let f conf =
    let rec f = function
    | [] -> []
    | coll :: colr :: last ->
        (gen_col conf.key_t coll)
        @ (gen_joint conf.key_t coll colr)
        @ (f (colr :: last))
    | col :: last ->
        (gen_col conf.key_t col)
        @ (f last)
    in f conf.cols |> M.union
