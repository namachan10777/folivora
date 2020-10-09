module M = Scad_ml.Model
module Mat = Model.Matrix
open Core
open Scad_ml.Util

type col_t = {
    w : float;
    pos : Scad_ml.Math.t;
    angle: Scad_ml.Math.t;
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
    let p = Mat.trans (Mat.rot (get_x col.angle) (get_y col.angle) (get_z col.angle)) p in
    (a, p)

let gen_col t col =
    List.init (List.length col.keys) ~f:(fun i ->
        let (a, p) = calc_pos col i in
        let gen = List.nth_exn col.keys i |> get_z in
        let key = gen (col.w, List.nth_exn col.keys i |> get_x, t) |>> (0., 0., -.t) in
        key |@> a |>> p)

let f conf =
    let rec f = function
    | [] -> []
    | col :: last -> List.append (gen_col conf.key_t col) (f last)
    in f conf.cols |> M.union
