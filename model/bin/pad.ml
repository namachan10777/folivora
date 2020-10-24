module M = Scad_ml.Model
module Mat = Model.Matrix
open Scad_ml.Util
open Core

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

type conf_t = {
    main_pad : pad_conf_t;
    pad_tilt : Scad_ml.Math.t;
    thumb_pad : pad_conf_t;
    thumb_tilt : Scad_ml.Math.t;
    thumb_pos : Scad_ml.Math.t;
    wrist_switch_size : Scad_ml.Math.t;
    wrist_switch : (Scad_ml.Math.t -> Scad_ml.Core.scad_t);
    wrist_switch_offset : float * float;
    wrist_switch_pos : Scad_ml.Math.t;
}

let gen_col c =
	let center_size = fst c.base in
	let (_, d, h) = center_size in
	let center = (snd c.base) center_size |>> (0., 0., -.h) in
	let rec gen trans p angle_acc = function
		| [] -> []
		| key :: remain ->
			let (key_size, key_gen, angle) = key in
			let (w, d, h) = key_size in
			let (x, y, z) = p in
			let angle_acc' = angle_acc +. angle in
			let plate = M.cube (w, 0.0001, h) |>> (0., 0., -.h) in
			let joint = M.hull [
				plate |@> (angle_acc, 0.0, 0.0) |>> p;
				plate |@> (angle_acc+.angle, 0.0, 0.0) |>> p;
			] in
			let key = trans key_gen key_size  |>> (0., 0., -.h) |@> (angle_acc', 0.0, 0.0) |>> p in
			key :: joint :: gen trans (x, y +. d *. Float.cos angle_acc', z +. d *. Float.sin angle_acc') angle_acc' remain
	in
	let far = gen (fun gen size -> gen size) (0.0, d, 0.0) 0.0 c.far in
	let near =
		gen (fun gen size -> gen size |> M.mirror (0, 1, 0) |>> (0., get_y size, 0.)) (0.0, 0.0, 0.0) 0.0 c.near
		|> M.union |> M.mirror (0, 1, 0)
	in
	M.union (center :: near :: far)

let f conf =
    let get_col_width c = match c.base with
        ((w, _, _), _) -> w
    in
    let rec g p = function 
        | [] -> []
        | c :: []  -> [gen_col c |> M.translate p]
        | c1 :: (c2 :: _ as remain) ->
            (gen_col c1 |> M.translate p) :: g (p <+> c2.d <+> (get_col_width c1, 0., 0.)) remain
    in
    let p_start = List.hd conf
        |> Option.map ~f:(fun c -> c.d)
        |> Option.value ~default:(0., 0., 0.)
    in
    conf |> g p_start |> M.union

let calc_pos t col n =
    let reverse_adjust bend a =
        if Float.(<.) bend 0.
        then
            let y = t *. Float.sin (-.bend) in
            let z = -. t +. t *. Float.cos bend in
            (0., (y *. Float.cos a) -. (z *. Float.sin a), (y *. Float.sin a)  +. (z *. Float.cos a))
        else (0., 0., 0.)

    in
    let () = Printf.printf "\n--------------\n" in
    let rec f a p =function
    | [] -> (a, p)
    | (_, bend, _) :: [] ->
        let () = Printf.printf "%f\n" a in
        (a +. bend, p <+> reverse_adjust bend a)
    | (d, bend, _) :: last ->
        let () = Printf.printf "pass %f\n" a in
        let (x, y, z) = p in
        let a = a +. bend in
        let p = (x, y +. d *. Float.cos a, z +. d *. Float.sin a) <+> reverse_adjust bend (a-.bend) in
        f a p last
    in
    let (ax, p) = f 0.0 (0., 0., 0.) (List.take col.keys (n+1)) in
    let a = (ax, 0., 0.) <+> col.angle in
    let p = Mat.trans (Mat.rot (get_x col.angle, get_y col.angle, get_z col.angle)) p in
    (a, p <+> col.pos)

let gen_col t col =
    List.init (List.length col.keys) ~f:(fun i ->
        let (a, p) = calc_pos t col i in
        let (d, _, gen) = List.nth_exn col.keys i in
        let key = gen (col.w, d, t) |>> (0., 0., -.t) in
        let key = key |@> a |>> p in
        if i >= (List.length col.keys) - 1
        then [key]
        else
            let plate = M.cube (col.w, 0.01, t) |>> (0., 0., -.t) in
            let near = plate |@> a |>> (p <+> Mat.trans (Mat.rot a) (0., d, 0.) ) in
            let (a, p) = calc_pos t col (i+1) in
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
        let (al, pl) = calc_pos t coll il in
        let (ar, pr) = calc_pos t colr (ir-1) in
        let (ar', pr') = calc_pos t colr ir in
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
        let (al, pl) = calc_pos t coll il in
        let (ar, pr) = calc_pos t colr ir in
        let (ar', pr') = calc_pos t colr (ir-1) in
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
        let (ar, pr) = calc_pos t colr ir in
        let (al, pl) = calc_pos t coll il in
        let joint = M.hull [sider |@> ar |>> pr; sidel |@> al |>> pl] in
        if i >= take_n-1
        then [joint]
        else
            let needle = M.cube (0.01, 0.01, t) |>> (0., 0., -.t) in
            let (ar', pr') = calc_pos t colr (ir+1) in
            let (al', pl') = calc_pos t coll (il+1) in
            [joint;M.hull [
                needle |@> ar' |>> pr';
                needle |>> (coll.w, 0., 0.) |@> al' |>> pl';
                needle |>> (0., dr, 0.) |@> ar |>> pr;
                needle |>> (coll.w, 0., 0.) |>> (0., dl, 0.) |@> al |>> pl;
            ]])
    |> List.concat
    in
    join_center @ jointl_far @ jointl_near

let pad conf =
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

let wrist_switch size key p =
    let (w, d, h) = size in
    let inner = (w-.10., d-.10., h-. 5.) in
    let switch_size = ((get_x inner) -. Float.abs (w /. 2. -. fst p), (get_y inner) -. Float.abs (d /. 2. -. snd p), 5.) in
    let switch = key switch_size in
    M.union [
        M.difference (M.cube size) [
            M.cube inner |>> (5., 5., 0.);
            M.cube switch_size |>> ((fst p) -. (get_x switch_size) /. 2., (snd p) -. (get_y switch_size) /. 2., 5.);
        ];
        switch |>> ((fst p) -. (get_x switch_size) /. 2., (snd p) -. (get_y switch_size) /. 2., 5.);
    ]

let f conf =
    M.union [
         pad conf.main_pad |@> conf.pad_tilt;
         pad conf.thumb_pad |@> conf.thumb_tilt |>> conf.thumb_pos;
         wrist_switch conf.wrist_switch_size conf.wrist_switch conf.wrist_switch_offset |>> conf.wrist_switch_pos;
    ]
