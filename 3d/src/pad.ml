type wall_cfg = {
    t: float;
    clearance: float;
}

module type PadConf = sig
    val far_curve: float
    val near_curve: float
    val block_size: (float * float * float)
    val keygen: (float * float * float) -> Model.t
    val len_wall_clearance: float
    val gen_len_wall: bool
    val col_d: float
    val params: (int * int * float * float) list
    val len_wall: wall_cfg option
    val wall_h: float
    val row_wall: wall_cfg
    val prevent_near_wall: int
    val thumb_angle_interval: float
    val thumb_pos: (float * float * float)
end

module Pad (C: PadConf) = struct
    module M = Model

    let eps = 0.0001

    let (w, d, h) = C.block_size
    let block = C.keygen C.block_size |>> (w/.2., d/.2., 0.)
    let o = (0., 0., 0.)

    let block_local_p bending n = List.init n (fun i -> bending *. float_of_int (i+1))
        |> List.fold_left (fun p bending -> p <+> (0., d *. cos bending, d *.sin bending)) (0., 0., 0.)

    let block_local_a bending n = bending *. float_of_int n

    let mov_block_local bending n scad = scad |@> (bending *. float_of_int (n+1), 0., 0.) |>> block_local_p bending n

    (* 0 <= x < List.length params *)
    let block_p x y =
        let xp = (C.col_d +. w) *. float_of_int x in
        let (n, m, dy, dz) = List.nth C.params x in
        if y = 0
        then (xp, dy, dz)
        else if y > 0
        then (xp, dy +. d, dz) <+> block_local_p C.far_curve y
        else (xp, dy, dz) <+> ((block_local_p C.far_curve (-y)) <*> (1., -1., 1.))

    let mov_block x y scad =
        if y > 0
        then scad |@> (C.far_curve *. float_of_int y, 0., 0.) |>> block_p x y
        else scad |>> (0., -.d, 0.) |@> (C.near_curve *. float_of_int y, 0., 0.) |>> block_p x y

    let gen_col_half bending n = M.union @@ List.init n (fun i -> mov_block_local bending i block)
    let gen_bond_half (n, n') (dy, dz) bending =
        let side_face = M.cube (eps, d, h) in
        let focus = M.cube (eps, eps, h) |>> (0., d, 0.) in
        let common = List.init (min n n') (fun i -> M.hull [
                mov_block_local bending i side_face;
                mov_block_local bending i side_face |>> (C.col_d, dy, dz);
            ]) in
        let remain =
            if n > n'
            then List.init (abs (n-n')) (fun i -> M.hull [
                mov_block_local bending (i+min n n') side_face;
                mov_block_local bending ((min n n')-1) focus |>> (C.col_d, dy, dz)
            ])
            else
            if n' > n
            then List.init (abs (n-n')) (fun i -> M.hull [
                mov_block_local bending ((min n n')-1) focus;
                mov_block_local bending (i+min n n') side_face |>> (C.col_d, dy, dz);
            ])
            else [] in
        M.union (common @ remain)

    let pad params =
        let rec build x = function
            | (n, m, dy, dz) :: ((n', m', dy', dz') as succ) :: tl ->
                let col_far = gen_col_half C.far_curve n |>> (0., d, 0.) in
                let bond_far = gen_bond_half (n, n') (dy'-.dy, dz'-.dz) C.far_curve |>> (w, d, 0.) in
                let col_near = gen_col_half C.near_curve m |> M.mirror (0, 1, 0) in
                let bond_near = gen_bond_half (m, m') (dy-.dy', dz'-.dz) C.near_curve |> M.mirror (0, 1, 0) |>> (w, 0., 0.) in
                let center = block in
                let bond_center = M.hull [M.cube (eps, d, h); M.cube (eps, d, h) |>> (C.col_d, dy'-.dy, dz'-.dz)] |>> (w, 0., 0.) in
                (M.union [col_far; bond_far; col_near; bond_near; center; bond_center] |>> (x, dy, dz))
                :: build (x +. w +. C.col_d) (succ :: tl)
            | (n, m, dy, dz) :: [] ->
                let col_far = gen_col_half C.far_curve n |>> (0., d, 0.)in
                let col_near = gen_col_half C.near_curve m |> M.mirror (0, 1, 0) in
                let center = block in
                (M.union [col_far; col_near; center] |>> (x, dy, dz)) :: []
            | [] -> []
        in M.union @@ build 0.0 params

    let rec ignore_n n l = match n with 
        | 0 -> l
        | n -> ignore_n (n-1) (List.tl l)

    let len_wall cfg =
        let rec build bending x =
            let gen_ext t n dy dz = 
                let p = block_local_p bending n in
                M.hull [
                    M.cube (t, cfg.clearance, h *. cos (bending *. float_of_int n));
                    M.cube (t, 0.001, h) |@> (bending *. float_of_int n, 0., 0.);
                ] |>> (p <+> (0., dy, dz)) in
            let gen_wall t n dy dz =
                let p = block_local_p bending n in
                M.cube (
                    t,
                    cfg.t,
                    C.wall_h +. dz +. (get_z p) +. h *. cos (bending *. float_of_int n)
                ) |>> (0., dy +. cfg.clearance +. get_y p, -.C.wall_h) in
            function
            | (n, dy, dz) :: ((n', dy', dz') as succ) :: tl ->
                let mk_bond f =
                    if n > n'
                    then M.hull [
                        f 0.001 n dy dz |>> (w, 0., 0.);
                        f 0.001 n dy dz |>> (w +. cfg.t, 0., 0.);
                        f 0.001 n' dy' dz' |>> (w +. C.col_d, 0., 0.);
                        f 0.001 n' dy' dz' |>> (w +. C.col_d +. cfg.t, 0., 0.);]
                    else if n' > n then M.hull [
                        f 0.001 n dy dz |>> (w, 0., 0.);
                        f 0.001 n dy dz |>> (w -. cfg.t, 0., 0.);
                        f 0.001 n' dy' dz' |>> (w +. C.col_d, 0., 0.);
                        f 0.001 n' dy' dz' |>> (w +. C.col_d -. cfg.t, 0., 0.);]
                    else  M.hull [
                        f 0.001 n dy dz |>> (w, 0., 0.);
                        f 0.001 n' dy' dz' |>> (w +. C.col_d, 0., 0.);
                    ]
                in
                (M.union [gen_ext w n dy dz; mk_bond gen_ext; mk_bond gen_wall; gen_wall w n dy dz] |>> (x, 0., 0.))
                    :: build bending (x +. w +. C.col_d) (succ :: tl)
            | (n, dy, dz) :: [] ->
                [M.union [gen_ext w n dy dz; gen_wall w n dy dz] |>> (x, 0., 0.)]
            | [] -> [] in
        let far_params = C.params |> List.map (fun (n, _, dy, dz) -> (n, dy, dz)) in
        let near_params = C.params |> ignore_n C.prevent_near_wall |> List.map (fun (_, m, dy, dz) -> (m, -.dy, dz)) in
        M.union [
            M.union (build C.far_curve 0. far_params) |>> (0., d, 0.);
            M.union (build C.near_curve 0. near_params)
            |> M.mirror (0, 1, 0)
            |>> ((w +. C.col_d) *. float_of_int C.prevent_near_wall, 0., 0.);
        ]

    let rec last = function
        | [] -> invalid_arg ""
        | [x] -> x
        | x :: tl -> last tl

    let row_wall =
        let left = List.hd C.params in
        let right = last C.params in
        let build_half prevent_len_wall wall_h bending n =
            let rec series = function
            | 0 -> []
            | n -> let p = block_local_p bending (n-1) in
                (M.hull [
                    M.cube (C.row_wall.t, d, h) |> mov_block_local bending (n-1);
                    M.cube (C.row_wall.t, d *. cos (bending *. float_of_int n), 0.001) |>> (0., get_y p, -.wall_h);
                ]) :: series (n-1) in
            match C.len_wall with
            | None -> M.union (series n)
            | Some(cfg) ->
                if prevent_len_wall
                then M.union (series n)
                else
                    let p = block_local_p bending n in
                    let seal = M.cube (
                        C.row_wall.t,
                        cfg.clearance +. cfg.t +. h *. sin (bending *. float_of_int n),
                        (get_z p) +. wall_h +. h *. cos (bending *. float_of_int n)
                    ) |>> (0., (get_y p) -. h *. sin (bending *. float_of_int n), -.wall_h) in
                    M.union (seal :: series n) in
        let build (prev_far, prev_near) (n, m, dy, dz) =
            let wall_h = C.wall_h +. dz in
            M.union [
                build_half prev_far wall_h C.far_curve n |>> (0., d +. dy, dz);
                build_half prev_near wall_h C.near_curve m |> M.mirror (0, 1, 0) |>> (0., dy, dz);
                M.cube (C.row_wall.t, d, h +. wall_h) |>> (0., dy, -.C.wall_h);
            ] in
        let prev_far_wall = match C.len_wall with | Some(_) -> false | _ -> true in
        M.union [
            build (prev_far_wall, C.prevent_near_wall > 0) left
            |>> (-.C.row_wall.t, 0., 0.);
            build (prev_far_wall, false) right
            |>> (-. C.col_d +. (w +. C.col_d) *. float_of_int (List.length C.params), 0., 0.);
        ]

    let bond range =
        let rec gen_domain col = function
            | (n, m, dy, dz) :: tl ->
                if col = 0
                then []
                else let local_p = List.init m (fun i -> C.near_curve *. float_of_int (i+1))
                    |> List.map (fun t -> (0., -.d *. cos t, d *. sin t))
                    |> List.fold_left (<+>) (0., 0., 0.) in
                let bar = M.cube (0.001, 0.001, h) |@> (-.(float_of_int m) *. C.near_curve, 0., 0.) |>> local_p in
                let base_x = (w +. C.col_d) *. float_of_int col in
                (base_x, bar |>> (base_x, dy, dz)) :: (base_x +. w, bar |>> (base_x +. w, dy, dz))
                :: gen_domain (col+1) tl
            | [] -> [] in
        let domain = gen_domain 0 C.params
        in domain

    (* 扇状に並べる *)
    let thumb =
        let rec ext p angle = function 
            | block :: blocks ->
                let side = M.cube (0.001, d, h) in
                let angle' = angle -. C.thumb_angle_interval in
                (M.hull [
                    side |@> (0., 0., angle) |>> p;
                    side |@> (0., 0., angle') |>> p;
                ])
                :: (block |@> (0., 0., angle') |>> p)
                :: ext (p <+> (w *. cos angle', w *. sin angle', 0.)) angle' blocks
            | [] -> [] in
        let right = ext (0., 0., 0.) 0.0 [block]
            |> M.union |>> (w, 0., 0.) in
        let center = block in
        (* blockを一旦反転して展開し、展開後再度反転させる *)
        let left = ext (0., 0., 0.) 0.0 ([block; block] |> List.map (fun x -> x |> M.mirror (1, 0, 0) |>> (w, 0., 0.)))
            |> M.union |> M.mirror (1, 0, 0) in
        M.union (left :: center :: right :: []) |>> C.thumb_pos

    let thumb_bridge =
        let needle = M.cube (0.5, 0.5, h) in
        let matrix_edges =
            let rec gen x = function
                | (_, n, dy, dz) ::  rest ->
                    let needle1 = mov_block_local C.near_curve n (needle |>> (x, -.dy, dz)) |> M.mirror (0, 1, 0) in
                    needle1 :: (needle1 |>> (w, 0., 0.)) :: gen (x +. w +. C.col_d) rest
                | [] -> []
            in gen 0.0 C.params in
        let thumb_edges =
            let rec gen p angle = function
                | 0 -> []
                | n ->
                    let dp = (w *. cos angle, -. w *. sin angle, 0.) in
                    let base = p <+> (d *. sin angle, d *. cos angle, 0.) in
                    let far  = (base <+> dp) in
                    base :: far :: gen (p <+> dp) (angle +. C.thumb_angle_interval) (n-1) in
            let right = gen (0., 0., 0.) C.thumb_angle_interval 1 |> List.map (fun p -> p <+> (w, 0., 0.)) in
            let left = gen (0., 0., 0.) C.thumb_angle_interval 2 |> List.map (fun (x, y, z) -> (-.x, y, z)) |> List.rev in
            let center = [(0., d, 0.); (w, d, 0.)] in
            List.map (fun p -> needle |>> (p <+> C.thumb_pos)) (left @ center @ right) in
        M.union @@ matrix_edges @ thumb_edges

    let test = match C.len_wall with
        | None -> M.union [
            (*pad C.params;*)
            (*row_wall;*)
            thumb_bridge;
            thumb;
        ]
        | Some(cfg) -> M.union [
            len_wall cfg;
            row_wall;
            pad C.params;
        ]
end
