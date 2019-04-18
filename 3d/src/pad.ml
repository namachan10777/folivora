module type PadConf = sig
    val far_curve: float
    val near_curve: float
    val block_size: (float * float * float)
    val keygen: (float * float * float) -> Model.t
    val len_wall_clearance: float
    val gen_len_wall: bool
    val col_d: float
    val params: (int * int * float * float) list
end

module Pad (C: PadConf) = struct
    module M = Model

    let eps = 0.0001

    let (w, d, h) = C.block_size
    let block = C.keygen C.block_size |>> (w/.2., d/.2., 0.)
    let o = (0., 0., 0.)

    let block_local_p bending n = List.init n (fun i -> bending *. float_of_int (i+1))
        |> List.fold_left (fun p bending -> p <+> (0., d *. cos bending, d *.sin bending)) (0., 0., 0.)

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

    let test = pad C.params
end
