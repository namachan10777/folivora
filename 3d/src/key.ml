#use "./scad_ml/src/scad.ml"

module Key = struct
    let key_size = 19., 21., 5.
    let entrance_hole_size = 14., 14., 5.
    let hole_size = 15., 15., 3.
    let key_elm_tilt = pi /. 12.
    let center_move_vec c1 c2 =
        let diff a b = (a -. b) /. 2. in
        match c1, c2 with (w1, h1, d1), (w2, h2, d2) ->
            (diff w1 w2), (diff h1 h2), (diff d1 d2)

    let key_elm =
        Model.difference
            (Model.cube key_size)
            [
                (Model.cube entrance_hole_size
                |> Model.translate
                    (center_move_vec key_size entrance_hole_size
                    |> Math.Pos.mul (1., 1., 0.)));
                (Model.cube hole_size)
                |> Model.translate
                    (center_move_vec key_size hole_size
                    |> Math.Pos.mul (1., 1., 0.))]

    let get_x (x, _, _) = x
    let get_y (_, y, _) = y
    let get_z (_, _, z) = z

    let key_joint_pos_series tilts =
        fst @@ List.fold_left
            (fun (ps, tilt_acc) tilt ->
                let f g = (get_y key_size) *. (g (tilt_acc +. tilt)) in
                    let p = Math.Pos.add (List.hd ps) (0., f cos, f sin) in
                    (p :: ps, (tilt_acc +. tilt)))
            ([(0., 0., 0.)], 0.) tilts

    let tilt_acc tilts = List.fold_left (fun (tilts, acc) tilt ->
        let acc = acc +. tilt in (acc :: tilts), acc) ([], 0.) tilts |> fst

    let rec drop = function
        | [] -> raise (Invalid_argument "drop")
        | [t] -> []
        | h :: t -> h :: drop t

    let key_base_pos_series tilts =
        List.map2 (fun jp tilt ->
            Math.Pos.add jp (0., (get_z key_size) *. (sin tilt), -.(get_z key_size) *. (cos tilt)))
        (key_joint_pos_series tilts |> List.tl) (tilt_acc tilts)

    let key_tip_pos_series tilts =
        List.map2 (fun bp tilt ->
            Math.Pos.add bp (0., (get_y key_size) *. (cos tilt), (get_y key_size) *. (sin tilt)))
        (key_base_pos_series tilts) (tilt_acc tilts)

    let key_padding tri =
        let ps = tri @ List.map (Math.Pos.add (get_x key_size, 0., 0.)) tri in
        Model.polyhedron ps [[0; 1; 2];[1;0;3;4];[4;3;5];[2;1;4;5];[0;2;5;3]]

    let rec map3 f l1 l2 l3 = match (l1, l2, l3) with
        | ([], [], []) -> []
        | (e1::l1, e2::l2, e3::l3) -> (f e1 e2 e3) :: map3 f l1 l2 l3
        | _ -> raise (Invalid_argument "map3")

    let key_col_padding a b a_offset b_offset =
        let half tilts offset =
            let jp = key_joint_pos_series tilts in
            let bp = key_base_pos_series tilts in
            let tp = key_tip_pos_series tilts in
            let ps = (List.hd jp) :: List.flatten (map3 (fun t j b -> [t;b;j]) tp (List.tl jp) bp) @ [(0., 0., -.(get_z key_size))] in
            List.map (Math.Pos.add offset) ps in
        let right = half a a_offset in
        let left  = half b b_offset in
        let col_n = (List.length a) in
        let part = col_n * 3 + 2 in
        let rev_side = List.map (fun l -> List.map ((+) part) l |> List.rev) in
        let square_r = List.init col_n (fun i -> [i*3+3;i*3+2;i*3+1;i*3]) in
        let square_l = rev_side square_r in
        let triangle_r = List.init col_n (fun i -> [i*3+2;i*3+3;i*3+4]) in
        let triangle_l = rev_side triangle_r in
        let floor_large = List.init col_n (fun i -> [i*3+1;i*3+2;i*3+part+2;i*3+part+1]) in
        let floor_small = List.init col_n (fun i -> [i*3+2;i*3+4;i*3+part+4;i*3+part+2]) in
        let ceil = List.init col_n (fun i -> [i*3+3;i*3;i*3+part;i*3+part+3]) in
        let cover = [[0;1;part+1;part];[part-1;part-2;part*2-2;part*2-1]] in
        Model.polyhedron (right @ left) (square_r @ triangle_r @ square_l @ triangle_l @ floor_small @ floor_large @ ceil @ cover) 
 
    let key_col near far =
        let make_col tilts =
            List.map2 (fun bp tilt ->
                key_elm
                |> Model.rotate (tilt, 0., 0.)
                |> Model.translate (0., get_y key_size, get_z key_size)
                |> Model.translate bp)
            (key_base_pos_series tilts) (tilt_acc tilts) in
        let make_paddings tilts =
            let ps = map3
                (fun b j t -> t :: j :: b :: [])
                (key_base_pos_series tilts)
                (key_joint_pos_series tilts |> List.tl)
                (key_tip_pos_series tilts) |> List.flatten in
            let rec f = function
                | b :: j :: t :: tl ->
                    (key_padding [b; j; t]) :: f tl
                | _ -> []
            in f ((0., 0., -.(get_z key_size)) :: (List.rev ps))
                |> List.map (Model.translate (0., get_y key_size, get_z key_size))
        in Model.union (key_elm :: (make_col near) @ (make_paddings near))
end
