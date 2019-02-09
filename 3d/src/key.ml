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
        get_x @@ List.fold_left
            (fun (ps, p, tilt_acc) tilt ->
                let f g = (get_y key_size) *. (g tilt_acc) in
                    let p = Math.Pos.add p (0., f cos, f sin) in
                    (p :: ps, p, (tilt_acc +. tilt)))
            ([], (0., 0., 0.), 0.) tilts

    let tilt_acc tilts = List.fold_left (fun (tilts, acc) tilt ->
        let acc = acc +. tilt in (acc :: tilts), acc) ([], 0.) tilts |> fst

    let key_base_pos_series tilts =
        List.map2 (fun jp tilt ->
            Math.Pos.add jp (0., (get_z key_size) *. (sin tilt), -.(get_z key_size) *. (cos tilt)))
        (key_joint_pos_series tilts) (tilt_acc tilts)

    let key_tip_pos_series tilts =
        List.map2 (fun bp tilt ->
            Math.Pos.add bp (0., (get_y key_size) *. (cos tilt), (get_y key_size) *. (sin tilt)))
        (key_base_pos_series tilts) (tilt_acc tilts)

    let key_col near far =
        let make_col tilts =
            List.map2 (fun bp tilt ->
                key_elm
                |> Model.rotate (tilt, 0., 0.)
                |> Model.translate (0., 0., get_z key_size)
                |> Model.translate bp)
            (key_base_pos_series tilts) (tilt_acc tilts)
        in Model.union (key_elm :: (make_col near))
end
