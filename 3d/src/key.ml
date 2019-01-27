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
end
