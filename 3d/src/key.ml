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

    type sidewall_point_type =
        | Top_far
        | Bottom_far
        | Joint
        | Base

    let get_x (x, _, _) = x
    let get_y (_, y, _) = y
    let get_z (_, _, z) = z

    let sidewall_point tilt_step level spec =
        let rec f tilt level spec = match level, spec with
        | -1, _ -> 0., 0., 0.
        | (_, Top_far) ->
            Math.Pos.add (f tilt level Joint) (0., (cos tilt) *. (get_y key_size), (sin tilt) *. (get_y key_size))
        | (_, Bottom_far) ->
            Math.Pos.add (f tilt level Base) (0., (cos tilt) *. (get_y key_size), (sin tilt) *. (get_y key_size))
        | (_, Base) ->
            Math.Pos.add (f tilt level Joint) (0., (sin tilt) *. (get_z key_size), -. (cos tilt) *. (get_z key_size))
        | (_, Joint) ->
            f (tilt -. tilt_step) (level - 1) Top_far
        in
            Math.Pos.add (f (tilt_step *. float_of_int level) level spec) (0., 0., get_z key_size)

        

    let key_elm_joint_padding_with_pos tilt i =
        let bottom1 = sidewall_point tilt i Bottom_far in
        let joint   = sidewall_point tilt i Top_far in
        let bottom2 = sidewall_point tilt (i+1) Base in
        let width = get_x key_size in
        let bottom_surface = [bottom1; joint; bottom2] in
        Model.polyhedron
            (bottom_surface @ List.map (Math.Pos.add (width, 0., 0.)) bottom_surface)
            [[0; 2; 1]; [0; 1; 4; 3]; [3; 4; 5]; [0; 3; 5; 2]; [1; 2; 5; 4]]

    let key_col near far =
        let make_range start finish =
            Model.union @@
                (List.init (finish-start) @@ fun i ->
                    let i = i + start in
                    key_elm
                    |> Model.rotate (key_elm_tilt *. float_of_int i, 0., 0.)
                    |> Model.translate (sidewall_point key_elm_tilt i Base))
                @ (List.init (finish-start-1) @@ fun i ->
                    let i = i + start in
                    key_elm_joint_padding_with_pos key_elm_tilt i) in
        Model.union [
            make_range 0 (far+1);
            (Model.union [make_range 1 (near+1); key_elm_joint_padding_with_pos key_elm_tilt 0])
            |> Model.mirror (0, 1, 0)
            |> Model.translate (0., get_y key_size, 0.);
        ]
end
