#use "./scad_ml/src/scad.ml";;

module Track = struct
    let pillar_d = 41.9


    let ball_r = 17.0
    let ball_c = 0.5

    let hole_r = 1.7
    let hole_c = 3.

    let bearing_shaft_r = 1.6
    let bearing_shaft_h = 8.

    let bearing_r = 3.5
    let bearing_c = 0.4
    let bearing_t = 4.

    let eps = 0.1

    let bearing_hollowing =
        Model.union [
            Model.cylinder bearing_shaft_r bearing_shaft_h ~fn:30
                |> Model.rotate (0., pi /. 2., 0.);
            Model.cube (bearing_shaft_r+.eps, bearing_shaft_r*.2.,  bearing_shaft_h)
                |> Model.rotate (0., pi /. 2., 0.)
                |> Model.translate (0., -.bearing_shaft_r, bearing_shaft_r +. eps);
            Model.cylinder (bearing_r+.bearing_c) bearing_t ~fn:30
                |> Model.rotate (0., pi /. 2., 0.)
                |> Model.translate ((bearing_shaft_h -. bearing_t) /. 2., 0., 0.)
        ]

    let bearinghedge r =
        let hole = bearing_hollowing
            |> Model.translate (-.bearing_shaft_h/.2., r, 0.) in
        Model.union [
            hole;
            hole |> Model.rotate (0., 0.,    pi *. 2. /. 3.);
            hole |> Model.rotate (0., 0., -. pi *. 2. /. 3.)
        ]

    let foundation_bottom = (pillar_d +. hole_c *. 2., pillar_d)
    let foundation_center = (pillar_d /. 2. +. hole_c, pillar_d /. 2. +. hole_c)

    let foundation_body tilt offset =
        let h = ball_r -. offset in
        let h1 = h +. (fst foundation_bottom) *. (sin tilt) /. 2. in
        let h2 = h -. (fst foundation_bottom) *. (sin tilt) /. 2. in
        let points_half = [
            (0.0, 0.0, 0.0);
            (fst foundation_bottom, 0.0, 0.0);
            (fst foundation_bottom, 0.0, h1);
            (0.0, 0.0, h2)
        ] in
        let points_all = points_half @ List.map (Math.Pos.add (0., snd foundation_bottom, 0.)) points_half in
        Model.polyhedron points_all [
            [1; 0; 3; 2];
            [0; 1; 5; 4];
            [0; 4; 7; 3];
            [4; 5; 6; 7];
            [1; 2; 6; 5];
            [2; 3; 7; 6];
        ]
            

    let bearing_cover_t = 3.
end
