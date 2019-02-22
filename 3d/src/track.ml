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

    let foundation h =
        let t = ball_r -. h in
        let bearings_r = sqrt ((ball_r +. bearing_r) ** 2 -. h ** 2) in
        Model.difference
            (Model.union [
                Model.cube (pillar_d +. hole_c *. 2., pillar_d, t);
            ])
            [
                Model.cylinder hole_r t ~fn:30
                    |> Model.translate (hole_c, pillar_d /. 2. +. hole_c, 0.);
                Model.cylinder hole_r t ~fn:30
                    |> Model.translate (pillar_d +. hole_c, pillar_d /. 2. +. hole_c, 0.);
                bearinghedge bearings_r
                    |> Model.rotate (0., 0., pi /. 3.)
                    |> Model.translate (pillar_d /. 2. +. hole_c, pillar_d /. 2. +. hole_c, t -. bearing_shaft_r);
                Model.sphere (ball_r +. ball_c) ~fn:50
                    |> Model.translate (pillar_d /. 2. +. hole_c, pillar_d /. 2. +. hole_c, t -. bearing_shaft_r +. h)
            ]
end
