#use "./scad_ml/src/scad.ml";;

module Track = struct
    let pole_d = 41.9
    let ball_r = 17.0
    let ball_clearance = 0.5
    let hole_d = 1.7
    let hole_clearance = 3.
    let bearing_shaft_r = 1.6
    let bearing_shaft_h = 8.
    let bearing_r = 3.5
    let bearing_clearance = 0.4
    let bearing_thick = 4.

    let bearing_hole =
        Model.union [
            Model.cylinder bearing_shaft_r bearing_shaft_h ~fn:30
            |> Model.rotate (0., pi /. 2., 0.);
            Model.cube (bearing_shaft_r, bearing_shaft_r*.2.,  bearing_shaft_h)
            |> Model.rotate (0., pi /. 2., 0.)
            |> Model.translate (0., -.bearing_shaft_r, bearing_shaft_r);
            Model.cylinder (bearing_r+.bearing_clearance) bearing_thick ~fn:30
            |> Model.rotate (0., pi /. 2., 0.)
            |> Model.translate ((bearing_shaft_h -. bearing_thick) /. 2., 0., 0.)
        ]

    let bearing_holes r =
        let hole = bearing_hole
            |> Model.translate (-.bearing_shaft_h/.2., r, 0.) in
        Model.union [
            hole;
            hole |> Model.rotate (0., 0., pi *. 2. /. 3.);
            hole |> Model.rotate (0., 0., -. pi *. 2. /. 3.)
        ]

    let under_block t h =
        let bearings_r = sqrt ((ball_r +. bearing_r) ** 2 -. h ** 2) in
        Model.difference
            (Model.union [
                Model.cube (pole_d +. hole_clearance *. 2., pole_d, t);
            ])
            [
                (Model.translate (pole_d /. 2. +. 3., hole_clearance, 0.)
                    (Model.cylinder hole_d t ~fn:30));
                (Model.translate (3., pole_d /. 2. +. 3., 0.)
                    (Model.cylinder hole_d t ~fn:30));
                (Model.translate (pole_d +. 3., pole_d /. 2. +. 3., 0.)
                    (Model.cylinder hole_d t ~fn:30));
                bearing_holes bearings_r
                |> Model.translate (pole_d /. 2. +. 3., pole_d /. 2., t -. bearing_shaft_r);
                Model.sphere (ball_r +. ball_clearance) ~fn:30
                |> Model.translate (pole_d /. 2. +. 3., pole_d /. 2., t -. bearing_shaft_r +. h)
            ]
end
