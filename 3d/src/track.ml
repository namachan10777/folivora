#use "./scad_ml/src/scad.ml";;

module Track = struct
    let pole_d = 41.9
    let ball_r = 17.0
    let ball_clearance = 0.5
    let hole_d = 1.7
    let hole_clearance = 3.
    let bearing_shaft_r = 1.6
    let bearing_shaft_h = 8.
    let bearing_r = 3.8
    let bearing_thick = 4.

    let bearing_hole =
        Model.union [
            Model.cylinder bearing_shaft_r bearing_shaft_h ~fn:30
            |> Model.rotate (0., pi /. 2., 0.);
            Model.cube (bearing_shaft_r, bearing_shaft_r*.2.,  bearing_shaft_h)
            |> Model.rotate (0., pi /. 2., 0.)
            |> Model.translate (0., -.bearing_shaft_r, bearing_shaft_r);
            Model.cylinder bearing_r bearing_thick ~fn:30
            |> Model.rotate (0., pi /. 2., 0.)
            |> Model.translate ((bearing_shaft_h -. bearing_thick) /. 2., 0., 0.)
        ]
end
