module type ThumbConf = sig
    val block_size: (float * float * float)
    val keygen: (float * float * float) -> Model.t
end

module Thumb (C: ThumbConf) = struct
    module M = Model

    let breakout_size = (27.94, 21.59, 1.6)
    let hole_positions = [(2.54, 19.05); (25.4, 19.05)]
    let ball_pos = (13.97, 12.7)
    let ball_clearance = 0.7
    let top_size = (9.0, 9.0)
    let groove_ledge = 2.0
    let groove_w = 1.2
    let groove_d = 1.5
    let top_t = 1.0
    let module_h = 4.0
    let parts_range = (20., 15., 1.5)

    let cover =
        let centerize = (-1./.2., -1./.2., 0.) in
        let lump = M.cube (2. +. get_x breakout_size, 2. +. get_y breakout_size, module_h +. get_z breakout_size)
            |>> (-1., -1., 0.) in
        let breakout_hollowing_size = (breakout_size <+> (ball_clearance, ball_clearance, 0.)) in
        let breakout_hollowing = M.cube breakout_hollowing_size |>> (-.ball_clearance/.2., -.ball_clearance/.2., 0.) in
        let parts_hollowing = M.cube parts_range
            |>> (parts_range <*> centerize <+> (fst ball_pos, snd ball_pos, get_z breakout_size)) in
        let cable_hollowing = M.cube (get_x breakout_size, 2.54, module_h -. top_t)
            |>> (0., 0., get_z breakout_size) in
        let module_hollowing_size = ((fst top_size, snd top_size, module_h) <+> (0.2, 0.2, 0.0)) in
        let module_hollowing = M.cube module_hollowing_size
            |>> (module_hollowing_size <*> centerize <+> (fst ball_pos, snd ball_pos, get_z breakout_size)) in
        let groove_size = (groove_ledge *. 2. +. fst top_size, groove_w, module_h -. top_t) in
        let groove = M.cube groove_size
            |>> (groove_size <*> centerize <+> (fst ball_pos, snd ball_pos, get_z breakout_size)) in
        let grooves = [
            groove |>> (0., +.groove_d, 0.);
            groove |>> (0., -.groove_d, 0.);
        ] in
        let screw = M.cylinder 1.55 (module_h +. get_z breakout_size) ~fn:30 in
        let screw_holes = List.map (fun (x, y) -> screw |>> (x, y, 0.)) hole_positions in
        M.difference lump ([
            breakout_hollowing;
            parts_hollowing;
            cable_hollowing;
            module_hollowing;
        ] @ grooves @ screw_holes)
end
