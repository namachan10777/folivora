module M = Scad_ml.Model
open Scad_ml.Util

let centered (w, d, h) =
    M.cube (w, d, h) |>> (-.w/.2., -.d/.2., 0.0)

type key_elm_config_t = {
    groove_d: float;
    (* w * d *)
    bottleneck_t: float;
    groove_size: (float * float);
    depression_size: (float * float * float);
    stairwell_size: (float * float * float);
    (* w * d *)
    block_size: (float * float);
}

let key_gen conf =
    let gen size = M.cube size ~center:true |>> (0., 0., (get_z size) /. 2.) in
    let total_h = (get_z conf.depression_size) +. (get_z conf.stairwell_size) in
    let stairwell = gen conf.stairwell_size in
    let depression = gen conf.depression_size in
    let groove = gen (fst conf.groove_size, snd conf.groove_size, (get_z conf.stairwell_size) -. conf.bottleneck_t) in
    let lump = gen (fst conf.block_size, snd conf.block_size, total_h) in
    M.difference lump [
        stairwell;
        depression |>> (0., 0., get_z conf.stairwell_size);
        groove |>> (0., +. conf.groove_d /. 2., 0.);
        groove |>> (0., -. conf.groove_d /. 2., 0.);
    ]
    |>> ((fst conf.block_size) /. 2., (snd conf.block_size) /. 2., 0.)

let kailh_lp (w, d, h) =
    let depression_h = 1.2 in
    let bottleneck_t = 1.2 in
    key_gen {
        groove_d=7.0;
        groove_size=(15.5, 4.0);
        bottleneck_t;
        depression_size=(15.5, 15.5, depression_h);
        stairwell_size=(14., 14., h -. depression_h);
        block_size=(w, d);
    } 

(*TODO: dummy implementation*)
let cherry_mx (w, d, h) =
    let depression_h = 1.2 in
    let bottleneck_t = 1.2 in
    key_gen {
        groove_d=7.0;
        groove_size=(15.5, 4.0);
        bottleneck_t;
        depression_size=(15.5, 15.5, depression_h);
        stairwell_size=(14., 14., h -. depression_h);
        block_size=(w, d);
    } 
