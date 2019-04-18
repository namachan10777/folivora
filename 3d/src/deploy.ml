#use "./src/common.ml"
#use "./src/key.ml"
#use "./src/pad.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

module KailhLPPad = Pad(struct
    let far_curve = pi /. 10.
    let near_curve = pi /. 20.
    let block_size = (16.51, 21., 4.5)
    let keygen = Key.kailh_lp
    let len_wall_clearance = 1.5
    let gen_len_wall = true
    let col_d = 2.54
    let params = [
        (2, -1.0, 1.0);
        (2, 0.0, 0.0);
        (2, 3.0, -2.0);
        (2, 1.5, -1.0);
        (1, -4.0, 1.0);
        (1, -6.0, 2.0);
    ]
end)

let () =
    build (KailhLPPad.test) "key.scad"
