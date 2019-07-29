#use "./src/common.ml"
#use "./src/key.ml"
#use "./src/pad.ml"
#use "./src/thumb.ml"

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
    let thumb_angle_interval = pi /. 6.
    let thumb_pos = (16.51, -42., 0.)
    let params = [
        (1, 1, -1.0, 1.0);
        (1, 1, 0.0, 0.0);
        (1, 1, 3.0, -2.0);
        (1, 1, 1.5, -1.0);
        (1, 1, -4.0, 1.0);
        (1, 1, -6.0, 2.0);
    ]
    let wall_h = 2.0
    let len_wall = None (*Some({
        t=3.0;
        clearance=1.0;
    })*)
    let row_wall = {
        t=3.0;
        clearance=1.0;
    }
    let prevent_near_wall = 4
end)

let () =
    build (KailhLPPad.test) "key.scad";
