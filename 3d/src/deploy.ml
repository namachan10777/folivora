#use "./src/common.ml"
#use "./src/key.ml"
#use "./src/pad.ml"
#use "./src/thumb.ml"

let angle_unit = pi /. 18.

let build scad filename = 
    let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o666 filename in
    Scad.write oc scad;
    close_out oc

module KailhLPPadConfig = struct
    let far_curve = pi /. 10.
    let near_curve = pi /. 20.
    let block_size = (16.51, 21., 4.5)
    let keygen = Key.kailh_lp
    let len_wall_clearance = 1.5
    let gen_len_wall = true
    let col_d = 2.54
    let thumb_angle_interval = pi /. 6.
    let thumb_pos = (16.51, -42., 0.)
    let mount_size = 3.0
    let screw_size = 1.6
    let mount_screw_d = 50.0
    let params = [
        (1, 1, -1.0, 1.0);
        (1, 1, 0.0, 0.0);
        (1, 1, 3.0, -2.0);
        (1, 1, 1.5, -1.0);
        (1, 1, -4.0, 1.0);
        (1, 1, -6.0, 2.0);
    ]
    let wall_h = 2.0
    let len_wall = Some({
        t=3.0;
        clearance=1.0;
    })
    let row_wall = {
        t=3.0;
        clearance=1.0;
    }
    let prevent_near_wall = 4
    let plate_size = (40., 65.)
end

module ForProjection = Pad(struct
    let far_curve = KailhLPPadConfig.far_curve
    let near_curve = KailhLPPadConfig.near_curve
    let block_size = KailhLPPadConfig.block_size
    let keygen = Key.dummy
    let len_wall_clearance = KailhLPPadConfig.len_wall_clearance
    let gen_len_wall = KailhLPPadConfig.gen_len_wall
    let col_d = KailhLPPadConfig.col_d
    let thumb_angle_interval = KailhLPPadConfig.thumb_angle_interval
    let thumb_pos = KailhLPPadConfig.thumb_pos
    let mount_size = KailhLPPadConfig.mount_size
    let screw_size = KailhLPPadConfig.screw_size
    let mount_screw_d = KailhLPPadConfig.mount_screw_d
    let params = KailhLPPadConfig.params
    let wall_h = KailhLPPadConfig.wall_h
    let len_wall = KailhLPPadConfig.len_wall
    let row_wall = KailhLPPadConfig.row_wall
    let prevent_near_wall = KailhLPPadConfig.prevent_near_wall
    let plate_size = KailhLPPadConfig.plate_size
end)

module KailhLPPad = Pad(KailhLPPadConfig)

let () =
    build (KailhLPPad.body) "key.scad";
    build (Model.union[KailhLPPad.body; KailhLPPad.plate 1.0]) "test.scad";
    build (Model.projection (KailhLPPad.plate 1.0)) "key_electrical.scad";
    build (ForProjection.bottom) "key_bottom.scad";
    build (Model.linear_extrude ~height:1.5 ForProjection.bottom) "bottom_test.scad"
