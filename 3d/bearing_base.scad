include <ball_const.scad>

module core() {
	cube([$cover_size[0], $cover_size[1], $sensor_guard_t + $base_cover_padding]);
}

module bearing_base() {
	$z_ref = $sensor_pcb_target_offset - $sensor_guard_t;
	difference() {
		union() {
			translate([-$cover_size[0]/2, -$cover_size[1]/2, $z_ref]) 
				core();
			translate([-$cover_size[0]/2, -$cover_size[1]/2, 0]) 
				pillars($sensor_pcb_target_offset);
			translate([-$cover_size[0]/2, -$cover_size[1]/2, 0])
				cube([$cover_size[0], $base_junction_min_t, $sensor_pcb_target_offset]);
		}
		translate([-$sensor_hole[0]/2 + $hole_pos[0], -$sensor_hole[1]/2 + $hole_pos[1], $z_ref])
			cube($sensor_hole);
		translate([-$sensor_hole[0]/2 + $hole_pos[0], -$sensor_hole[1]/2 + $hole_pos[1], $z_ref + $sensor_hole[2]])
			cube([$sensor_hole[0], $sensor_hole[1], $base_cover_padding]);
		translate([$hole_pos[0], $hole_pos[1], $sensor_pcb_target_offset + $ball_size])
			sphere(r=($ball_size + $ball_clearance), $fn=50);
		translate([-$cover_size[0]/2 + $hole_pos[0], -$cover_size[1]/2 + $hole_pos[1], 0])
			v_holes($sensor_pcb_target_offset + $base_cover_padding);
	}
}

bearing_base();
