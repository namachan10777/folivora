include <ball_const.scad>

module core() {
	difference() {
		cube([$cover_size[0], $cover_size[1], $base_t], center=true);
		translate([$hole_pos[0], $hole_pos[1], $base_t/2 + $bearing_in_r + $ball_z])
			sphere(r=($ball_size+$ball_cover_clearance), fn=100);
		translate([0, 0, -$base_t/2])
			v_holes($base_t);
	}
}

module bearing_base() {
	translate([-$cover_size[0]/2, -$cover_size[1]/2, $cover_size[2]/2])
		core();
}
