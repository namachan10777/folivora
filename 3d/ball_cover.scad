include <ball_const.scad>

module core() {
	difference() {
		cube([$cover_size[0], $cover_size[1], $ball_cover_t], center=true);
		translate([
			$hole_pos[0],
			$hole_pos[1],
			$ball_z - ($ball_cover_t + $pillar_size[2] + ($cover_size[2] - $bearing_in_r))
		])
			sphere(r=($ball_size+$ball_cover_clearance), $fn=100);
	}
}

difference() {
	core();
	translate([0, 0, -$ball_cover_t/2])
		v_holes($ball_cover_t);
}
