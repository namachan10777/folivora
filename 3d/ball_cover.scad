include <ball_const.scad>

module core() {
	difference() {
		translate([-$cover_size[0]/2, -$cover_size[1]/2, 0])
			cube([$cover_size[0], $cover_size[1], $ball_cover_t]);
		translate([
			$hole_pos[0],
			$hole_pos[1],
			-$ball_cover_offset
		])
			sphere(r=($ball_size+$ball_cover_clearance), $fn=100);
	}
}

difference() {
	core();
	translate([-$cover_size[0]/2, -$cover_size[1]/2, 0])
		v_holes($ball_cover_t);
}
