include <ball_const.scad>

module core() {
	difference() {
		cube([$cover_size[0], $cover_size[1], $ball_cover_t], center=true);
		translate([$hole_pos[0], $hole_pos[1], $ball_z - $cover_size[2]/2 - $pillar_size[2]])
			sphere(r=($ball_size+$ball_cover_clearance), $fn=300);
	}
}

core();
