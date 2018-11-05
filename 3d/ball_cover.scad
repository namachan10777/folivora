include <ball_const.scad>

module core() {
	difference() {
		cube([$cover_size[0], $cover_size[1], $ball_cover_t], center=true);
		translate([$hole_pos[0], $hole_pos[1], $ball_z - $cover_size[2]/2 - $pillar_size[2]])
			sphere(r=($ball_size+$ball_cover_clearance), $fn=100);
	}
}

difference() {
	core();
	$screw_xs = [-$cover_size[0]/2+$pillar_size[0]/2, $cover_size[0]/2-$pillar_size[0]/2];
	$screw_ys = [-$cover_size[1]/2+$pillar_size[1]/2, $cover_size[1]/2-$pillar_size[1]/2];
	for (i = [0:1]) {
		for (j = [0:1]) {
			translate([$screw_xs[i], $screw_ys[j], -$ball_cover_t/2])
				cylinder(r=$screw_hole_r, h=$ball_cover_t, $fn=50);
		}
	}
}
