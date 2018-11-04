include <ball_const.scad>

module bearing_hole() {
	translate([-$bearing_in_r, -$shaft_length/2, 0]) {
		union() {
			translate([0, 0, -$bearing_in_r])
				cube([$bearing_in_d, $shaft_length, $bearing_in_r]);
			rotate([-90, 0, 0])
				translate([$bearing_in_r, 0, 0])
					cylinder(r=$bearing_in_r, h=$shaft_length, $fn=100);
			rotate([-90, 0, 0])
				translate([$bearing_in_r, 0, ($shaft_length - $bearing_excursion) / 2])
					cylinder(r=($bearing_out_r+$bearing_clearance), h=$bearing_excursion, $fn=100);
		}
	}
}

module bearing_holes() {
	for (theta = [0:120:240]) {
	rotate([0, 0, theta])
		translate([$bearing_spread_r, 0, 0])
			bearing_hole();
	}
}

module core() {
	difference() {
			translate([0, 0, ($cover_size[2] - $bearing_in_d) / 2])
				cube($cover_size, center=true);
			translate($hole_pos) {
				bearing_holes();
				translate([0, 0, $ball_z]) {
					sphere(r=($ball_size+$ball_clearance), $fn=100);
			}
		}
	}
}

difference() {
	$pillar_pos_xs = [-$cover_size[0]/2, $cover_size[0]/2-$pillar_size[0]];
	$pillar_pos_ys = [-$cover_size[1]/2, $cover_size[1]/2-$pillar_size[1]];
	$pillar_pos_z  = $cover_size[2]/2;
	union() {
		core();

		for (i = [0:1]) {
			for (j = [0:1]) {
				translate([$pillar_pos_xs[i], $pillar_pos_ys[j], $pillar_pos_z]) {
					cube($pillar_size);
				}
			}
		}
	}
	for (i = [0:1]) {
		for (j = [0:1]) {
			translate([$pillar_pos_xs[i]+$pillar_size[0]/2, $pillar_pos_ys[j]+$pillar_size[0]/2, -$cover_size[2]/2]) {
				cylinder(r=$screw_hole_r, h=($cover_size[2]+$pillar_size[2]), $fn=100);
			}
		}
	}
}
