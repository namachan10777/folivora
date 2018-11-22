include <ball_const.scad>


module bearing_hole() {
	translate([-$bearing_in_r, -$shaft_length/2, $bearing_in_r]) {
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
	$bearing_spread_r =
		sqrt(
			pow($ball_size + $ball_clearance + $bearing_out_r, 2)
			- pow($ball_size - $base_cover_padding - $bearing_in_r, 2)
		);
	for (theta = [-30:120:240]) {
	rotate([0, 0, theta])
		translate([$bearing_spread_r, 0, 0])
			bearing_hole();
	}
}

module core() {
	difference() {
			translate([-$cover_size[0]/2, -$cover_size[1]/2, 0])
				cube($cover_size);
			translate($hole_pos) {
				bearing_holes();
				translate([0, 0, $ball_size - $base_cover_padding]) {
					sphere(r=($ball_size+$ball_clearance), $fn=100);
			}
		}
	}
}

difference() {
	$pillar_height = $ball_size - $base_cover_padding + $ball_cover_offset;
	union() {
		core();
		translate([-$cover_size[0]/2, -$cover_size[1]/2, 0])
			pillars($pillar_height);
	}
	translate([-$cover_size[0]/2, -$cover_size[1]/2, 0])
		v_holes($pillar_height);
}

