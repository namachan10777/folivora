$plate_thickness = 6;
$plate_tilt = 12;
$square_length = 22;
$square_width = 20;

module square() {
	$square = [$square_length, $square_width, $plate_thickness];
	$hole_size = 14.6;
	$frange_hole_size = 14.0;
	$frange_thickness = 1.3;
	
	union() {
		difference() {
			cube($square);

			translate([($square_length - $hole_size) / 2, ($square_width - $hole_size) / 2, 0]) {
				cube([$hole_size, $hole_size, $plate_thickness - $frange_thickness]);
			}
			translate([($square_length - $hole_size) / 2, ($square_width - $frange_hole_size) / 2, $plate_thickness - $frange_thickness]) {
				cube([$hole_size, $frange_hole_size, $frange_thickness]);
			}
		}
	}
}

module rightColumn() {
	square();
	translate([$square_length, 0, 0]) {
		rotate([0, -$plate_tilt, 0]) {
			square();
			translate([$square_length, 0, 0]) {
				rotate([0, -$plate_tilt, 0]) {
					square();
				}
			}
		}
	}
}

module middleColumn() {
	square();
	translate([$square_length, 0, 0]) {
		rotate([0, -$plate_tilt, 0]) {
			square();
			translate([$square_length, 0, 0]) {
				rotate([0, -$plate_tilt, 0]) {
					square();
				}
			}
		}
	}
	translate([0, 20, 0]) {
		rotate([0, -$plate_tilt, 180]) {
			square();
		}
	}
}

module leftColumn() {
	square();
	translate([$square_length, 0, 0]) {
		rotate([0, -$plate_tilt, 0]) {
			square();
		}
	}
	translate([0, 20, 0]) {
		rotate([0, -$plate_tilt, 180]) {
			square();
		}
	}
}

rightColumn();
translate([3, 20, -2]) {
	middleColumn();
}
translate([6, 40, -4]) {
	middleColumn();
}
translate([2, 60, -2]) {
	middleColumn();
}
translate([0, 80, 0]) {
	leftColumn();
}
translate([-3, 100, 0]) {
	leftColumn();
}
