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

$column_xdiffs = [0, 3, 6, 2, 0, -3];
$column_zdiffs = [0, -2, -4, -2, 0, 0];

for (i = [0: 1: 5]) {
	translate([$column_xdiffs[i], $square_width * i, $column_zdiffs[i]]) {
		if (i == 0) {
			rightColumn();
		}
		else if (i < 4) {
			middleColumn();
		}
		else {
			leftColumn();
		}
	}
}

$anchorage_vertexies = [
	[0, 0, $plate_thickness],
	[0, 20, $plate_thickness],
	[
		$column_xdiffs[1] - $square_length * cos($plate_tilt) + $plate_thickness * sin($plate_tilt),
		$square_width,
		$column_zdiffs[1] + $square_length * sin($plate_tilt) + $plate_thickness * cos($plate_tilt)
	],
	[0, 0, 0],
	[$column_xdiffs[1], $square_width, $column_zdiffs[1]],
	[
		$column_xdiffs[1] - $square_length * cos($plate_tilt),
		$square_width,
		$column_zdiffs[1] + $square_length * sin($plate_tilt)
	],
];

polyhedron(
	points=$anchorage_vertexies,
	faces=[
		[3, 5, 4],
		[4, 1, 3],
		[1, 0, 3],
		[1, 4, 5, 2],
		[0, 2, 5],
		[0, 5, 3],
		[0, 1, 2],
	]
);
