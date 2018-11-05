$top_t = 6;
$key_tilt = 12;
$key_col_size = 22;
$key_repeat = 19;
$key_row_size = $key_repeat+0.01;

module square() {
	$square = [$key_col_size, $key_row_size, $top_t];
	$hole_size = 14.6;
	$frange_hole_size = 14.0;
	$frange_thickness = 1.3;
	
	union() {
		difference() {
			cube($square);

			translate([($key_col_size - $hole_size) / 2, ($key_row_size - $hole_size) / 2, 0]) {
				cube([$hole_size, $hole_size, $top_t - $frange_thickness]);
			}
			translate([($key_col_size - $hole_size) / 2, ($key_row_size - $frange_hole_size) / 2, $top_t - $frange_thickness]) {
				cube([$hole_size, $frange_hole_size, $frange_thickness]);
			}
		}
	}
}

module rightColumn() {
	square();
	translate([$key_col_size, 0, 0]) {
		rotate([0, -$key_tilt, 0]) {
			square();
			translate([$key_col_size, 0, 0]) {
				rotate([0, -$key_tilt, 0]) {
					square();
				}
			}
		}
	}
}

module middleColumn() {
	square();
	translate([$key_col_size, 0, 0]) {
		rotate([0, -$key_tilt, 0]) {
			square();
			translate([$key_col_size, 0, 0]) {
				rotate([0, -$key_tilt, 0]) {
					square();
				}
			}
		}
	}
	translate([0, $key_row_size, 0]) {
		rotate([0, -$key_tilt, 180]) {
			square();
		}
	}
}

module leftColumn() {
	square();
	translate([$key_col_size, 0, 0]) {
		rotate([0, -$key_tilt, 0]) {
			square();
		}
	}
	translate([0, $key_row_size, 0]) {
		rotate([0, -$key_tilt, 180]) {
			square();
		}
	}
}

$key_offsets = [
	[0, 0],
	[3, -2],
	[6, -4],
	[2, -2],
	[0, 0],
	[-3, 0]
];

for (i = [0: 1: 5]) {
	translate([$key_offsets[i][0], $key_repeat * i, $key_offsets[i][1]]) {
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
	[0, 0, $top_t],
	[0, $key_row_size, $top_t],
	[
		$key_offsets[1][0] - $key_col_size * cos($key_tilt) + $top_t * sin($key_tilt),
		$key_row_size,
		$key_offsets[1][1] + $key_col_size * sin($key_tilt) + $top_t * cos($key_tilt)
	],
	[0, 0, 0],
	[0, $key_row_size, 0],
	[
		$key_offsets[1][0] - $key_col_size * cos($key_tilt),
		$key_row_size,
		$key_offsets[1][1] + $key_col_size * sin($key_tilt)
	],
];

polyhedron(
	points=$anchorage_vertexies,
	faces=[
		[4, 5, 3],
		[3, 1, 4],
		[3, 0, 1],
		[2, 5, 4, 1],
		[5, 2, 0],
		[3, 5, 0],
		[2, 1, 0],
	]
);
