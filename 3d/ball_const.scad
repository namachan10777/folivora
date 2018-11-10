$cover_size = [57, 50, 4];
$hole_pos = [0, 0, 0];
$ball_size = 17;
$ball_clearance = 1;
$bearing_in_r  = 1.5;
$bearing_out_r = 3.5;
$bearing_clearance = 0.7;
$bearing_excursion = 4.2;
$shaft_length = 8;
$screw_hole_r = 1.5;
$ball_cover_t = 1.5;
$ball_cover_clearance = 0.1;
$base_t = 4;
$ball_cover_offset = 2;

$pcb_t = 1.6;
$sensor_pcb_target_offset = 7.40 - $pcb_t;
$sensor_guard_t = 2.4;
$sensor_hole = [12.96 + 0.5, 10.90 - 1.70, $sensor_guard_t];
$base_cover_padding = 4;
$base_junction_min_t = 3;
// computed value
$bearing_out_d = $bearing_out_r * 2;
$bearing_in_d = $bearing_in_r * 2;
$pillar_size = [8, 8];

module v_holes(h) {
	$hole_margin = 4;
	for (x = [$hole_margin, $cover_size[0] - $hole_margin]) {
		for (y = [$hole_margin, $cover_size[1] - $hole_margin]) {
			translate([x, y, 0]) rotate([0, 0, 90])
				cylinder(r=$screw_hole_r, h=h, $fn=50);
		}
	}
}

module pillars(h) {
	for (x = [0, $cover_size[0] - $pillar_size[0]]) {
		for (y = [0, $cover_size[1] - $pillar_size[1]]) {
			translate([x, y, 0])
				cube([$pillar_size[0], $pillar_size[1], h]);
		}
	}
}
