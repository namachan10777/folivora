$cover_size = [50, 42, 4];
$hole_pos = [1, 0, 0];
$ball_size = 17;
$ball_clearance = 1;
$bearing_in_r  = 1.5;
$bearing_out_r = 3.5;
$bearing_clearance = 0.7;
$bearing_excursion = 4.2;
$bearing_spread_r = 18;
$shaft_length = 8;
$pillar_size = [6, 6, 11];
$screw_hole_r = 1.5;
$ball_cover_t = 1.5;
$ball_cover_clearance = 0.1;

// computed value
$bearing_out_d = $bearing_out_r * 2;
$bearing_in_d = $bearing_in_r * 2;

$ball_z = sqrt(pow($ball_size+$ball_clearance+$bearing_out_r, 2)-pow($bearing_spread_r,2));
