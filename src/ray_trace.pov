#version 3.7;
#include "colors.inc"

global_settings { 
	assumed_gamma 1.0
	max_trace_level 20
}

camera {
    fisheye	
	angle 180
    right  x*image_width/image_height
	location <0,0,1.3>
	look_at <0,0,200>
}

background { White }

#include "/var/folders/_n/c5rvw2g9535118h_2t8bmhvh0000gn/T/tmp.jHVWVdI5je/P11S2.pov"
