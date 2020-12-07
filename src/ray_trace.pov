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
	location <0,0,1.8>
	look_at <0,0,200>
}

background { White }

#include "../dat/tls/denoise_laz/P15S2.pov"
