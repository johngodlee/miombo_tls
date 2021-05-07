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

#include "/Users/johngodlee/Desktop/test/P13S6.pov"
