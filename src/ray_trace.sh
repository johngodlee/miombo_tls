#!/usr/bin/env bash

if [ $# -ne 2 ]; then
	printf "Must supply two arguments:\n  [1] input.pov\n  [2] output.png\n"
    exit 1
fi

sed -i "19s|.*|#include \"$1\"|" ray_trace.pov

povray ray_trace.pov Width=4016 Height=4016 Output_File_Name=$2 

mogrify -monochrome $2
