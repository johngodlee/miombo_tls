#!/usr/bin/env sh

tlsin="../dat/tls/hemi/P15S3.png"
hemin="../dat/hemi_png/DSC_2076.png"
hemcp="../img/DSC_2076_crop.png"
joind="../img/hemi_tls_ex.png"

# Crop hemi photo to sqaure
gm convert $hemin -gravity center -crop 4016x4016+0+0 -strip $hemcp

# Join images
gm convert +append $hemcp $tlsin $joind
