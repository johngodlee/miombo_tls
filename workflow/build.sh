#!/usr/bin/env sh

# Adjust images
gm convert +append ../img/P1_raw.png ../img/P1_pit.png ../img/P1_both.png

# Copy includes and images
cp ../out/clim.tex inc
cp ../img/subplot.pdf img
cp ../img/grass_3d.png img
cp ../img/crown.pdf img
cp ../img/noise_vis.png img
cp ../img/tls_hemi_compare.pdf img
cp ../img/map.png img
cp ../img/winkelmass.pdf img
cp ../img/P1_both.png img
cp ../img/height_profile_illus_all.pdf img
cp ../img/hemi_tls_ex.png img

# Adjust some snippets
sed -i '7s/.*/{Site} \& \\multicolumn{1}{p{2.5cm}}{\\centering MAT \\\\ (\\textdegree{}C)} \& \\multicolumn{1}{p{2.5cm}}{\\centering MAP \\\\ (mm y\\textsuperscript{-1})} \& \\multicolumn{1}{p{2.5cm}}{\\centering Temp. range \\\\ (\\textdegree{}C)} \& \\multicolumn{1}{p{2.5cm}}{\\centering CWD \\\\ (mm y\\textsuperscript{-1})} \\\\/' "inc/clim.tex"

