#!/usr/bin/env sh

# Copy includes and images
cp ../out/clim.tex inc
cp ../img/subplot.pdf img
cp ../img/grass_3d.png img
cp ../img/crown.pdf img
cp ../img/noise_vis.png img
cp ../img/tls_hemi_compare.pdf img
cp ../img/map.png img

# Adjust some snippets
sed -i '7s/.*/{Site} \& \\multicolumn{1}{p{2.5cm}}{\\centering MAT \\\\ (\\textdegree{}C)} \& \\multicolumn{1}{p{2.5cm}}{\\centering MAP \\\\ (mm y\\textsuperscript{-1})} \& \\multicolumn{1}{p{2.5cm}}{\\centering Temp. range \\\\ (\\textdegree{}C)} \& \\multicolumn{1}{p{2.5cm}}{\\centering CWD \\\\ (mm y\\textsuperscript{-1})} \\\\/' "inc/clim.tex"

