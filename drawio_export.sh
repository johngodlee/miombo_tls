#!/usr/bin/env sh

drawio() {
	/Applications/draw.io.app/Contents/MacOS/./draw.io --crop -x -o $2 $1 \;
}

drawio drawio/subplot.drawio workflow/img/subplot.pdf
