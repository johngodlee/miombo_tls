#!/usr/bin/env sh

drawio() {
	/Applications/draw.io.app/Contents/MacOS/./draw.io --crop -x -o $2 $1 \;
}

drawio drawio/plot.drawio img/plot.pdf
drawio drawio/winkelmass.drawio img/winkelmass.pdf
