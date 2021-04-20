#!/usr/bin/env sh

drawio() {
	/Applications/draw.io.app/Contents/MacOS/./draw.io --crop -x -o $2 $1 \;
}

drawio drawio/subplot.drawio img/subplot.pdf
drawio drawio/tls.drawio img/tls.pdf
drawio drawio/crown.drawio img/crown.pdf
