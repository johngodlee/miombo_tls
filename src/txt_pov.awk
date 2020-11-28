BEGIN { 
	FS = ","
	print "union {" 
}
NR!=1 {
	printf "box { <%f,%f,%f>,<%f,%f,%f> }\n", $1-0.01, $2-0.01, $3-0.01, $1+0.01, $2+0.01, $3+0.01 ;
}
END { print "}" }
