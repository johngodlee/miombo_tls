// User inputs
///////////////////////////////////
input_path = "/Users/johngodlee/google_drive/phd/thesis/lidar/dat/raw/hemi_photos/jpg/";
output_path = "/Users/johngodlee/google_drive/phd/thesis/lidar/dat/hemi_png/";
algorithm = "Default"
///////////////////////////////////
// END user inputs

list = getFileList(input_path);

for (i=0; i<(list.length); i++){
	open(""+input_path+list[i]+"");
	file_name = getInfo("image.filename");
	run("Split Channels");
	selectWindow(file_name+" (blue)");
	setAutoThreshold(algorithm);
	setOption("BlackBackground", true);
	run("Invert LUT");
	run("Convert to Mask");
	saveAs("png", ""+output_path+file_name+"");
	close("*");
}
