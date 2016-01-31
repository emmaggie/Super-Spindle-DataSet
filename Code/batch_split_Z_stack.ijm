macro "Split Z-stack" {
// uncomment to run in batch mode (should be faster)
//setBatchMode(TRUE);

// open file dialog to choose direcctory
dir = getDirectory("Choose directory");

// change to select a different directory for saving files
saveDir = "/Volumes/Seagate_RED/A_Heald_Lab/A_My_papers/AA_TECH_PAPER/REVISION/Anne_Output";

// comment out to use a different saving directory
saveDir = dir;


// save file names in array
files = getFileList(dir);

// loop through file list
for(f=0; f<files.length; f++) {
	// check if current file is a LSM file; may not be necessary but avoids errors if there are text or single channel tiff files in the same folder
	if(endsWith(files[f],".lsm")==1) {
		// open file
		open(files[f]);
		// get name of just opened file
		
		imgname = getTitle();
		savename = replace(imgname,".lsm","");
		
		// split into single channels
		run("Stack to Images");
		
		n = nImages();
		for(i=0; i<n; i++){
			saveAs("Tiff",dir + savename  + '_' + i + ".tiff");
			close();
		}
		// because anne has DAPI already in the name, I had to replace it here so you don't have problems selecting by 'DAPI'in filenames later
		//savename = replace(imgname,"DAPI","nuclei");
		// just cosmetics - removes ".lsm" from the filename
		
		
		// select channels by imageJ naming convention: C1-... C2-... C3-...
		// channel 3 (C3-) is DAPI
		// select the window
		//selectWindow("C3-" + imgname);
		// save with -DAPI ending
		//saveAs("Tiff", saveDir + savename + "-DAPI" + ".tif");
		// close the window
		//close();

		// same for green channel; used "FITC" here but adjust as needed
		//selectWindow("C2-" + imgname);
		//saveAs("Tiff", saveDir + savename + "-FITC" + ".tif");
		//close();

		// same for red channel; used "TRITC" here but adjust as needed
		//selectWindow("C1-" + imgname);
		//saveAs("Tiff", saveDir + savename + "-TRITC" + ".tif");
		//close();
		}
	


}