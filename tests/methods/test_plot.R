#' Function to compare a plot to a reference output
#'
#' The function writes out the plot to the temporary diretory and takes its md5. The test fails if the md5 of the image
#' is different than the one provided in the reference. If a reference does not exist, it will be written to the path.
#' IMPORTANT: The function works only on GNU LINUX
#' @param expr The plotting expression to be written.
#' @param path The path to the reference file.
#' @param dir The parent directory of path. Useful for cases when path is just a filename.
#' @param temp The directory to write out the new image file. Defaults to the temporary directory.
#' @param plotter The graphical device function to use for the plotting.
#' @param height The height argument of the graphical device.
#' @param height The width argument of the graphical device.
#' @param update Logical indicating whether the reference image needs updating.
test_plot <- function(expr, path, dir=NULL, temp=tempdir(), plotter=svg, height=10, width=10, update=FALSE){

	require(tinytest)


	# complete the path
	path <- file.path(dir, path)

	# split the reference path
	splitPath <- unlist(strsplit(path, "/"))

	# the file name
	filename <- splitPath[length(splitPath)]

	splitName <- unlist(strsplit(filename, "\\."))

	# the name of the plotting device
	plotter <- splitName[length(splitName)]

	# the device to be used
	plotter <- match.fun(plotter)

	# the directory where the file is/was supposed to be
	parent <- paste(splitPath[-length(splitPath)], collapse="/")


	# new file in the temporary directory
	tempFile <- file.path(temp, filename)

	# create the plot in the temorary directory
	plotter(tempFile, height=height, width=width)
		eval(expr, envir=parent.frame())
	dev.off()

	# test if file is present
	if(filename%in%list.files(parent)){
		# if need to be updated, copy over
		if(update){
			file.remove(path)
			file.copy(tempFile, path)
			message(paste0("Updating reference image '", path, "'."))
		# do a comparison
		}else{
			# get 2 md5s
			reference <- unname(tools::md5sum(path))
			temporary <- unname(tools::md5sum(tempFile))

			# test the image
			tinytest::expect_equal(reference, temporary)

		}

	# if not present, copy over
	}else{
		file.copy(tempFile, path)
		message(paste0("Creating reference image '", path, "'."))
	}
}
