#' Limit range of values in a SpatRaster object
#' 
#' @param x object to limit.
#' @param y Range, numeric vector with two values, or data frame of positioned color values. 
#' @param min The minimum value to have in the raster.
#' @param max The maximum value to have in the raster.
#' 
#' @export
limit <- function(x, y=NULL, min=NULL, max=NULL){

	if(!requireNamespace("terra", quietly=TRUE)) stop("This function requires the 'terra' package. ")
		
	# if it is a ramp
	if(is.data.frame(y)) y <- range(y$z)
	
	# if these were not given, assign them
	if(is.null(min)) min <- sort(y)[1]
	if(is.null(max)) max <- sort(y)[2]

	# do the limiting of the raster
	terra::values(x)[which(terra::values(x)>max)]<-max
	terra::values(x)[which(terra::values(x)<min)]<- min

	return(x)
}
