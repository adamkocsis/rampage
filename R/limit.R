#' Limit range of values in a SpatRaster object
#' 
#' @param x object to limit, a \code{SpatRaster} object.
#' @param y Range, numeric vector with two values, or data frame of positioned color values. 
#' @param min The minimum value to have in the raster.
#' @param max The maximum value to have in the raster.
#' @return A \code{SpatRaster} object.
#' 
#' @export
#' @examples
#' # an example for spatraster limitation
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

#' Trimming a calibrated color ramp object.
#'
#' @param x A calibrated color ramp (e.g. \code{calibramp} class object.
#' @param low The minimum value in the calibrated ramp.
#' @param high The maximum value in the calibrated ramp.
#' @return A trimmed version of \code{x}
#' @export
#' @examples
#' data(paleomap)
#' trimmed <- trimramp(paleomap, low=-5000, high=5000)
trimramp <- function(x, low=NULL, high=NULL){
	if(is.null(low) & is.null(high)) stop("Please provide at least either a 'low' or a 'high' value.")
	# adjust the minimum
	if(!is.null(low)){
		if(length(low)>1 | !is.numeric(low)) stop("Please provide a single numeric 'low' argument.")
		if(!is.finite(low)) stop("The 'low' argument has to be a finite number.")

		if(low> max(x$breaks)) stop("The 'low' argument must be lower than the highest value in the breaks.")
		# needs to be added
		if(low< min(x$breaks)){
			# adjust the low
			x$breaks[which.min(x$breaks)] <- low
			x$mid[which.min(x$mid)] <- mean(sort(x$breaks)[1:2])

		# need to remove some entries
		}else{
			# the breaks
			index <- which(low < x$breaks)
			x$breaks <- c(low, x$breaks[index])

			# keep the mids and the cols
			x$col <- x$col[index-1]
			x$mid <- x$mid[index-1]

			x$mid[which.min(x$mid)] <- mean(sort(x$breaks)[1:2])

		}
	}

	# adjust the maximum
	if(!is.null(high)){
		if(length(high)>1 | !is.numeric(high)) stop("Please provide a single numeric 'high' argument.")
		if(!is.finite(high)) stop("The 'high' argument has to be a finite number.")
		if(high< min(x$breaks)) stop("The 'high' argument must be higher than the the lowest value in the breaks.")
		# needs to be added
		if(high>  max(x$breaks)){
			# adjust the low
			x$breaks[which.max(x$breaks)] <- high
			x$mid[which.max(x$mid)] <- mean(sort(x$breaks)[(length(x$breaks)-1):length(x$breaks)])

		# need to remove some entries
		}else{
			# the breaks
			index <- which(high > x$breaks)
			x$breaks <- c(x$breaks[index], high)

			# keep the mids and the cols
			x$col <- x$col[index]
			x$mid <- x$mid[index]

			x$mid[which.max(x$mid)] <- mean(sort(x$breaks)[(length(x$breaks)-1):length(x$breaks)])

		}
	}
	# return the changed object
	return(x)
}
