#' Color gradient ramps
#'
#' The object contains functions produced by the \code{\link[grDevices:colorRamp]{colorRampPalette}} function.
#' 
# \cr
#' You can also view single palettes individually. The following color palettes are implemented:
#' \itemize{
#' \item \code{gradinv()}: inverse heatmap.
#' }
#' 
#' @return A function producing a color gradient ramp.
#' @name ramps
#' @param n (\code{numeric}) Number of different colors to generate from the palette
#' 
NULL


#' An inverse heatmap
#' @rdname ramps
#' 
#' @export 
#' @examples
#' cols <- gradinv(20)
#' plot(1:20, col=cols, pch=16, cex=2)
gradinv <- grDevices::colorRampPalette(c("#33358a", "#76acce", "#fff99a",  "#e22c28", "#690720"))


#' Topographic color palettes with tiepoints
#'
#' The object contains \code{data.frame}-class objects.
#'
#' @format A \code{list} with 4 \code{data.frame}s:
#' \describe{
#' \item{\code{jakarta}}{: The "Jakarta" theme, color values by \emph{Arcanographia}. }
#' \item{\code{havanna2}}{: The "Havanna-2" theme, color values by \emph{Arcanographia}.}
#' \item{\code{tokio1}}{: The "Tokio-1" theme, color values by \emph{Arcanographia}.}
#' \item{\code{zagreb}}{: The "Zagreb" theme, color values by \emph{Arcanographia}.}
#' }
#' @usage data(topos)
#' 
"topos"


#' Topographic gradient map color map of PALEOMAP project
#'
#' The elevation to color bindings are from Scotese, C. R., Vérard, C., Burgener, L., Elling, R. P., & Kocsis, A. T. (2025). The Cretaceous World: Plate Tectonics, Paleogeography, and Paleoclimate. Geological Society, London, Special Publications, 544(1), SP544–2024..
#' @format A \code{calibramp}-class \code{list} with 3 \code{numeric}s:
#' \describe{
#' \item{\code{col}}{: The color levels as hexadecimal RGB values. }
#' \item{\code{breaks}}{: The boundaries for the individual levels.}
#' \item{\code{mid}}{: The mid values of the color levels.}
#' }
#' @source \url{https://zenodo.org/records/10659112}
#'
"paleomap"

#' Create color ramp palette from fixed, color-tiepoint objects
#'
#' Function to create ramp palettes from fixed color positions
#'
#' @param x A \code{data.frame} object with two columns: \code{color} for hexadecimal color values, \code{z} for their position.
#' @param n Single integer number
#' @param color A \code{chraracter} value, the column name of the colors in \code{x}, defaults to \code{"color"}.
#' @param z A \code{chraracter} value, the column name of the values in \code{x}, defaults to \code{"z"}.
#' @param ... Arguments passed to the \code{colorRampPalette} function.
#'
#' @return A \code{list}-class object, with three elements: \code{$col} hexadecimal color values, \code{$mid}: z-values of midpoints (one for every color), and \code{$breaks}: separator borders between color values.
#' @examples
#' data(topos)
#' ramp <- expand(topos$havanna2, n=200)
#' plot(NULL, NULL, xlim=c(-1,1), ylim=c(-8000,5000))
#' rect(xleft=-2, xright=2, ybottom=ramp$breaks[-1], ytop=ramp$breaks[-length(ramp$breaks)],
#' col=ramp$col, border=NA)
#' @export
expand <- function(x, n, color="color", z="z", ...){
	if(any(!c(color, z)%in%colnames(x))) stop("'color' and 'z' 'x' must be the column names of 'x'.")

	# separate data
	color <- x[,color]
	z <- x[,z]

	# generate sequence between minimum and maximum
	centers <- seq(min(z), max(z), length.out=n)
	# difference between the bins
	di <- diff(centers)[1]

	# the breaks (ready for return)
	breaks <- c(centers-di/2, centers[length(centers)]+di/2)

	# find the closest midpoint to every given z value
	col <- rep(NA, n)

	# where are the given color values
	index <- rep(NA, length(z))
	for(i in 1:length(z)){
		# differences
		absDiffs <- abs(z[i] - centers)
		index[i] <- which(min(absDiffs)==absDiffs)[1]
	}

	# construct the ramps
	for(i in 2:length(index)){
		# the indices where this should go
		fit <- index[i-1]:index[i]
		# generate ramp generator function
		thisChunkFunc <- grDevices::colorRampPalette(c(color[i-1], color[i]), ...)
		# construct this part of the ramp
		colorSubset <- thisChunkFunc(length(fit))

		# fit this bit into the complete palette
		col[fit] <- colorSubset
	}
	
	y<- list(
		col=col,
		breaks=breaks,
		mid=centers
	)
	class(y) <- "calibramp"

	return(y)
}
