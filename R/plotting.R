#' Colored points
#'
#' The function is a wrapper around the points function, controling the color of the points similar to \code{ggplot} but using S-style plotting.
#'
#' @param x The x argument of \code{points}.
#' @param y The y argument of \code{points}.
#' @param z Numeric, the variable to visualize using the colors.
#' @param ramp A calibramp object (including both breaks and colors).
#' @param col A vector of colors. Used only if ramp is not given.
#' @param breaks A vector of breaks. If given, this has to be one element longer than the length of col.
#' @param legend A list of arguments passed to the ramplegend function. Set to \code{NULL} if you do not want to plot a legend.
#' @param ... Arguments passed to \code{points}.
#' @export
colorpoints <- function(x, y=NULL, z, ramp=NULL, col=NULL, breaks=NULL, legend=list(x="topleft"), ...){
	# if a ramp object is given
	if(!is.null(ramp)){
		if(!inherits(ramp, "calibramp")) stop("The argument ramp has to be 'calibramp' class object.")
		breaks <- ramp$breaks
		col <- ramp$col
	}

	# if the breaks are not given, this has to done automatically
	# split up the range of z equally to as many bins as many are given
	if(is.null(breaks)){
		breaks <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=length(col)+1)
	}

	# check breaks!
	if(length(breaks)!=(length(col)+1)) stop("The number of breaks have to be one more than the number of colors.")

	# cut up wih breaks
	cutUp <- cut(z, breaks, labels=FALSE)

	# draw the actual points
	graphics::points(x=x, y=y, col=col[cutUp], ... )

	# draw a legend
	if(!is.null(legend)){
		# the basic configuration of the legend come from colorpoints
		basic <- list(breaks=breaks, col=col)

		# the extended arguments
		legendArgs<- c(basic, legend)

		do.call("ramplegend", legendArgs)

	}
}


#' Create a heatmap legend based on calibrated color ramp values
#'
#' @param x Position of the legend or the left coordinate of legend box.
#' @param y Coordinate of the upperleft coordinate of the legend (if needed).
#' @param shift Used instead of the inset argument of the default legend. If plotted within the inner plotting area, the x and y user coordinates with which the position of the legend will be shifted to be shifted.
#' @param ramp A calibrated color ramp object. Either \code{ramp} or both \code{col} and \code{breaks} are required.
#' @param col Vector of colors.
#' @param breaks Breaks between the colors.
#' @param zlim A numeric vector with two values, passed to \code{trimramp}.  The low and high extreme values to be shown on the legend.
#' @param height Height of the legend bar in inches.
#' @param width Width of the legend bar in inches.
#' @param tick.length The length of the legend's ticks.
#' @param cex Legend size scaler.
#' @param box.args the box's arguments.
#' @param horizontal Legend orientation. Not yet implemented!
#' @param at Where should the legend be drawn in the z dimension?
#' @param label What are the labels
#' @return The function has no return value.
#' @export
#' @examples
#' # Examples will come here
#' plot(1:10)
ramplegend <- function(x="topleft", y=NULL, shift=c(0,0), ramp=NULL, col=NULL, breaks=NULL, zlim=NULL, height=3, width=0.3,
	tick.length=0.15, cex=1, box.args=list(col="#ffffffbb"), horizontal=FALSE,
	at=NULL, label=NULL){

## x <- -156
## y <- 40
## height <- 3
## width <- 0.3
## tick.length <- 0.15
## col <- paleomap$col
## breaks <- paleomap$breaks
## cex <-0.5
## box.args=list(col="#bbbbbbbb")
## at <- c(5000, 3000, 1500, 0, -4000, -8000)
## label <- as.character(at)

	# either ramp or col+breaks
	if(!is.null(ramp)){
		if(!is.null(col) | !is.null(breaks)) stop("A 'ramp' was provided, 'col' and 'breaks' will be ignored.")
		# extract information from the ramp
		col <- ramp$col
		breaks <- ramp$breaks
	}else{
		if(is.null(col) & is.null(breaks)) stop("You have to provide both a 'col' and a 'breaks' argument.")
	}

	if(length(col)!=(length(breaks)-1)) stop("The 'col' vector has to be one element shorter than 'breaks'.")

	# automatic labeling
	if(is.null(at)){
		# calculate the pretty labels
		prettyLabs <- pretty(range(breaks))

		# within the range of the labels
		innerPretty <- prettyLabs[prettyLabs<max(breaks) & prettyLabs>min(breaks)]

		# the pretty lables become the indices
		at <- innerPretty

	}

	# if at is given but there are no labels
	if(!is.null(at) & is.null(label)){
		label <- as.character(at)
	}
	if(!is.null(at) & !is.null(label)){
		if(length(at) != length(label)) stop("The arguments 'at' and 'label' must have the same length.")

	}

	if(horizontal){
		stop("Not yet, this will be implemented in the next version.")
	}

	# the parameters of the current plot
	params <- par()

	# the user coordinates
	usr <- params$usr

	# the longest label
	longestChar <- max(nchar(label))
	labelOffsetX <- (longestChar)*params$cxy[1]


	# the range of the vars x and y
	usrange <- c(abs(usr[2]-usr[1]), abs(usr[4]-usr[3]))

	pin <- params$pin
	oneIn <- usrange/pin

	if(x=="topleft"){
		x <- usr[1]
		y <- usr[4]
	}
	if(x=="topright"){
		y <- usr[4]
		rightbound <- usr[2]

		# calculate the topright corner
		x <- rightbound - tick.length*oneIn[1]*cex - width*oneIn[1] - params$cxy[1]*2 - labelOffsetX
		x <- rightbound-(rightbound-x)*cex

	}
	if(x=="bottomleft"){
		x <- usr[1]
		bottombound <- usr[3]

		# calculate the topright corner
		y <- bottombound +params$cxy[1]*2 + height*oneIn[2] + params$cxy[1]*2
		y <- bottombound+ (y-bottombound)*cex

	}
	if(x=="bottomright"){
		# right
		rightbound <- usr[2]
		# calculate the topright corner
		x <- rightbound - tick.length*oneIn[1]*cex - width*oneIn[1] - params$cxy[1]*2 - labelOffsetX
		x <- rightbound-(rightbound-x)*cex

		# bottom
		bottombound <- usr[3]
		# calculate the topright corner
		y <- bottombound +params$cxy[1]*2 + height*oneIn[2] + params$cxy[1]*2
		y <- bottombound+ (y-bottombound)*cex

	}
	# shift the legend
	x <- x+ shift[1]
	y <- y+ shift[2]

	# get the bar
	box.left <- x
	box.top <- y


	# the bar's coordinates
	bar.left <- box.left+params$cxy[1]*2
	bar.top <- box.top-params$cxy[1]*2
	bar.bottom <- bar.top-height*oneIn[2]
	bar.right <- bar.left+width*oneIn[1]


	# modify and trim the color legend
	if(!is.null(zlim)){
		# the top

		# the bottom

	}

	# where are the given lables
	colScaler <- (bar.top-bar.bottom) / (max(breaks) - min(breaks))

	# the offsets in the Z dimension
	offsets <- breaks-min(breaks)
	newBreaks <- offsets*colScaler + bar.bottom

 	# the ticks
	tick.left <- bar.right
	tick.right <- tick.left+tick.length*oneIn[1]*cex

	text.right <- tick.right-tick.length*oneIn[1]*(1-cex)

	# the positions of the ticks
	tick.y <- bar.bottom + (at-min(breaks))*colScaler


	# assumes left to right
	box.right <- tick.right+labelOffsetX
	box.bottom <- bar.bottom-params$cxy[1]*2

	# the box
	boxArgs <- list(ybottom=y+cex*(box.bottom-y), ytop=y+cex*(box.top-y),
		 xleft=x+cex*(box.left-x), xright=x+cex*(box.right-x))
	boxArgs <- c(box.args, boxArgs)
	do.call("rect",boxArgs)

	# the boundaries of the rectangles
	graphics::rect(
		ybottom=y+cex*(newBreaks[2:length(newBreaks)-1]-y),
		ytop=y+cex*(newBreaks[2:length(newBreaks)]-y),
		xleft=x+cex*(bar.left-x), xright=x+cex*(bar.right-x), col=col, border=NA)

	# the border of the bar
	graphics::rect(ybottom=y+cex*(bar.bottom-y), ytop=y+cex*(bar.top-y),
		 xleft=x+cex*(bar.left-x), xright=x+cex*(bar.right-x), col=NA)

	# the ticks
	graphics::segments(x0=x+cex*(tick.left-x), x1=x+cex*(tick.right-x),
		y0=y+cex*(tick.y-y), y1=y+cex*(tick.y-y))

	# labels
#	text(x=x+cex*(tick.right+labelOffsetX/2-x),
#		y=y+cex*(tick.y-y), label=label, cex=cex*1.2)

	graphics::text(x=x+cex*(text.right-x),pos=4,
		y=y+cex*(tick.y-y), label=label, cex=cex*1.2)
}


#' Visualize a calibrated color ramp
#'
#' The method can be used to inspect a calbirated color ramp object.
#'
#' @param x The calibirated color ramp object (\code{calibramp}-class object).
#' @param ... Arguments passed to the \code{rampplot} function.
#' @param breaks Should the distribution of breaks be visualized?
#'
#' @export
#' @rdname rampplot
#' @examples
#' # the paleomap ramp
#' data(paleomap)
#' plot(paleomap)
#' # 0-calibrated, expanded ramp
#' tiepoints <- data.frame(z=c(c(-1, -0.1, 0, 0.1, +1)), color=gradinv(5))
#' ramp <- expand(tiepoints, n=255)
#' plot(ramp)
plot.calibramp<- function(x, ...){
	rampplot(x, ...)
}


#' @export
#' @rdname rampplot
rampplot <- function(x, breaks=FALSE){

	plot(NULL, NULL,
		xlim=c(-1,1), ylim=range(x$breaks), axes=FALSE,
		ylab="z", xlab="", xaxs="i", yaxs="i")
	graphics::rect(xleft=-1, xright=1,
		ybottom=x$breaks[-1],
		ytop=x$breaks[-length(x$breaks)],
		col=x$col, border=NA)
	graphics::axis(2)
	graphics::box()

	graphics::mtext(side=1, line=1, text=paste0("Minimum break value: ", min(x$breaks)))
	graphics::mtext(side=3, line=1, text=paste0("Maximum break value: ", max(x$breaks)))

	if(breaks) graphics::abline(h=x$breaks, col="red")

}
