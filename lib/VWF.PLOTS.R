##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################



#######################################################################################################################
##
##
##   generic plotting functions:
##
##      plot_ncdf(data, mainTitle, legendTitle, latRange, lonRange, zRange, zConstrain, levels, aspect, myColours)
##      .
##      .
##      .  data = a single hour of netcdf data e.g. w50m[lon, lat]
##		          the longitude and latitute are read from the dimension names of data
##      .
##      .  mainTitle = text to go above the main plot (i.e. your variable)
##      .  legendTitle = text to go above the legend scale (i.e. your units)
##      .
##      .  latRange and lonRange = optionally control the extent of the map, defaults to something sensible 
##      .  zRange = optionally control the range of values to be plotted, defaults to the full data range
##      .  zConstrain = set to TRUE to cap speed data to this range.  without this, the plot breaks outside the limits
##      .
##      .  levels = optionally control the number of colours to use in the plot, high numbers look smoother but render slower
##      .  aspect = optionally specify the aspect ratio for displaying and printing the map
##      .
##      .  myColours = optionally specify a colorRampPalette to use, defaults to some nice heat colours
##
##
##      full_ncdf(data, ..., dateTime) - same as above but with no axes, legends, etc.  suitable for converting into videos
##      .
##      . dateTime = set to TRUE to add three boxes in the southern oceans giving the current date and time
##
##
##      print_ncdf(filename, data, width, height, res, ...) - draws a fullscreen plot straight to file - saving on rendering time
##
##
##
##
##
##   wind plotting functions: (same as above, but with good formatting for wind speeds)
##
##      plot_ncdf_wind(data)
##      full_ncdf_wind(data)
##      print_ncdf_wind(filename, data)
##
##
##
##
##   then there are some testing functions (looking at other ways to plot, which aren't as good)
##   and the base function that they all rely on:
##
##      filled.contour.is(...)
##
##





#######################################################################################################################
##
##  GENERIC PLOTTING FUNCTIONS
##
##
##
##  a generic function for plotting any NetCDF variable
##
plot_ncdf = function(data, mainTitle='', legendTitle='', lonRange=NULL, latRange=NULL, zRange=range(data), zConstrain=FALSE, levels=64, aspect=NULL, myColours=NULL)
{
	# get our world map shape (low res, for faster drawing)
	if (!exists('world.map'))
		world.map <<- get_world_map(res='low')


	# get our coordinates
	lon = as.numeric(dimnames(data)[[1]])
	lat = as.numeric(dimnames(data)[[2]])


	# calculate our default lon & lat ranges if needed
	if (is.null(lonRange))
		lonRange = range(lon)

	if (is.null(latRange))
		latRange = range(lat)


	# colour scheme for the plot
	if (is.null(myColours))
	{
		heat.colours = c('#3F168A', '#2049D0', '#3288BD', '#66C2A5', '#ABDDA4', '#E6F598', '#FFFFBF', '#FEE08B', '#FDAE61', '#F46D43', '#D53E4F', '#9E0142')
		myColours = colorRampPalette(heat.colours)
	}
		

	# constrain data to our desired zRange
	if (zConstrain)
	{
		data[data < zRange[1] ] = zRange[1];
		data[data > zRange[2] ] = zRange[2];
	}


	# aspect ratio of the map
	if (is.null(aspect))
		aspect = 2.5 * (max(lat)-min(lat)) / (max(lon)-min(lon))


	# plot
	filled.contour.is(lon, lat, data, 
		nlevels=levels, color.palette=myColours,
		asp=aspect, xlim=lonRange, ylim=latRange, zlim=zRange,
		key.title = title(main=legendTitle), plot.title = title(main=mainTitle, xlab='Longitude', ylab='Latitude'),
		plot.axes = { plot(world.map, xlim=lonRange, ylim=latRange, add=TRUE); axis(1); axis(2) }
	)
}





##
##  a generic function for plotting any NetCDF variable fullscreen
##
full_ncdf = function(data, mainTitle='', legendTitle='', lonRange=NULL, latRange=NULL, zRange=range(data), zConstrain=FALSE, levels=1024, aspect=NULL, myColours=NULL, dateTime=FALSE)
{
	# get our world map shape (high res for better visuals)
	if (!exists('world.map'))
		world.map <<- get_world_map(res='high')


	# get our coordinates
	lon = as.numeric(dimnames(data)[[1]])
	lat = as.numeric(dimnames(data)[[2]])


	# calculate our default lon & lat ranges if needed
	if (is.null(lonRange))
		lonRange = range(lon)

	if (is.null(latRange))
		latRange = range(lat)


	# colour scheme for the plot
	if (is.null(myColours))
	{
		heat.colours = c('#3F168A', '#2049D0', '#3288BD', '#66C2A5', '#ABDDA4', '#E6F598', '#FFFFBF', '#FEE08B', '#FDAE61', '#F46D43', '#D53E4F', '#9E0142')
		myColours = colorRampPalette(heat.colours)
	}
		

	# constrain data to our desired zRange
	if (zConstrain)
	{
		data[data < zRange[1] ] = zRange[1];
		data[data > zRange[2] ] = zRange[2];
	}


	# aspect ratio of the map
	if (is.null(aspect))
		aspect = 2.5 * (max(lat)-min(lat)) / (max(lon)-min(lon))


	# remove the borders for full-screen
	omar <- par(mar = par('mar'))
	on.exit(par(omar))
	par(mar=rep(0,4))


	# plot
	filled.contour.is(lon, lat, data, 
		nlevels=levels, color.palette=myColours,
		asp=aspect, xlim=lonRange, ylim=latRange, zlim=zRange,
		plot.axes = { plot(world.map, xlim=lonRange, ylim=latRange, add=TRUE) },
		axes=FALSE, add.legend=FALSE
	)

	# add datetime
	if (dateTime == TRUE)
	{
		# box centres
		bx = c(-120,0,120)
		by = c(-50,-50,-50)

		# box width & height
		bw = 25
		bh = 5

		# plot rectangle and text
		rect(bx-bw/2, by-bh/2, bx+bw/2, by+bh/2, col=rgb(1,1,1,0.4))
		text(bx, by, t, adj=c(0.5,0.5))
	}

}



##
##  a generic function for plotting any NetCDF variable directly to a PNG file
##
print_ncdf = function(filename, data, width, height, res, ...)
{
	png(filename, height=height, width=width, res=res)

	full_ncdf(data, lon, lat, ...)

	dev.off()	
}






#######################################################################################################################
##
##  WIND PLOTTING FUNCTIONS
##
##
##
## defaults for plotting wind speeds
##   legend is fixed to 0-30 m/s - plot is done in blues
##
plot_ncdf_wind = function(data, mainTitle='', legendTitle='m/s', lonRange=NULL, latRange=NULL, zRange=c(0,30), zConstrain=FALSE, levels=64, aspect=NULL, myColours=NULL)
{
	# default colour scheme
	if (is.null(myColours))
		myColours = colorRampPalette(c('#F7FBFF', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B'))

	# plot it
	plot_ncdf(data, mainTitle, legendTitle, lonRange, latRange, zRange, zConstrain, levels, aspect, myColours)
}


full_ncdf_wind = function(data, latRange=NULL, lonRange=NULL, zRange=c(0,30), zConstrain=FALSE, levels=1024, aspect=NULL, myColours=NULL, dateTime=FALSE)
{
	# default colour scheme
	if (is.null(myColours))
		myColours = colorRampPalette(c('#F7FBFF', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B'))

	# plot it
	full_ncdf(data, mainTitle, legendTitle, latRange, lonRange, zRange, zConstrain, levels, aspect, myColours)
}


print_ncdf_wind_axes = function(filename, data, width, height, res, ...)
{
	png(filename, height=height, width=width, res=res)

	plot_ncdf_wind(data, ...)

	dev.off()	
}

print_ncdf_wind_full = function(filename, data, width, height, res, ...)
{
	png(filename, height=height, width=width, res=res)

	full_ncdf_wind(data, ...)

	dev.off()	
}






#######################################################################################################################
##
##  a better filled contour function for R 3.x.x
##  iain staffell ~ 2014
##
##  by default it no longer draws borders between levels in the legend
##  this lets you draw smooth plots with lots of levels..
##
##  if you run with the new option add.legend=FALSE, you just end up with
##  the level plot.  this means subsequent calls to points(), lines(), etc..
##  will work as expected, as you remain in the coordinate system of the plot body
##
##  if you call par(mar=rep(0,4)), then call this with axes=FALSE, add.legend=FALSE
##  you can obtain a full-screen plot with no borders, no nothing. 
##
filled.contour.is = function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, add.legend=TRUE, ...) 
{
    # sort out data
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")

    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))

    # plot legend
    if (add.legend)
    {
        w <- (3 + mar.orig[2L]) * par("csi") * 2.54
        layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
        par(las = las)
        mar <- mar.orig
        mar[4L] <- mar[2L]
        mar[2L] <- 1
        par(mar = mar)

        plot.new()
        plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
        rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = NA)  ## this removes legend borders
        if (missing(key.axes)) {
            if (axes) 
                axis(4)
        }
        else key.axes
        box()
        if (!missing(key.title)) 
            key.title
        mar <- mar.orig
        mar[4L] <- 1
        par(mar = mar)
    }

    # plot body
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}

