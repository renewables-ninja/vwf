##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################




#######################################################################################################################
##
##  global storage:
##    
##    farms = data frame of wind farms
##            essential columns are name, lon, lat, height, model
##
##
##  reading in wind farm csv data:
##
##    prepare_windfarms()     - check all your wind farms are valid and prepares them for use
##    prepare_farms_files()
##
##  plotting and locating farms:
##
##    plot_farms(wf, cap.weight=FALSE, add.points=FALSE, ...)
##
##    define_farm_region(wf, margin=10)
##
##    find_nearest_farm(myfarm, wf, cols)
##    find_farm_outliers(wf)
##    locate_farm_countries(wf)
##



# THIS IS NEXT LEVEL VERSION 5 SHIT

#
#  READS IN THE WIND FARMS
#  CHECKS THEY ARE VALID
#  RETURNS THEM
#
prepare_windfarms = function(windFarmInfo, powerCurveFile = 'E:/WORK/Wind Modelling/~ Wind Turbine Power Curves/R/Wind Turbine Power Curves ~ 5 (0.01ms with 0.00 w smoother).csv')
{

	# read in the .r file that gives the csv file path and the column names
	if (!file.exists(windFarmInfo))
		stop("prepare_windfarms: Cannot find your wind farms - please specify a different windFarmInfo:\n", windFarmInfo, "\n\n")
	
	source(windFarmInfo)



	# read in details on our wind farms
	wf = read_csv(fnchk(windFarmFile))
	colnames(wf) = windFarmCols


	# check our wind farm files have all the necessary columns
	essentialWindFarmCols = c('name', 'lon', 'lat', 'height', 'power_curve')

	check = essentialWindFarmCols %in% windFarmCols
	if (sum(check) < length(check)) stop("prepare_windfarms: You must have the following columns in your wind farms file:\n", paste(essentialWindFarmCols, collapse=', '), "\n\n")

	# and those columns have valid input
	for (e in essentialWindFarmCols)
	{
		bad = is.na(wf[ , e])
		if (sum(bad) > 0) stop("    prepare_windfarms: You must have the complete data for " %&% e %&% " in your wind farms file:\n\n")
	}


	# assign default capacity if none is specified
	if (is.null(wf$capacity))
	{
		cat("    prepare_windfarms: Farm capacity is not specified - setting all farms to 1 MW...\n")
		wf$capacity = 1
	}

	# assign default performance ratio if none is specified
	if (is.null(wf$PR))
	{
		cat("    prepare_windfarms: Farm performance ratio is not specified - setting all farms to 100% (i.e. no bias correction)...\n")
		wf$capacity = 1
	}

	# assign default start date if none is specified
	if (is.null(wf$date))
	{
		cat("    prepare_windfarms: Farm start date is not specified - setting all farms to be newborns...\n")
		wf$date = today()
	}


	# check our date formats -- dmy_hm -> dmy
	fix = grep(':', wf$date)
	if (length(fix) > 0)
	{
		wf$date[fix] = get_text_before(wf$date[fix], ' ')
	}

	# check our date formats -- ymd -> dmy
	fix = grep('-', wf$date)
	if (length(fix) > 0)
	{
		d = ymd(wf$date[fix])
		wf$date[fix] = format(d, '%d/%m/%Y')
	}




	# the file our power curves live in
	powerCurveFile = fnchk(powerCurveFile, stopOnError=FALSE)
	if (!file.exists(powerCurveFile)) stop("    prepare_windfarms: Cannot find your power curves - please specify a different powerCurveFile:\n", powerCurveFile, "\n\n")
	curveNames = read.csv(powerCurveFile, nrows=1)
	curveNames = colnames(curveNames)

	## TODO: HACK
	#repower_hack = (wf$power_curve == 'REpower 3.4M 104')
	#wf$power_curve[repower_hack] = 'REpower 3.4M104'


	err0r = 0
	for (i in 1:nrow(wf))
	{
		myModel = wf$power_curve[i]
		myModel = make.names(myModel)

		if ( !(myModel %in% curveNames) )
		{
			cat ("    prepare_windfarms: Couldn't find a power curve for", myModel, "\n")
			err0r = 1
		}
	}
	if (err0r)
	{
		cat ("prepare_windfarms: valid names are:\n")
		print(curveNames)
		stop("\n")
	}


	# convert wind farm names into valid column names (to match those in the wind speed file)
	wf$name = make.unique( make.names(wf$name) )


	# simplify the farm heights to speed up the extrapolation stage
	# have these sort-of equally spaced in log terms (~0.015 resolution in log10)
	h = wf$height

	h[ h >= 0   ] = 0.5 * round(h[ h >= 0   ] / 0.5)
	h[ h >= 30  ] =   1 * round(h[ h >= 30  ] / 1)
	h[ h >= 60  ] =   2 * round(h[ h >= 60  ] / 2)
	h[ h >= 90  ] =   3 * round(h[ h >= 90  ] / 3)
	h[ h >= 120 ] =   4 * round(h[ h >= 120 ] / 4)

	wf$height = h


	# and return
	wf

}







#######################################################################################################################
##
##  read a list of farms and coordinates
##  optionally specify the column names, if they are not the default
##
##  return the data
##
read_farms_file = function(filename, colnames=NA)
{
	# read the data
	data = read_csv(filename)

	# if colnames=NA, define default column names
	if (sum(is.na(colnames)) > 0)
		colnames = c('id', 'region', 'name', 'capacity', 'lat', 'lon', 'model', 'height', 'nTurbines')

	# check that the names match the data
	if (ncol(data) != length(colnames))
		stop("    read_farms_file -- data has ", ncol(data), " columns, but you want to name ", length(colnames), " of them!\n\n")

	# warn people if we don't have what I consider to be the necessary columns
	essentialCols = c('lon', 'lat', 'height', 'name');
	check = essentialCols %in% colnames;

	if (sum(check) < length(check))
		cat("    read_farms_file -- Woah, you ought to have the following columns in your coordinates files:\n", essentialCols, "\n\n");

	# apply
	colnames(data) = colnames
	return(data)
}







#######################################################################################################################
##
##  define the outer boundary box that contains a set of wind farm coordinates
##  box will contain all lat and lon coordinates within wf, plus a buffer of 'margin' degrees
##  the default margin of 10 degrees around all locations gives a minimal error when loessing
##  
##
##  returns a list: extent$lon[1:2] and extent$lat[1:2]
##
define_farm_region = function(wf, margin=10)
{
	lonBounds = range(wf$lon);
	lonBounds = range(c(lonBounds-margin, lonBounds+margin));
	if (lonBounds[1] < -180) lonBounds[1] = -180;
	if (lonBounds[1] > 180) lonBounds[1] = 180;

	latBounds = range(wf$lat);
	latBounds = range(c(latBounds-margin, latBounds+margin));
	if (latBounds[1] < -90) latBounds[1] = -90;
	if (latBounds[1] > 90) latBounds[1] = 90;

	return( list(lon=lonBounds, lat=latBounds) );
}




#######################################################################################################################
##
##  plot a set of wind farms on a map
##  optionally set cap.weight to a number to scale points by farm capacity (bigger number = bigger points)
##  optionally set other image properties (inherited from __inc.r:plot_map_points)
##  optionally add points to an existing plot
##
##  return the aspect ratio for saving an image
##
plot_farms = function(wf, cap.weight=1, add.points=FALSE, ...)
{
	# imported from __inc.r
	# plot_map_points = function(lon, lat, lonBounds=NA, latBounds=NA, padding=1,
	# 	                       mapBorder="darkgrey", mapFill="lightgrey", 
	# 	                       mapAspect=1.5, shapeFile=NA, 
	# 	                       resetPar=FALSE, blank=FALSE, ...)

	if (add.points == FALSE)
	{
		# plot all points the same size
		if (cap.weight == FALSE)
			ar = plot_map_points(wf$lon, wf$lat, ...)

		# size points proportional to capacity
		if (cap.weight != FALSE)
		{
			cap = wf$capacity 
			cap = sqrt(cap * cap.weight / mean(wf$capacity)) 

			ar = plot_map_points(wf$lon, wf$lat, cex=cap, lwd=cap*2, lend=1, ...)
		}

		# return the ideal height:width ratio for saving a PNG
		return(ar)

	}


	if (add.points == TRUE)
	{
		# plot all points the same size
		if (cap.weight == FALSE)
			points(wf$lon, wf$lat, ...)

		# size points proportional to capacity
		if (cap.weight != FALSE)
		{
			cap = wf$capacity 
			cap = sqrt(cap * cap.weight / mean(wf$capacity))

			points(wf$lon, wf$lat, cex=cap, lwd=cap*2, lend=1, ...)
		}

		return()

	}

}



#######################################################################################################################
##
## find the farm in wf that is physically closest to myfarm
## return a data frame showing the supplied cols for both farms
##
find_nearest_farm = function(myfarm, wf, cols = c('name', 'capacity', 'height', 'lat', 'lon'))
{
	w = which.min( (myfarm$lat - wf$lat)^2 + (myfarm$lon - wf$lon)^2)
	rbind(z[i, cols], o[w, cols])
}



#######################################################################################################################
##
##  analyse a set of farm coordinates
##  draw a map, highlighting any outliers
##
find_farm_outliers = function(wf)
{
	padding = 1

	# what are potentially sensible boundaries?
	latInner = quantile(wf$lat, c(0.01, 0.99))
	latInner = range(latInner - padding, latInner + padding)

	lonInner = quantile(wf$lon, c(0.01, 0.99))
	lonInner = range(lonInner - padding, lonInner + padding)


	# how many farms lie outside these boundaries?
	f1 = (wf$lat < latInner[1] | wf$lat > latInner[2])
	f2 = (wf$lon < lonInner[1] | wf$lon > lonInner[2])
	filter = f1 | f2

	if (sum(filter) == 0)
	{
		cat("\nDon't think I found any silly outliers..\n")
		return()
	}


	# show ones that aren't..
	# don't reset par so that we can add more points later
	plot_farms(wf, pointCol='darkgreen', resetPar=FALSE)

	# highlight the dodgy points
	points(wf[filter, "lon"], wf[filter, "lat"], pch=21, cex=1.2, col="red3", bg="pink")

	# identify them
	cat("\nFound", sum(filter), "potential outliers..")
	wf[filter, ]
	junk = readline(prompt = "Press ENTER to continue...")
}



#######################################################################################################################
##
##  locate which country each farm resides in - separating onshore and offshore
##
##  return a data frame of coordinates with country and region
##
locate_farm_countries = function(wf)
{
	source("E:/WORK/Code/R/Coordinates to Countries.r");

	# get our map from a custom shapefile which contains EN, WA, SC, NI
	shapeFile = 'E:/WORK/Z Data/Maps/Natural Earth/Admin Map Subunits/ne_10m_admin_0_map_subunits.shp'

	# set up our coordinates
	points = data.frame(lon = wf$lon, lat = wf$lat)

	# get a list of country names
	points = coords2country(points, offshore=TRUE, shapeFile)

	return(points)
}



