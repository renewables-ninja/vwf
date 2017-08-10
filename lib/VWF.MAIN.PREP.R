##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################



#####
## ##  BIAS CORRECTION FUNCTIONS
#####


	# decide where to get performance ratio data for each farm
	# either set PR_column to be a column name in WindFarmFile to use the csv data
	# or set to NULL to use hard-coded defaults...
	PR_column = 'PR'



	# determine the scalar for each farm (if we are using fixed scalars)
	# this gives the number that we multiply all wind speeds by, before finding the offset which matches our desired capacity factor
	determine_farm_scalar = function(PR, iso='')
	{
		# if we are using the same scalar for every farm
		if (match_method == 1)
			scalar = 0.65

		# if we are basing the scalar on the farm's PR
		if (match_method == 2)
		{
			scalar = (scalar_alpha * PR) + scalar_beta

			# some crazy post-hacking from the Christian Grams era...
			# updated for Axpo 2017...
			# this relies on having 0.50 x 0.333 as your parameters
			if (iso == 'DE') scalar = scalar + 0.10
			if (iso == 'DK') scalar = scalar + 0.05
			if (iso == 'ES') scalar = scalar - 0.05
			if (iso == 'FR') scalar = scalar - 0.03
			if (iso == 'GB') scalar = scalar - 0.06
			if (iso == 'GR') scalar = scalar + 0.12
			if (iso == 'IE') scalar = scalar + 0.05
			if (iso == 'IT') scalar = scalar - 0.08
			if (iso == 'SE') scalar = scalar + 0.46

			# WTF with Sweden ey?
		}

		return(scalar)
	}



	# determine the offset for each farm (if we are using fixed offsets)
	# this gives the number we subtract from all wind speeds (in m/s), before finding the scalar which matches our desired capacity factor
	determine_farm_offset = function(PR)
	{
		# if we are using the same scalar for every farm
		if (match_method == 3)
			offset = 1.00

		# if we are basing the scalar on the farm's PR
		if (match_method == 4)
			offset = 3.00 * PR^0.333 / 0.85^0.333

		return(offset)
	}



	# which matching method should we use?
	#
	# 1 = fixed scalar same for every farm, find offset  <--- this is the first method i used for the decline paper
	# 2 = fixed scalar from each farm's PR, find offset
	# 3 = fixed offset same for every farm, find scalar
	# 4 = fixed offset from each farm's PR, find scalar  <--- this is the method being used in the most recent code
	#
	# offsets will add or subtract a fixed number to all wind speeds (in m/s)
	# scalars will multiply all speeds by a constant (dimensionless)
	#
	# here we set the default value :)
	#
	if (!exists('match_method')) match_method = 2



	# specify the standard deviation of the convolution
	# used to create the multi-turbine farm curve
	# (this should be improved!)
	get_power_curve_convolution_spread = function(farm)
	{
		return(convolverStdDev)

		# check either tCount or nTurbines
		# and adjust based on that
	}



	farms_inflate_offshore_pr = function(farms, offshore_factor=1.16, column='offshore')
	{
		if (column %notin% colnames(farms))
			stop("farms_inflate_offshore_pr: cannot find the " %&% column %&% " column...\n")
		
		onshore = farms[ , column] %in% c('No', 'no')
		offshore = !onshore

		farms$PR[offshore] = farms$PR[offshore] * offshore_factor

		farms
	}




#####
## ##  CHECK OUR INPUTS AND OUTPUTS
#####

	# check we can save files
	if (substr_reverse(baseSaveFolder, 1) != '/')
		baseSaveFolder = baseSaveFolder %&% '/'

	if (!dir.exists(baseSaveFolder))
	{
		ok = dir.create(baseSaveFolder)

		if (!ok)
			stop("Could not create folder to save files in:", baseSaveFolder, "\n")
	}




#####
## ##  SORT OUT OUR WIND FARM DATA
#####

	##
	##  read in our wind farms data
	##

	flush("> Preparing your wind farms...\n")

	windFarms = prepare_windfarms(farmOptionsFile)

	# inflate offshore PR
	if (inflate_offshore_pr)
	{
		windFarms = farms_inflate_offshore_pr(windFarms, 1.16)
	}


	# calculate the boundaries for lat and long that we're interested in
	# a margin of 10 degrees around all farms should eliminate interpolation errors
	region = define_farm_region(windFarms, margin=6.666)

	
	## TODO: FIX -- SHOULD CHECK IF THIS REALLY NEEDS TO BE 6.666 DEGREES... 


	# plot on a map to check we're being sensible
	if (a_few_plots)
		farm_plot_aspect = plot_farms(windFarms, pch='+', col='green3', cap.weight=TRUE)



	# make a list of all wind farm heights
	all_heights = sort(unique(windFarms$height))




	##
	##  read in power curve data
	##

	# read in power curves for each turbine model
	# this must run from 0 to 40 m/s - and will be interpolated to 0.01 m/s steps
	turbCurve = read_csv(turbCurveFile)
	turbCurveNames = colnames(turbCurve)[-1]

	farmCurve = read_csv(farmCurveFile)
	farmCurveNames = colnames(farmCurve)[-1]


	# high-res interpolation
	turbCurve = interpolateTable(turbCurve, seq(0,40,0.01))
	farmCurve = interpolateTable(farmCurve, seq(0,40,0.01))




	# determine whether we need special measures for crossing the international date line...
	dateLineMadness = FALSE

	if (min(region$lon) < -180 | max(region$lon) > 180)
	{
		dateLineMadness = TRUE

		# remember our original lon range
		region_lon = region$lon
		if (region_lon[1] < -180) region_lon[1] = region_lon[1] + 360
		if (region_lon[2] > 180) region_lon[2] = region_lon[2] - 360

		# grab the entire range of lon from our NetCDF files
		region$lon = c(-180, 180)
	}






#####
## ##  SORT OUT OUR MERRA WIND DATA
#####

	if (is.null(windSpeedFile))
	{
		# find and prepare all our extrapolation data files
		flush("> Locating MERRA data")
		merra_wind = prepare_merra_files(merraFolders, merra_grouping, processNewestFirst)
		flush(" - Found", length(merra_wind$files), "days...\n")


		# create a NetCDF file handler for the wind data
		fn = merra_wind$files[1]
		nc_wind = NetCdfClass(fn, reanalysis)


		# set up the region we want to read (so that future reads are faster)
		nc_wind$subset_coords(region)


		flush("> Filtering NetCDF dataset down to", round(100 * diff(region$lon) * diff(region$lat) / 360 / 180, 1), "% of the earth's surface..\n")
	}




#####
## ##  SORT OUT OUR DATA OBJECTS FOR INTERPOLATION AND EXTRAPOLATION
#####

	##  INTERPOLATION METHOD

	# either 'akima' or 'loess'
	spatial.method = 'akima'


	if (is.null(windSpeedFile))
	{

		if (spatial.method == 'loess')
		{
			### loess and tps require data to be passed as a 1d vector, whereas the merra data comes as a 2d array [lon, lat]
			### so we pre-allocate some storage in 'long format' as a keyed vector

			# read in a merra variable
			loess_w = nc_wind$get_var('A')
			loess_w = loess_w[ , , 14]
			nc_wind$close_file()


			# create long-format storage for each of our input variables (holding all grid points for a single hour)
			# FIXME i should be using melt for this, i now know ;)
			loess_w = reshapeData(loess_w, nc_wind$lon, nc_wind$lat)
			colnames(loess_w) = c("lon", "lat", "value")

			# required by thin plate splines (fastTps)
			tps_xy_in = as.matrix(loess_w[ , 1:2])
			tps_xy_out = as.matrix(windFarms[ , c('lon', 'lat')])


			### loess requires a fitting paramter - how many nearest neighbours to include in the fit

			# determine our loess span based on the geographic area we cover
			# 12 merra grid points gives almost the minimum RMS error (10 or 11 is better, but slower)
			loessSpan = 12 / nrow(loess_w)

			### FIXME ### IMPROVE THIS ### FIXME ###
			loessSpan = sqrt(0.1 * loessSpan)
		}




		# if we are crossing the date line, set up the way we will process our data
		if (dateLineMadness)
		{
			# figure out where we want both sides of the date-line
			dateLineWest = which(nc_wind$lon >= region_lon[1])
			dateLineEast = which(nc_wind$lon <= region_lon[2])
			dateLineDims = c(nc_wind$lon[dateLineWest], nc_wind$lon[dateLineEast] + 360)
			originalDims = nc_wind$lon
		}

	}






#####
## ##  LAUNCH THE MULTI-CORE CLUSTER
#####

	# build a multi-core parallel cluster
	if (exists('cl'))
	{
		flush("> Reusing existing", length(cl), "core cluster...\n")

	} else {

		flush("> Building", n.cores, "core cluster...\n")
		cl = makeCluster(n.cores)
		registerDoParallel(cl)
	}

	if (spatial.method == 'akima')
	{
		library(akima)
		clusterExport(cl, varlist='bicubic')
	}

	if (spatial.method == 'loess')
	{
		library(MASS)
	}







	# the VWF model should now be ready to run...
