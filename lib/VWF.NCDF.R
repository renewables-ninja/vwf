##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################



	suppressPackageStartupMessages(library(ncdf4))
	suppressPackageStartupMessages(library(lubridate))




#######################################################################################################################
##
##  locate all the MERRA files in the given folder(s) - note that all files are found, regardless of extension!
##  extract all their dates and create an index that groups them by 'monthly' or 'daily'
##  return a list with $files $dates $groups $index
##
##  if grouping='daily' then you get faux-groups which allows the group-loop code to still work
##
prepare_merra_files = function(folders, grouping='monthly', processNewestFirst=FALSE)
{
	merra = list()

	# find NetCDF files within our folders
	merra$files = list.files(folders, full.names=TRUE)

	if (length(merra$files) == 0)
		stop("prepare_merra_files -- no files found in the given folder:", folders, "\n\n")

	# extract the dates from files
	if (grepl('Nx', merra$files[1])) {
		merra$dates = get_text_between(merra$files, 'Nx.', '.nc')
	} else {
		merra$dates = get_text_between(merra$files, 'profile.', '.nc')
	}
	merra$dates = get_text_before(merra$dates, '.SUB')
	merra$dates = ymd(merra$dates)

	# create an index for grouping files
	merra$index = list()

	# group files by quarter
	if (grouping == 'quarterly')
	{
		all_quarters = paste0( year(merra$dates), 'Q', ceiling(month(merra$dates)/3))
		merra$groups = unique(all_quarters)

		for (g in merra$groups)
		{
			filter = which( all_quarters == g )
			merra$index[[g]] = filter
		}
	}

	# group files by months
	if (grouping == 'monthly')
	{
		merra$groups = floor_date(merra$dates, 'month')
		merra$groups = format(unique(merra$groups))

		for (g in merra$groups)
		{
			filter = which( year(merra$dates) == year(g) & month(merra$dates) == month(g) )
			merra$index[[g]] = filter
		}
	}

	# create a dummy $index which goes through each file individually
	if (grouping == 'daily')
	{
		merra$groups = format(merra$dates)

		for (g in merra$groups)
		{			
			filter = which(merra$dates == g)
			merra$index[[g]] = filter
		}
	}


	# run in reverse order if desired
	if (processNewestFirst == TRUE)
		merra$groups = rev(merra$groups)


	# return the list
	merra
}










#######################################################################################################################
##
##  a class for opening and reading NetCDF files
##
NetCdfClass = function(filename, model, printInfo=FALSE)
{

	#####
	## ##  MEMBERS
	#####

	NetCdf = list(
		
		ncdf = NULL,

		all_vars = NULL,
		all_desc = NULL,
		all_dims = NULL,

		date = NULL,
		hour = NULL,
		levels = NULL,

		lon = NULL,
		lat = NULL,
		subset = NULL,

		model = NULL,

		calendar = NULL
	)



	#####
	## ##  METHODS
	#####

	#
	# constructor
	#   opens file
	#   defines the latitude and longitudes contained within it
	NetCdf$constructor = function(filename, model, printInfo=FALSE)
	{
		# set our reanalysis model
		NetCdf$model = model

		# open - getting our date and time
		NetCdf$open_file(filename, printInfo)

		# get our coordinate system
		if (NetCdf$model == 'MERRA2')
		{
			NetCdf$lon = ncvar_get(NetCdf$ncdf, 'lon')
			NetCdf$lat = ncvar_get(NetCdf$ncdf, 'lat')
		}

		if (NetCdf$model == 'MERRA1')
		{
			NetCdf$lon = ncvar_get(NetCdf$ncdf, 'longitude')
			NetCdf$lat = ncvar_get(NetCdf$ncdf, 'latitude')
		}

		if (NetCdf$model == 'MERRA1NEW')
		{
			NetCdf$lon = ncvar_get(NetCdf$ncdf, 'XDim')
			NetCdf$lat = ncvar_get(NetCdf$ncdf, 'YDim')
		}

		if (NetCdf$model == 'CMIP5')
		{
			NetCdf$lon = ncvar_get(NetCdf$ncdf, 'lon')
			NetCdf$lat = ncvar_get(NetCdf$ncdf, 'lat')
		}		



		# get our pressure levels too, if present
		if ('levels' %in% names(NetCdf$ncdf$dim))
			NetCdf$levels = ncvar_get(NetCdf$ncdf, 'levels')

		# default to no subsetting
		NetCdf$subset = FALSE

		# save
		assign('NetCdf', NetCdf, envir=NetCdf)
	}


	#
	# open a NetCDF file
	# set ncdf (the file handle), date and hour
	#
	# state which reanalysis this is: 'MERRA1' or 'MERRA2'
	#
	# optionally describes the file contents
	#
	NetCdf$open_file = function(filename, printInfo=FALSE)
	{
		# open
		NetCdf$ncdf = nc_open(filename)
		nc = NetCdf$ncdf

		# list all the variables inside it
		for (i in 1:nc$nvar)
		{
			var = nc$var[[i]]
			push(NetCdf$all_vars, var$name)

			desc = var$longname %&% ' [' %&% paste(var$size[1], var$size[2], var$size[3], sep=', ') %&% ']'	
			push(NetCdf$all_desc, desc)
		}

		# list all the dimensions inside it
		for (i in 1:nc$ndim)
		{
			dim = nc$dim[[i]]
			push(NetCdf$all_dims, dim$name)
		}



		# get the date and time for this file
		if (NetCdf$model == 'MERRA2')
		{
			NetCdf$date = ncatt_get(nc, 0, 'RangeBeginningDate')$value
			NetCdf$date = ymd(NetCdf$date)

			NetCdf$hour = ncvar_get(nc, 'time') / 60
		}

		if (NetCdf$model == 'MERRA1')
		{
			NetCdf$date = ncatt_get(nc, 'time', 'units')$value
			NetCdf$date = strsplit(NetCdf$date, ' ')[[1]][3]
			NetCdf$date = ymd(NetCdf$date)

			NetCdf$hour = ncvar_get(nc, 'time')
		}

		if (NetCdf$model == 'MERRA1NEW')
		{
			NetCdf$date = ncatt_get(nc, 'TIME', 'units')$value
			NetCdf$date = strsplit(NetCdf$date, ' ')[[1]][3]
			NetCdf$date = ymd(NetCdf$date)

			NetCdf$hour = ncvar_get(nc, 'TIME') / 60
		}

		# get the date and time for this file
		if (NetCdf$model == 'CMIP5')
		{
			# note that CMIP is just completely stupid and has 360 day years... 
			# we embed a strange string into the data dimnames just for the record...
			# and we set up the NetCdf$calendar variable to allow for easy access to all the date components
			calendar = data.frame(index=1:14400, year=NA, season=NA, hour=NA)

			# build a calendar to know the years,seasons,hours
			calendar$year = rep(2026:2030, each=360*8)

			calendar$month = rep(1:12, each=30*8)

			calendar$season = calendar$month
			f = (calendar$season == 12)
			calendar$season[f] = 0
			calendar$season = floor(calendar$season / 3)  # winter = 0, spring = 1, summer = 2, autumn = 3

			calendar$hour = rep(c(3, 6, 9, 12, 15, 18, 21, 0), times=360*5)

			# assign
			NetCdf$hour = paste('Y', calendar$year, 'M', calendar$month, 'H', calendar$hour, sep='')

			NetCdf$calendar = calendar
		}


		# HACK - i didn't update the dates correctly in the MERRA2 profile files...
		# HACK - they are identifiable because they are called .profile.
		if (grepl('profile', filename))
		{
			# so... extract the date from the filename
			d = get_text_between(filename, 'profile.', '.nc')
			d = get_text_before(d, '.SUB')
			NetCdf$date = ymd(d)
		}



		# describe contents
		if (printInfo)
		{
			# annoyingly, this is now hyper-detailed...
			# print(NetCdf$ncdf)

			cat(nc$ndim, 'Dimensions:\n')
			for (i in 1:nc$ndim)
			{
				d = NetCdf$all_dims[i]

				cat('\t')

				if (d == 'time')
				{
					cat(sprintf('%10s', 'time'), '\t')

					t_min = suppressWarnings(min(NetCdf$hour))
					t_max = suppressWarnings(max(NetCdf$hour))

					cat(format(NetCdf$date, "%Y-%m-%d"), ' - ', t_min, ':', t_min)

				} else if (d == 'longitude' | d == 'lon')
				{
					lon = ncvar_get(NetCdf$ncdf, d)
					cat(sprintf('%10s', 'longitude'), '\t')
					cat(min(lon), ':', max(lon))

				} else if (d == 'latitude' | d == 'lat')
				{
					lat = ncvar_get(NetCdf$ncdf, d)
					cat(sprintf('%10s', 'latitude'), '\t')
					cat(min(lat), ':', max(lat))

				} else
				{
					cat(d)
				}

				cat('\n')
			}

			cat(nc$nvar, 'Variables:\n')
			for (i in 1:nc$nvar)
				cat('\t', sprintf('%10s', NetCdf$all_vars[i]), '\t', NetCdf$all_desc[i], '\n')
		}

		# save
		assign('NetCdf', NetCdf, envir=NetCdf)
	}


	#
	# close a NetCDF file
	#
	NetCdf$close_file = function()
	{
		nc_close(NetCdf$ncdf)
		assign('NetCdf', NetCdf, envir=NetCdf)
	}




	#
	# set up the internals so that future reads from NetCDF files are subsetted to a particular region
	# 
	# region is a list containing $lon[min,max] and $lat[min,max]
	#
	NetCdf$subset_coords = function(region)
	{
		# remove fools
		if (length(region$lon) != 2 | length(region$lat) != 2)
		{
			cat("NetCdf::subset_coords() -- must pass a list of $lon[min,max] $lat[min,max]\n")
			return()
		}

		# get our spatial resolution
		dx = diff(NetCdf$lon)[1]
		dy = diff(NetCdf$lat)[1]

		# expand the region to make sure we include the points adjacent to our range
		region$lon[1] = region$lon[1] - dx - 0.01
		region$lon[2] = region$lon[2] + dx + 0.01
		region$lat[1] = region$lat[1] - dy - 0.01
		region$lat[2] = region$lat[2] + dy + 0.01

		# define which parts of the file live within our region
		myLon = which(NetCdf$lon >= region$lon[1] & NetCdf$lon <= region$lon[2])
		myLat = which(NetCdf$lat >= region$lat[1] & NetCdf$lat <= region$lat[2])

		# store the start and count parameters to use when reading variables
		NetCdf$start = c(min(myLon), min(myLat), 1)
		NetCdf$count = c(length(myLon), length(myLat), -1)

		# chop down our lon and lat vectors to this region
		NetCdf$lon = NetCdf$lon[myLon]
		NetCdf$lat = NetCdf$lat[myLat]

		# and note that we have done this chopping
		NetCdf$subset = TRUE

		# save
		assign('NetCdf', NetCdf, envir=NetCdf)
	}




	#
	# read a variable from the open NetCDF file
	# if you have defined a region with subset_coords(), employ subsetted reading to save time
	#
	NetCdf$get_var = function(var)
	{
		# check the var we are reading exists
		var_ok = (var %in% NetCdf$all_vars)
		if (!var_ok)
		{
			# check if it's a stupid case error in MERRA
			var_ok = (toupper(var) %in% toupper(NetCdf$all_vars))
			if (!var_ok)
			{
				flush("NetCdf$get_var -- your var [", var, "] doesn't exist in this reanalysis...\n")
				flush("Available variables are:", NetCdf$all_vars, "\n\n")
			}

			if (var_ok)
			{
				# figure out the actual name
				old_var = var
				var = match(toupper(var), toupper(NetCdf$all_vars))
				var = NetCdf$all_vars[var]

				flush("NetCdf$get_var -- you requested [", old_var, "], giving you [", var, "]\n")
			}
		}

		# if we want to read from the entire file...
		if (NetCdf$subset == FALSE)
			data = ncvar_get(NetCdf$ncdf, var)
		
		# if we want to read from a specific region...
		if (NetCdf$subset == TRUE)
			data = ncvar_get(NetCdf$ncdf, var, NetCdf$start, NetCdf$count)

		# encode our longitude, latitude and time into the variable's dimensions
		dimnames(data) = list(NetCdf$lon, NetCdf$lat, NetCdf$hour)
		return(data)
	}

	#
	# same as above, but works for 3d variables (with various levels)
	# 
	NetCdf$get_var_3d = function(var)
	{
		# if we want to read from the entire file...
		if (NetCdf$subset == FALSE)
			data = ncvar_get(NetCdf$ncdf, var)
		
		# if we want to read from a specific region...
		if (NetCdf$subset == TRUE)
		{
			# modify start and count to include the fourth dimension (pressure levels)
			start = c(NetCdf$start, 1)
			count = c(NetCdf$count, -1)

			data = ncvar_get(NetCdf$ncdf, var, start, count)
		}

		# encode our longitude, latitude and time into the variable's dimensions
		dimnames(data) = list(NetCdf$lon, NetCdf$lat, NetCdf$levels, NetCdf$hour)

		return(data)
	}




	#
	# read the wind speed at 50 metres
	#
	NetCdf$get_speed50m = function(min=0, max=NA)
	{
		# format is u50m[lon, lat, time]
		if (NetCdf$model == 'MERRA2')
		{
			u50m = NetCdf$get_var('U50M')
			v50m = NetCdf$get_var('V50M')
		}

		if (NetCdf$model == 'MERRA1')
		{
			u50m = NetCdf$get_var('u50m')
			v50m = NetCdf$get_var('v50m')
		}

		if (NetCdf$model == 'MERRA1NEW')
		{
			u50m = NetCdf$get_var('U50M')
			v50m = NetCdf$get_var('V50M')
		}

		# calculate the speed from pythagoras
		speed50m = sqrt(u50m^2 + v50m^2)

		# constrain speeds to our given limits (useful if our plot colours don't go higher than that)
		if (!is.na(min))
			speed50m[ speed50m < min ] = min

		if (!is.na(max))
			speed50m[ speed50m > max ] = max

		# return
		return(speed50m)
	}

	NetCdf$get_speed10m = function(min=0, max=NA)
	{
		# format is u10m[lon, lat, time]
		if (NetCdf$model == 'MERRA2')
		{
			u10m = NetCdf$get_var('U10M')
			v10m = NetCdf$get_var('V10M')
		}

		if (NetCdf$model == 'MERRA1')
		{
			u10m = NetCdf$get_var('u10m')
			v10m = NetCdf$get_var('v10m')
		}

		if (NetCdf$model == 'MERRA1NEW')
		{
			u10m = NetCdf$get_var('U10M')
			v10m = NetCdf$get_var('V10M')
		}

		# calculate the speed from pythagoras
		speed10m = sqrt(u10m^2 + v10m^2)

		# constrain speeds to our given limits (useful if our plot colours don't go higher than that)
		if (!is.na(min))
			speed10m[ speed10m < min ] = min

		if (!is.na(max))
			speed10m[ speed10m > max ] = max

		# return
		return(speed10m)
	}

	NetCdf$get_speed02m = function(min=0, max=NA)
	{
		# format is u02m[lon, lat, time]
		if (NetCdf$model == 'MERRA2')
		{
			u02m = NetCdf$get_var('U2M')
			v02m = NetCdf$get_var('V2M')
		}

		if (NetCdf$model == 'MERRA1')
		{
			u02m = NetCdf$get_var('u2m')
			v02m = NetCdf$get_var('v2m')
		}

		if (NetCdf$model == 'MERRA1NEW')
		{
			u02m = NetCdf$get_var('U2M')
			v02m = NetCdf$get_var('V2M')
		}

		# calculate the speed from pythagoras
		speed02m = sqrt(u02m^2 + v02m^2)

		# constrain speeds to our given limits (useful if our plot colours don't go higher than that)
		if (!is.na(min))
			speed02m[ speed02m < min ] = min

		if (!is.na(max))
			speed02m[ speed02m > max ] = max

		# return
		return(speed02m)
	}






	#####
	## ##  BUILD CLASS
	#####

	# define this as an environment
	NetCdf = list2env(NetCdf)

	# set the class type
	class(NetCdf) = 'NetCdfClass'

	# run the constructor
	NetCdf$constructor(filename, model, printInfo)

	# return
	return(NetCdf)
}
