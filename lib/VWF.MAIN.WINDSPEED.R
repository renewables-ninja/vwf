##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################




#####
## ##  READ IN EXISTING SPEEDS IF WE HAVE THEM
#####

# we have specified a custom wind speed file to use...
if (!is.null(windSpeedFile))
{
	# check this file exists...
	if (!file.exists(windSpeedFile))
	{
		flush("  OH NO! You specified a custom windSpeedFile, but it doesn't exist!\n")
		 stop("        ", windSpeedFile, "\n")
	}
}

# consider the default filename for wind speeds
if (is.null(windSpeedFile))
{
	windSpeedFile = baseSaveFolder %&% baseSaveFile %&% 'windspeed.' %&% xtn
}


# if that file exists - read it in!
use_cached_speeds = file.exists(windSpeedFile)

if (use_cached_speeds)
{
	flush('> Reading pre-compiled wind speeds...\n')
	windSpeed = read_data(windSpeedFile)
}





#####
## ##  RUN THROUGH EACH GROUP (MONTH / QUARTER) OF MERRA FILES, READ IN THE WIND SPEED FOR EACH WINDFARM & SAVE TO A TEMP FILE
#####

if (!use_cached_speeds)
{
	flush('> Generating wind speeds... go and use the [progress meter] file to monitor how things are going...\n')
	flush('  Progress files being written to', baseSaveFolder, '\n')

	windSpeed = foreach (g = merra_wind$groups, .combine=rbind, .packages=c('ncdf4', 'lubridate')) %dopar%
	{

		# establish our progress reporting
		progress_file = paste0(baseSaveFolder, '~progress~', baseSaveFile, '.', g, '.txt')
		write('Wind speed processing started', progress_file)
		date_1 = head(merra_wind$dates, 1)
		date_2 = tail(merra_wind$dates, 1)
		date_range = paste(format(date_1), 'to', format(date_2))
		write(date_range, progress_file, append=TRUE)
		write('\n', progress_file, append=TRUE)



		# prepare this month's input files
		nFiles = length(merra_wind$index[[g]])
		cat('Processing', g, '=', nFiles, 'files...\n')

		# prepare our results storage
		# this holds the estimated wind speeds, rows=hours, columns=farms
		outputCols = c("Timestamp", windFarms$name)
		speed = data.frame( matrix(NA, nrow=nFiles*24, ncol=length(outputCols) ) )
		colnames(speed) = outputCols

		# initialise our hour counter
		hour = 0


		###
		###  READ EACH FILE (DAY) IN TURN
		###
		for (f in merra_wind$index[[g]])
		{

			##
			##  READ IN THE NETCDF DATA
			##

			# read in the wind profile data
			fn = merra_wind$files[f]
			nc_wind$open_file(fn)

			# make a vector of our 24 timestamps
			nc_wind_date = ymd(merra_wind$dates[f])
			timeVector = nc_wind_date %&% ' ' %&% sprintf("%02d:00", nc_wind$hour)  #### horror hack

			# apply this to our storage
			todays_hours = (hour+1):(hour+24)
			speed[todays_hours, 1] = timeVector

			# update
			#flush('Reading', format(nc_wind$date))
			write(format(nc_wind$date), progress_file, append=TRUE)


		
			# extract our extrapolation parameters - format is A[lon, lat, time]
			A = nc_wind$get_var('A')
			z = nc_wind$get_var('z')
		
			# close this input file
			nc_wind$close_file()




			# if we are crossing the date line, reshape our data to contain what we need
			if (dateLineMadness)
			{
				A = A[ c(dateLineWest, dateLineEast), , ]
				z = z[ c(dateLineWest, dateLineEast), , ]
				dimnames(A)[[1]] = dateLineDims
				dimnames(z)[[1]] = dateLineDims
				nc_wind$lon = dateLineDims
			}
 



			##
			##  SPATIAL INTERPOLATION AND HEIGHT EXTRAPOLATION
			##

			# interpolate each hour of the day
			for (h in 1:24)
			{
				# initialise the wind speeds for all farms
				s = rep(NA, nrow(windFarms))

				# run through each height in turn
				for (height in all_heights)
				{				
					# get wind speeds at this height
					w = A[ , , h] * log(height / z[ , , h])

					# locate the farms we care about
					my_farms = (windFarms$height == height)

					# loess interpolation
					if (spatial.method == 'loess')
					{
						# create a loess fit
						loess_w$value = as.vector(w)
						loess_fit = loess(value ~ lon*lat, data=loess_w, degree=2, span=loessSpan)

						# interpolate speeds to these farms
						s[my_farms] = predict(loess_fit, newdata=windFarms[my_farms, ])
					}

					# spline interpolation
					if (spatial.method == 'akima')
					{
						# cubic spline our speeds
						s[my_farms] = bicubic(nc_wind$lon, nc_wind$lat, w, windFarms$lon[my_farms], windFarms$lat[my_farms])$z
					}

				}

				# append these extrapolated speeds - forbidding silliness like negative speeds
				hour = hour + 1
				speed[hour, -1] = pmax(0, s)
			}


			# if we are crossing the date line, repair the damage we did
			if (dateLineMadness)
			{
				nc_wind$lon = originalDims
			}

		}


		# save the wind speeds if requested
		if (save_original_wind_speeds)
		{
			fn = baseSaveFolder %&% baseSaveFile %&% 'windspeed.' %&% g %&% '.' %&% xtn
			write_data(speed, fn, xtn)
			flush('\n ~ Written', fn)
		}


		# could return the wind speeds for joining in memory?
		# speed

	} # finished processing all files - end of parallel loop







	###
	###  CLEAR OUT TEMPORARY FILES
	###  AND BIND ALL WIND SPEEDS TOGETHER
	###
	
	# clear out the progress files
	progress_file = paste0(baseSaveFolder, '~progress~', baseSaveFile, '.', merra_wind$groups, '.txt')
	progress_file = progress_file[ file.exists(progress_file) ]
	if (length(progress_file) > 0) file.remove(progress_file)

	# concatenate all the results files
	fn = baseSaveFolder %&% baseSaveFile %&% 'windspeed.' %&% merra_wind$groups %&% '.' %&% xtn
	windSpeed = read_data_bind_rows(fn, verbose=FALSE)

	# and save
	big_fn = baseSaveFolder %&% baseSaveFile %&% 'windspeed.' %&% xtn
	flush('~ Writing', big_fn, '\n')
	write_data(windSpeed, big_fn, xtn)

	# and remove the intermediates
	file.remove(fn)

}




#####
## ##  FINISH OFF
#####

	# kill the plot of wind farm locations
	if (a_few_plots) try( dev.off(), silent=TRUE )
	flush('>', nrow(windSpeed), 'hours of wind speeds calculated for', ncol(windSpeed), 'farms\n')
