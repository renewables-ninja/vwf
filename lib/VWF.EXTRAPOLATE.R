##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################




 ########################################################################################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
 ########################################################################################################################

#####
## ##  WIND PROFILE EXTRAPOLATION ==== VERSION FOR PRE-COMPILING
#####


# calculate the wind extrapolation parameters for a given data point (one location and height)
# takes four parameters: wind speed at 2, 10, 50 metres, and the displacement height
# returns two parameters: the scale factor (A) and reference height (z)
# wind speed at any height can be calculated from: v = A * log(h / z)
extrapolate_log_law = function(w2m, w10m, w50m, dh)
{
	# assemble our three heights and wind speeds
	h = c(2+dh, 10+dh, 50)
	v = c(w2m, w10m, w50m)

	# linearise and perform a ls fit
	# weight the data at 50m more strongly
	logh = log(h)
	fit = lm(v ~ logh, weights=c(1,1,2))

	# extract our coefficients
	# v = A log(h) - A log(z) therefore slope = A, exp(-intercept / A) = z
	A = coef(fit)[[2]]
	z = exp(-coef(fit)[[1]] / A)

	return( c(A, z) )
}



# calculate the wind extrapolation parameters for a given data point (one location and height)
# takes four parameters: wind speed at 2, 10, 50 metres, and the displacement height
# returns two parameters: the scale factor (epsilon) and shear coefficient (alpha)
# wind speed at any height can be calculated from: v = epsilon * h ^ alpha
extrapolate_power_law = function(w2m, w10m, w50m, dh)
{
	# assemble our three heights and wind speeds
	h = c(2+dh, 10+dh, 50)
	v = c(w2m, w10m, w50m)

	# linearise and perform a ls fit
	# weight the data at 50m more strongly
	logh = log(h)
	logv = log(v)

	fit = lm(logv ~ logh, weights=c(1,1,2))

	# extract our coefficients
	# v2 / v1 =  (h2 / h1) ^ alpha, therefore v2 = epsilon * h ^ alpha
	epsilon = exp(coef(fit)[[1]])
	alpha = coef(fit)[[2]]

	return( c(epsilon, alpha) )
}



# wrapper function to calculate the extrapolation parameters for all data points
# takes the filename to be processed
# returns a list of arrays, one containing each parameter returned by the chosen extrapolation function
#
# note - if you want to change which extrapolation function is used, change the mapply() statement, and the last segment of code (extracting & returning parameters)
#
extrapolate_ncdf = function(nc.filename)
{

	#####
	## ##  READ THE NECESSARY NETCDF DATA
	#####

	# open the file
	nc$open_file(nc.filename)
	flush("Calculating wind shear using the log law -", format(nc$date))

	# extract wind speed at 2, 10 and 50 metres - format is speed2m[lon, lat, time]
	wind2m = nc$get_speed02m()
	wind10m = nc$get_speed10m()
	wind50m = nc$get_speed50m()

	# extract displacement height
	if (nc$model == 'MERRA1') disph = nc$get_var('disph')
	if (nc$model == 'MERRA2') disph = nc$get_var('DISPH')

	# close this input file
	nc$close_file()





	#####
	## ##  CALCULATE ALL EXTRAPOLATION PARAMETERS
	#####

	# create our results storage
	profile = NULL

	# run through each hour in the data set
	for (h in 1:length(nc$hour))
	{
		flush(sprintf(" %02d:00", h-1))

		# process all locations in parallel
		wp = foreach (y = 1:length(nc$lat), .combine=cbind) %dopar%
		{
			mapply(FUN=extrapolate_log_law, wind2m[ , y, h], wind10m[ , y, h], wind50m[ , y, h], disph[ , y, h])
		}

		# bind the results together
		profile = cbind(profile, wp)

		flush("\b\b\b\b\b\b")
	}

	clear_line()


	# extract the parameters from extrapolate_one(), and reshape to match our input arrays
	A = array(profile[1, ], dim=dim(wind2m), dimnames=dimnames(wind2m))
	z = array(profile[2, ], dim=dim(wind2m), dimnames=dimnames(wind2m))

	# return as a list
	list(A=A, z=z)
}








 ########################################################################################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
 ########################################################################################################################


#####
## ##  WIND PROFILE EXTRAPOLATION ==== OLD VERSION
#####

	#  function that takes our speed and height data and extrapolates the wind speed up to hub height
	#  uses the log law, deriving the roughness length from the set of observations
	extrapolateToHubHeight = function(row)
	{
		# assemble our data
		h = as.numeric(c(row["height2m"], row["height10m"], row["height50m"]))
		v = as.numeric(c(row["speed2m"], row["speed10m"], row["speed50m"]))
		
		hx = as.numeric(row["hubHeight"])


### FIX ME ###
#
#		## this might be faster, and cleaner
#		h = c(row$height2m, row$height10m, row$height50m)
#		v = c(row$speed2m, row$speed10m, row$speed50m)
#		hx = row$hubHeight
#
#
### FIX ME ###


		# extrapolate to our hub height
		vx = extrapolateUsingLogLaw(h, v, hx)

		#plot(h, v, xlim=c(0,100), ylim=c(min(v)/1.3, max(v)*1.3))
		#lines(1:100, A*log(1:100 / z), col="red3")
		#lines(1:100, epsilon*(1:100)^alpha, col="blue")
		#points(hx, vx, pch=16, col="purple")

		if (is.infinite(vx) | is.na(vx))
			vx = 0

		return (vx)
	}


	#  h = list of heights for which we have measurements
	#  v = list of speeds at those heights
	# hx = hub height at which you want to know the speed 
	extrapolateUsingLogLaw = function(h, v, hx)
	{
		# linearise and perform a ls fit
		# weight the data at 50m more strongly
		log_h = log(h)

		fit = lm(v ~ log_h, weights=c(1,1,2))

		# extract our coefficients
		# v = A log(h) - A log(z) therefore slope = A, exp(-intercept/ / A) = z
		A = coef(fit)[[2]]
		z = exp(-coef(fit)[[1]] / A)

		# extrapolate to the desired height
		vx = A * log(hx / z)

		return (vx)
	}



	#  h = list of heights for which we have measurements
	#  v = list of speeds at those heights
	# hx = hub height at which you want to know the speed 
	extrapolateUsingPowerLaw = function(h, v, hx)
	{
		# linearise and perform a ls fit
		# weight the data at 50m more strongly
		log_h = log(h)
		log_v = log(v)

		fit = lm(log_v ~ log_h, weights=c(1,1,2))

		# extract our coefficients
		# v2 / v1 =  (h2 / h1) ^ alpha, therefore v2 = epsilon * h ^ alpha
		epsilon = exp(coef(fit)[[1]])
		alpha = coef(fit)[[2]]

		# extrapolate to the desired height
		vx = epsilon * hx ^ alpha

		return (vx)
	}



