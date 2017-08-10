##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################




	#
	#  correct wind speeds based on air density
	#
	#       w = wind speed at hub height (m/s)
	#   rho_0 = air density at ground level (kg/m^3)
	#       z = hub height (m)
	#
	air_density_correction = function(w, rho_0, z)
	{
		# estimate air density at hub height using the international standard atmosphere
		rho = rho_0 - 0.00012*z

		# decide the exponent based on the wind speed
		# 1/3 up to 8 m/s, then a smooth-step function up to 2/3 above 13 m/s
		# this was fitted in matlab from the form x = [0, 1]; y = -2x^3 + 3x^2;
		m = -2/375 * w^3  +  21/125 * w^2  -  208/125 * w  +  703/125
		m[ w < 8 ]  = 1/3
		m[ w > 13 ] = 2/3

		# modify the wind speed
		w = w * (1.225 / rho)^m
		w
	}



 ########################################################################################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
 ########################################################################################################################


#####
## ##  SPEED TO POWER CONVERSION THINGS
#####



	#
	# take a time series of wind speeds and convert to load factors
	#
	# pass:
	#   a vector of wind speeds
	#   the power curve of the wind turbine / farm
	#   the scale factor and offset for adjusting wind speeds
	#
	wind_speed_to_power_output = function(windSpeed, powerCurve, myScalar=1.00, myOffset=0.00)
	{
		# transform the wind speeds
		windSpeed = (windSpeed + myOffset) * myScalar
		#windSpeed = (windSpeed * myScalar) + myOffset

		# convert to a power curve index
		# (i.e. which element of powerCurve to access)
		index = (100 * windSpeed) + 1

		# constrain this to within acceptable limits
		index[ index < 1 ] = 1
		index[ index > 4000 ] = 4000

		return(powerCurve[index])
	}


	#
	# find the fixed offset to add to wind speeds such that the resulting load factor equals energyTarget
	#
	# pass:
	#   speed = the wind speed time series
	#   farmCurve = the power curve you want to use for this farm
	#   myScalar = the scalar you want to use for this farm
	#   energyInitial = the initial load factor coming from unmodified MERRA
	#   energyTarget = the desired load factor that we are fitting to
	find_farm_offset = function(speed, farmCurve, myScalar, energyInitial, energyTarget, verbose=FALSE)
	{
		myOffset = 0

		# decide our initial search step size
		stepSize = -0.64
		if (energyTarget > energyInitial)
			stepSize = 0.64

		repeat
		{
			# change our offset
			myOffset = myOffset + stepSize

			# calculate the yield with our farm curve
			mylf = wind_speed_to_power_output(speed, farmCurve, myScalar, myOffset)
			energyGuess = mean(mylf)

			if (verbose) cat("target =", sprintf("%2.2f%%", 100*energyTarget), "~ guess =", sprintf("%2.2f%%", 100*energyGuess), "~ error =", sprintf("%+2.3f%%", 100 * (energyGuess - energyTarget) / energyTarget), "% with offset =", myOffset, "\n")

			# if we have overshot our target, then repeat, searching the other direction
			# if ((guess < target & sign(step) < 0) | (guess > target & sign(step) > 0))
			if (sign(energyGuess - energyTarget) == sign(stepSize))
				stepSize = -stepSize / 2

			# if we have done enough loops then step-size is smaller than our power curve's resolution
			if (abs(stepSize) < 0.002)
				break

			# if we have ended up in a very strange place, quit, and spit an error later on
			if (myOffset < -20 | myOffset > 20)
				break
		}

		return( myOffset )
	}


	#
	# find the scalar to multiply wind speeds by such that the resulting load factor equals energyTarget
	#
	# pass:
	#   speed = the wind speed time series
	#   farmCurve = the power curve you want to use for this farm
	#   myOffset = the offset you want to use for this farm
	#   energyInitial = the initial load factor coming from unmodified MERRA
	#   energyTarget = the desired load factor that we are fitting to
	find_farm_scalar = function(speed, farmCurve, myOffset, energyInitial, energyTarget, verbose=FALSE)
	{
		myScalar = 1.00

		# decide our initial search step size
		stepSize = -0.128
		if (energyTarget > energyInitial)
			stepSize = 0.128

		repeat
		{
			# change our scalar
			myScalar = myScalar + stepSize

			# calculate the yield with our farm curve
			mylf = wind_speed_to_power_output(speed, farmCurve, myScalar, myOffset)
			energyGuess = mean(mylf)

			if (verbose) cat("target =", sprintf("%2.2f%%", 100*energyTarget), "~ guess =", sprintf("%2.2f%%", 100*energyGuess), "~ error =", sprintf("%+2.3f%%", 100 * (energyGuess - energyTarget) / energyTarget), "% with scalar =", myScalar, "\n")

			# if we have overshot our target, then repeat, searching the other direction
			# if ((guess < target & sign(step) < 0) | (guess > target & sign(step) > 0))
			if (sign(energyGuess - energyTarget) == sign(stepSize))
				stepSize = -stepSize / 2

			# if we have done enough loops then step-size is smaller than our power curve's resolution
			if (abs(stepSize) < 0.0002)
				break

			# if we have ended up in a very strange place, quit, and spit an error later on
			if (myScalar <= 0 | myScalar > 5)
				break
		}

		return( myScalar )
	}








	#
	#  do what it says on the tin
	#
	plot_power_curve_and_wind_speed = function(speed, myCurve, myOffset, mySpread)
	{
		x = seq(0, 40, 0.01)
		y = myCurve
		y[y==0] = NA

		# plot the density of wind speeds
		ds = density(speed)
		plot(ds$x, 4*ds$y, type='h', col='grey85', xlab="MERRA wind speed (m/s)", ylab="Load factor", lwd=2, main=paste(myName, "=", myModel), xlim=c(0, 30), ylim=c(0, 1.01))

		# the original power curve
		lines(x, y, lwd=2)

		# our convoluted power curve
		farmCurve = convoluteFarmCurve(myCurve, myOffset, mySpread)
		x = x / myScalar
		lines(x, farmCurve, col="red3")
	}


#	plot_power_curve_and_wind_speed_alt = function(speed, myCurve, myOffset, mySpread)
#	{
#		x = seq(0, 40, 0.01)
#		y = myCurve
#		y[y==0] = NA
#
#		# plot the density of wind speeds
#		ds = density(speed)
#		plot(ds$x, 4*ds$y, type='h', col='grey85', xlab="MERRA wind speed (m/s)", ylab="Load factor", lwd=2, main=paste(myName, "=", myModel), xlim=c(0, 30), ylim=c(0, 1.01))
#
#		modspeed = (speed + myOffset) * myScalar
#		ds = density(modspeed)
#		lines(ds$x, 4*ds$y, col='grey45', lwd=3)
#
#		# the original power curve
#		lines(x, y, lwd=2)
#
#		# our convoluted power curve
#		farmCurve = convoluteFarmCurve(myCurve, 0, mySpread)
#		lines(x, farmCurve, col="red3")
#	}






 ########################################################################################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
 ########################################################################################################################

#####
## ##  RESHAPE DATA
#####

	#
	#  function to reshape a wide format matrix (with only row and column numbers as descriptors)
	#  to long format, optionally specifying the labels to assign to each row and column number.
	#
	reshapeData = function(myMatrix, myRowLabels = FALSE, myColLabels = FALSE)
	{
		# check how many variables we have
		nRows = nrow(myMatrix)
		nCols = ncol(myMatrix)

		if (!identical(myRowLabels, FALSE) & nRows != length(myRowLabels))
		{
			cat("reshapeData -- warning: number of rows in myMatrix does not equal the number of myRowVars labels\n");
			myRowVars = FALSE
		}

		if (!identical(myColLabels, FALSE) & nCols != length(myColLabels))
		{
			cat("reshapeData -- warning: number of columns in myMatrix does not equal the number of myColVars labels\n");
			myColVars = FALSE
		}

		# invent row and column labels if you didn't
		if (identical(myRowLabels, FALSE))
			myRowLabels = 1:nRows
		if (identical(myColLabels, FALSE))
			myColLabels = 1:nCols


		# create a long list of all variable combinations
		allRowsList = rep(1:nRows, times=nCols)  # 1 2 3 1 2 3 1 2 3 1 2 3
		allColsList = rep(1:nCols, each=nRows)   # A A A B B B C C C D D D

		# create matrices of row and column vars
		matrixRows = matrix(myRowLabels[allRowsList], nRows, nCols)
		matrixCols = matrix(myColLabels[allColsList], nRows, nCols)


		# create our final long format list
		return ( data.frame( row=as.vector(matrixRows), col=as.vector(matrixCols), value=as.vector(myMatrix) ) )
	}











 ########################################################################################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
 ########################################################################################################################

#####
## ##  INTERPOLATE AND CONVOLUTE POWER CURVES
#####

	#
	#  function to turn a low resolution power curve file (e.g. 0..40 m/s in 1 m/s steps)
	#  into the kind we need, with 0.01 m/s resolution, using cubic spline interpolation
	#
	interpolateTable = function(oldData, newX)
	{
		# remember our old x index
		oldX = oldData[ , 1];

		# create a matrix to hold our new data
		newData = mat.or.vec(length(newX), ncol(oldData));
		
		# fill our new x axis
		newData[ , 1] = newX;

		
		# run through each column..
		for (i in 2:ncol(oldData))
		{
			# extract the power curve
			oldY = oldData[ , i];
			yMax = max(oldY);
			
			# create a spline.. but ignore any zeros past the upper cut-off speed
			# because the spline goes nuts when there's a step change..
			s = spline(oldX[oldX<10 | oldY>0], oldY[oldX<10 | oldY>0], method="fmm", xout=newX);
		
			# recover some dignity
			# clip numbers at zero and the original max (with 0.1% leeway)
			s$y[s$y < yMax * 0.001] = 0;
			s$y[s$y > yMax * 0.999] = yMax;
		
			# reinstate the lower cutoff limit
			cutoff = min(which(oldY > 0));   # finds which array index has the first non-zero speed
			cutoff = oldX[cutoff] - 0.01;    # finds the speed corresponding to this
			if (cutoff > 3.5) cutoff = 3.5;  # none too high
			if (cutoff < 2.5) cutoff = 2.5;  # none too low
			s$y[s$x < cutoff] = 0;
			
			# reinstate the upper cutoff limit
			cutoff = max(which(oldY > 0));
			cutoff = oldX[cutoff] + 0.01;
			s$y[s$x > cutoff] = 0;
			
			# and save
			newData[ , i] = s$y;
		}
		
		# convert to a data frame
		newData = data.frame(newData)
		colnames(newData) = colnames(oldData)
		
		return (newData);

	}



	#
	#  function to turn a 0..40 m/s turbine power curve into an aggregate farm curve convoluted by N(mean, sd)
	#
	convoluteFarmCurve = function(myCurve, myMean, mySD)
	{
		# define our output resolution
		resolution = 0.01
		real_x = seq(0, 40, 0.01)

		# pad our curve sufficiently for the filtering
		x = seq(-10, 50, 0.01)
		y = c(rep(0, 1000), myCurve, rep(0, 1000))

		# build a gaussian filter
		w = 10
		convolver.x = seq(-w, w, 0.01)
		convolver.y = dnorm(convolver.x, myMean, mySD)
		convolver.y = convolver.y / sum(convolver.y)

		smooth_y = filter(y, convolver.y, sides=2)

		x_range = which(x == min(real_x)) : which(x == max(real_x))

		return(smooth_y[x_range])
	}







 ########################################################################################################################
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
 ########################################################################################################################


#####
## ##  WIND SPEED TEMPORAL INTERPOLATION AND NOISE INJECTION
#####
	
	interpolate_wind_speed = function(windSpeed, resolution=30)
	{
		## get rid of fools
		if (resolution > 30)
		{
			cat("interpolate_wind_speed: doing nothing because resolution is greater than 30 minutes...\n")
			return(windSpeed)
		}


		##
		## build our new storage
		steps = (60 / resolution)
		if (steps %% 1 != 0)
			stop('interpolate_wind_speed: must have an integer number of steps within each hour -- resolution =' %&% resolution %&% 'minutes just nah work!\n')

		ws = windSpeed
		for (i in 2:steps)
			ws = rbind(ws, windSpeed)



		##
		## generate the new date sequence

		# create originals
		dates = ymd_hm(windSpeed$Timestamp)
		index = seq_along(dates) * steps
		index = index - (steps - 1)

		# start building big versions
		datesAll = dates
		indexAll = index

		for (i in 1:(steps-1))
		{
			shift = i * resolution
			datesAll = c(datesAll, dates + minutes(shift))
			indexAll = c(indexAll, index + i)
		}

		# interleave them
		datesAll = datesAll[ order(indexAll) ]

		# generate the sequence
		ws[ , 1] = datesAll



		##
		## do the interpolation

		# original data is on the half-hour
		x1 = seq(1, nrow(windSpeed)) - 0.5

		# new data will be the average of each half-hour - i.e. on the quarter hour
		x2 = (seq(1, nrow(ws)) - 0.5) / steps

		# shower down
		for (i in 2:ncol(windSpeed))
		{
			# ignore if all NA
			if (sum(!is.na(ws[ , i])) == 0) next
			# spline otherwise			
			ws[ , i] = spline(x1, windSpeed[ , i], xout=x2)$y
		}

		return(ws)
	}


	inject_noise_into_wind_speeds = function(windSpeed, stdev)
	{
		len = dim(windSpeed)[1] * dim(windSpeed)[2]
		noise = rnorm(len, 1.000, stdev)
		
		windSpeed * noise
	}

