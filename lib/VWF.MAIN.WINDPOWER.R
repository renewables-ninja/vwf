##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################



	# establish two plots
	if (a_few_plots)
	{
		dev.new(height=5.4, width=12, xpos=0, ypos=450)
		good_graphics()
		layout(t(1:3))

		plot_farms(windFarms, pch='+', cap.weight=1)
		dev.set()
	}





####################################

	##
	##  plot the mean wind speeds at each site
	##

	if (ncol(windSpeed) > 2) {
		site_means = colMeans(windSpeed[ , -1])
	} else {
		site_means = windSpeed[ , 2]
	}

	ylim = range(site_means/1.1, site_means*1.1)

	if (a_few_plots)
		plot(sort(site_means), type='l', ylim=ylim, main='Average Speeds by Site')



	##
	##  read in air density data
	##

	if (doAirDensity) ##### could parallelise this with cbind? iterators over each column
	{
		airDensity = read_data(airDensityFile)

		cat("\n")

		for (i in 1:nrow(windFarms))
		{
			clear_line()
			flush( paste("Applying air density correction: ", i, "/", nrow(windFarms), sep='') )

			# valid variable version of farm name (column name in csv)
			myName = windFarms[i, 'name']
			myCol = make.names(myName)
			if (!is.na(as.numeric(myName))) { myCol = as.character(myName) } ######
		
			# get the relevant info
			speed = windSpeed[ , myCol]
			density = airDensity[ , myCol]
			height = windFarms[i, 'height']

			windSpeed[ , myCol] = air_density_correction(speed, density, height)
		}

		clear_line()
		flush("Applying air density correction...\n")

		if (ncol(windSpeed) > 2) {
			site_means = colMeans(windSpeed[ , -1])
		} else {
			site_means = windSpeed[ , 2]
		}

		if (a_few_plots)
			lines(sort(site_means), col='red')
	}


	##
	##  transform unfathomably low speeds
	##  use a cubic hermite to enforce a lower-bound floor
	##

	if (doTransformLowestSpeeds)
	{
		# our transformation parameters
		# floor is the lowest we can go (m/s)
		# width is how smooth the transition is..
		# e.g. 4 and 1.5 means speeds of 2.5 go up to 4, speeds of 5.5 are unchanged  
		trnsfrm_floor = 4
		trnsfrm_width = 1.5

		trnsfrm_min = trnsfrm_floor - trnsfrm_width
		trnsfrm_max = trnsfrm_floor + trnsfrm_width

		w = which(site_means < trnsfrm_min)

		if (length(w) > 0)
		{
			scalars = 4.5 / site_means[w]

			speed = windSpeed[ , w+1]
			speed = mult_cols_by_vector(speed, scalars)
			windSpeed[ , w+1] = speed
		}

		w = which(site_means >= trnsfrm_min & site_means < trnsfrm_max)

		if (length(w) > 0)
		{
			scalars = trnsfrm_max - site_means[w]
			scalars = scalars ^ 2 / (4 * trnsfrm_width)
			scalars = (site_means[w] + scalars) / site_means[w]

			speed = windSpeed[ , w+1]
			speed = mult_cols_by_vector(speed, scalars)
			windSpeed[ , w+1] = speed
		}

		if (ncol(windSpeed) > 2) {
			site_means_x = colMeans(windSpeed[ , -1])
		} else {
			site_means_x = windSpeed[ , 2]
		}

		if (a_few_plots)
		{
			lines(sort(site_means_x), col='red3')
			lines(sort(site_means), col='red')
		}
	}

#################################





	##
	##  process the wind speed data
	##

	# interpolate to half hourly
	if (halfHourly)
	{
		cat("    Interpolating to half hourly...\n")
		windSpeed = interpolate_wind_speed(windSpeed, resolution=30)
	}

	# separate the date column from the numerics
	if (!exists('datecol'))
	{
		datecol = windSpeed[ , 1]
		windSpeed = windSpeed[ , -1]
	}

	# inject noise into the new profile
	if (halfHourly)
	{
		cat("    Injecting gaussian noise into profile...\n")
		windSpeed = inject_noise_into_wind_speeds(windSpeed, noise_stdev)
	}

	# make sure wind farm names line up in the two files
	windFarms$name = colnames(windSpeed)






#####
## ##  ESTABLISH THE PERFORMANCE RATIO OF ALL FARMS
#####

	##
	## transform wind speeds from the nasa ideal to the on-the-ground reality

	# set the base performance for all farms
	performance = rep(1.000, ncol(windSpeed))

	# modify these with the input file values if desired
	if (!is.null(PR_column))
	{
		performance = performance * windFarms[ , PR_column]
	}

	# show a histogram of performance ratios...
	if (a_few_plots)
		plot(performance)












#####
## ##  CONVERT FROM SPEED TO POWER -- GENERATE LOAD FACTORS
#####

	flush('> Generating load factors with ')
	if (match_method == 1) flush(" fixed scalar same for every farm, finding offset\n")
	if (match_method == 2) flush(" fixed scalar from each farm's PR, finding offset\n")
	if (match_method == 3) flush(" fixed offset same for every farm, finding scalar\n")
	if (match_method == 4) flush(" fixed offset from each farm's PR, finding scalar\n")


	# to avoid passing the whole data.frame...
	colnames_windSpeed = colnames(windSpeed)


	##WEIBULL_HAXX
	#wxx = seq(0, 1, length.out=1e6)
	#w20 = qweibull(wxx, 2.0, 10)
	#w17 = qweibull(wxx, 1.7, 10)
	#hist(w18, xlim=c(0,40), breaks=100)
	#hist(w20, xlim=c(0,40), breaks=100)
	#plot_l(w20, w20/w18)
	#weibull_spline = spline(w20, w20/w18, xout=seq(0,40,0.001))
	##WEIBULL_HAXX


	# run through each farm in parallel building a huge mega-list of results
	results = foreach (speed=iter(windSpeed, by='col'), i=icount(), .combine='rbind') %dopar%
	{
		# farm name and turbine model
		myName = windFarms[i, 'name']
		myModel = windFarms[i, 'power_curve']

		# valid variable version of farm name (column name in csv)
		# numeric farm names don't need an X on the front
		#myCol = make.names(myName)
		myCol = myName
		if (is.numeric(myCol))
				myCol = as.character(myCol)

		myModel = make.names(myModel)

		# my individual power curve - unmodified
		myCurve = turbCurve[ , myModel]

		# check our name is ok
		if (myCol %notin% colnames_windSpeed)
			stop("Cannot find the farm:", myName, "\n\n")




		# interpolate any NAs (which i believe are all zeroish)
		missing = which(is.na(speed))
		if (length(missing) > 0)
			speed[missing] = spline(speed, xout=missing)$y


		# calculate load factors using the unmodified turbine power curve
		mylf = wind_speed_to_power_output(speed, myCurve)
		energyInitial = mean(mylf)

		# calculate our target energy yield
		energyTarget = energyInitial * performance[i]





		##
		##  calculate modified yield using convoluted farm curve

		# determine how to convolute this to a curve for this whole farm
		mySpread = get_power_curve_convolution_spread(windFarms[i, ])

		# convolute to produce the power curve for this farm
		myCurve = farmCurve[ , myModel]
		myCurve = convoluteFarmCurve(myCurve, 0, mySpread)


		##WEIBULL_HAXX
		#weibull_adjuster = spline(w20, w20/w17, xout=speed)
		#speed = speed * weibull_adjuster$y
		##WEIBULL_HAXX


		# we choose to match using offsets, with fixed scalars
		if (match_method == 1 | match_method == 2)
		{
			# get the scalar for this farm
			myScalar = determine_farm_scalar(performance[i], windFarms$iso[i])
		
			# find the additive convolution offset that gives us the correct yield
			myOffset = find_farm_offset(speed, myCurve, myScalar, energyInitial, energyTarget)
		}

		# we choose to match using scalars, with fixed offsets
		if (match_method == 3 | match_method == 4)
		{
			# determine the offset we should use for this farm
			myOffset = determine_farm_offset(performance[i])

			# find the multiplicative scalar for wind speeds that gives us the correct yield
			myScalar = find_farm_scalar(speed, myCurve, myOffset, energyInitial, energyTarget)
		}

		# calculate the final load factors of this farm
		mylf = wind_speed_to_power_output(speed, myCurve, myScalar, myOffset)

		# make it clear if we got stuck
		if (myOffset < -20 | myOffset > 20 | myScalar < 0 | myScalar > 5)
			mylf = mylf * NA

		# calculate the power output in MW
		mymw = mylf * windFarms[i, 'capacity']



		# save the the corrected wind speeds if wanted
		if (save_modified_wind_speeds)
			speed = (speed + myOffset) * myScalar



		# print our VWF convolution parameters
		mo = sprintf('%1.2f', myOffset)
		cs = sprintf('%1.2f', convolverStdDev)
		ms = sprintf('%3.1f%%', myScalar * 100)
		flush('->  [ws + N(', mo, ', ', cs, ')] x ', ms, '\n', sep='')
	

		# save the VWF parameters for this farm
		parms = c(myName, energyInitial, performance[i], energyTarget, myOffset, myScalar, mySpread)

		results = list(
			speed=speed,
			mylf=mylf,
			mymw=mymw,
			parms=parms
		)

		return(results)
	}




#####
## ##  EXTRACT RESULTS
#####

	# now process the mega-results table

	# modified wind speeds
	windSpeed = as.data.frame(results[ , 1])
	colnames(windSpeed) = colnames_windSpeed

	# load factors
	loadFactor = as.data.frame(results[ , 2])
	colnames(loadFactor) = colnames_windSpeed

	# power outputs
	powerMW = as.data.frame(results[ , 3])
	colnames(powerMW) = colnames_windSpeed

	# VWF model parameters
	parms = as.data.frame(results[ , 4], stringsAsFactors=FALSE)
	parms = as.data.frame(t(parms), stringsAsFactors=FALSE)
	for (i in colnames(parms)[-1])
		parms[ , i] = as.numeric(parms[ , i])
	colnames(parms) = c('name', 'original_LF', 'PR', 'desired_LF', 'offset', 'scalar', 'stdev')


	# conserve memory
	rm(results)
	gc()







#####
## ##  POST PROCESSING
#####

	if (a_few_plots)
	{
		# histogram of site load factors
		cf = parms$desired_LF * 100
		hist(cf, breaks=0:ceiling(max(cf)), col='grey90')
		abline(v=mean(cf), lwd=2, col='red3')

		# map of capacity factors
		dev.set()
		cfc = colour_ramp_data(cf)
		plot_farms(windFarms, cap.weight=1, add.points=TRUE, pch='+', col=cfc)
		dev.set()
	}


	# run garbage collection to keep memory in check
	junk = capture.output(gc())
	