##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################




#####
## ##  SAVE HOURLY FILES
#####

	flush('> Saving results...\n')



	# save the bias corrected wind speeds for each farm
	if (save_modified_wind_speeds)
	{
		fn = baseSaveFolder %&% baseSaveFile %&% 'windspeed.corrected.' %&% xtn
		write_data(windSpeed, fn, xtn)
		flush('  ~ Written', fn, '\n')
	}



	# save the hourly capacity factors for every farm
	if (save_hourly_farm_cf)
	{
		fn = baseSaveFolder %&% baseSaveFile %&% '(a).farm.MW.' %&% xtn
		write_data(powerMW, fn, xtn)
		flush('  ~ Written', fn, '\n')
	}

	# save the hourly capacity factors for every farm
	if (save_hourly_farm_cf)
	{
		fn = baseSaveFolder %&% baseSaveFile %&% '(a).farm.CF.' %&% xtn
		write_data(loadFactor, fn, xtn)
		flush('  ~ Written', fn, '\n')
	}





	# save the total aggregate across all farms
	if ('*' %in% save_files_split_by)
	{
		# hourly
		if (ncol(powerMW > 1)) 
		{
			total = rowSums(powerMW, na.rm=TRUE)

		} else {

			total = powerMW
		}

		total = data.frame(GMT=ymd_wtf(datecol), MW=total, CF=total/sum(windFarms$capacity))

		fn = baseSaveFolder %&% baseSaveFile %&% '(b).hourly.MW.CF.csv'
		write_csv(total, fn, row.names=FALSE)
		flush('  ~ Written', fn)

		# monthly
		total_m = aggregate_monthly('GMT', total, mean)
		fn = baseSaveFolder %&% baseSaveFile %&% '(b).monthly.MW.CF.csv'
		write_csv(total_m, fn, row.names=FALSE)
		clear_line('  ~ Written', fn)

		# yearly
		total_y = aggregate_yearly('GMT', total, mean)
		fn = baseSaveFolder %&% baseSaveFile %&% '(b).yearly.MW.CF.csv'
		write_csv(total_y, fn, row.names=FALSE)
		clear_line('  ~ Written', fn, '\n')

	}




	# save the total MW and average CF grouped by various things (e.g. by country)
	for (split_col in save_files_split_by)
	{
		# skip doing nothing or everything or something silly
		if (split_col == '') next
		if (split_col == '*') next
		if (split_col %notin% colnames(windFarms))
		{
			flush('  !! you want to split results by ' %&% split_col %&% ' which isnt a column in your wind farms file...\n')
			flush('  !! you need to look at `save_files_split_by` and `windFarmCols`...\n')
			next
		}


		# establish results storage
		n = length(datecol)
		m = nrow(windFarms)
		snapshot_capacity = snapshot_output = data.frame(GMT=ymd_wtf(datecol))
		evolving_capacity = evolving_output = data.frame(GMT=ymd_wtf(datecol))

		# understand what time period this is
		min_date = min(evolving_output$GMT)
		max_date = max(evolving_output$GMT)
		total_duration = (max_date - min_date) / dyears(1)

		iii = 0

		# go through each value in that column
		for (split_val in sort(unique(windFarms[ , split_col])))
		{
			s_c = s_o = rep(0, n)
			e_c = e_o = rep(0, n)

			# filter our farms - filter gives the farm row / speed column
			filter = which(windFarms[ , split_col] == split_val)
			if (length(filter) == 0)
				next

			# calculate the total output and capacity
			for (f in filter)
			{
				my_output = powerMW[ , f]
				my_capacity = rep(windFarms$capacity[f], n)
				my_start = dmy(windFarms$date[f])

				# simply add these on to create the snapshot
				s_o = s_o + my_output
				s_c = s_c + my_capacity

				# if we have a start date, then wipe out the period before birth
				if (!is.na(my_start))
				{
					unborn = (evolving_capacity$GMT < my_start)

					# what percentage of the whole period have you lived for?
					# take that to be your capacity - so that the oldest farms have
					# the greatest weight when we go so far back that we know nothing
					my_duration = (max_date - my_start) / dyears(1)
					my_duration = my_duration / total_duration
					my_scalar = (0.1 * my_duration) / windFarms$capacity[f]

					# now scale output and capacity by this
					my_output[unborn] = my_output[unborn] * my_scalar
					my_capacity[unborn] = my_capacity[unborn] * my_scalar
				}

				# now add these to create the evolving fleet
				e_o = e_o + my_output
				e_c = e_c + my_capacity


				# gui
				iii = iii + 1
				if (iii %% 10 == 0)
					clear_line('Processing', iii, '/', m, '-', split_val)
			}

			# push these into our results storage
			snapshot_output[ , split_val] = s_o
			snapshot_capacity[ , split_val] = s_c

			# push these into our results storage
			evolving_output[ , split_val] = e_o
			evolving_capacity[ , split_val] = e_c
		}

		clear_line()

		# save power output to file
		fn = baseSaveFolder %&% baseSaveFile %&% '(c).snapshot.MW.' %&% split_col %&% '.csv'
		write_csv(snapshot_output, fn, row.names=FALSE)
		flush('  ~ Written', fn)

		# now normalise to CF
		for (i in 2:ncol(snapshot_output))
		{
			snapshot_output[ , i] = snapshot_output[ , i] / snapshot_capacity[ , i]
		}

		fn = baseSaveFolder %&% baseSaveFile %&% '(c).snapshot.CF.' %&% split_col %&% '.csv'
		write_csv(snapshot_output, fn, row.names=FALSE)
		clear_line('  ~ Written', fn)




		# save power output to file
		fn = baseSaveFolder %&% baseSaveFile %&% '(d).evolving.MW.' %&% split_col %&% '.csv'
		write_csv(evolving_output, fn, row.names=FALSE)
		clear_line('  ~ Written', fn)

		# now normalise to CF
		for (i in 2:ncol(evolving_output))
		{
			evolving_output[ , i] = evolving_output[ , i] / evolving_capacity[ , i]
		}

		fn = baseSaveFolder %&% baseSaveFile %&% '(d).evolving.CF.' %&% split_col %&% '.csv'
		write_csv(evolving_output, fn, row.names=FALSE)
		clear_line('  ~ Written', fn)

		fn = baseSaveFolder %&% baseSaveFile %&% '(d).evolving.capacity.' %&% split_col %&% '.csv'
		write_csv(evolving_capacity, fn, row.names=FALSE)
		clear_line('  ~ Written', fn)



		# oh, oh - finally, aggregate to months!
		snapshot_cf = aggregate_monthly('GMT', snapshot_output, mean, silent=TRUE)
		evolving_cf = aggregate_monthly('GMT', evolving_output, mean, silent=TRUE)

		#snapshot_cf = aggregate(snapshot_output[ , -1], by=list(years), mean, na.rm=TRUE)
		#evolving_cf = aggregate(evolving_output[ , -1], by=list(years), mean, na.rm=TRUE)

		fn = baseSaveFolder %&% baseSaveFile %&% '(c).snapshot.CF.monthly.' %&% split_col %&% '.csv'
		write_csv(snapshot_cf, fn, row.names=FALSE)

		fn = baseSaveFolder %&% baseSaveFile %&% '(d).evolving.CF.monthly.' %&% split_col %&% '.csv'
		write_csv(evolving_cf, fn, row.names=FALSE)
		clear_line('  ~ Written', fn)



		# oh, oh - finally, aggregate to years!
		snapshot_cf = aggregate_yearly('GMT', snapshot_output, mean, silent=TRUE)
		evolving_cf = aggregate_yearly('GMT', evolving_output, mean, silent=TRUE)

		#snapshot_cf = aggregate(snapshot_output[ , -1], by=list(years), mean, na.rm=TRUE)
		#evolving_cf = aggregate(evolving_output[ , -1], by=list(years), mean, na.rm=TRUE)

		fn = baseSaveFolder %&% baseSaveFile %&% '(c).snapshot.CF.yearly.' %&% split_col %&% '.csv'
		write_csv(snapshot_cf, fn, row.names=FALSE)

		fn = baseSaveFolder %&% baseSaveFile %&% '(d).evolving.CF.yearly.' %&% split_col %&% '.csv'
		write_csv(evolving_cf, fn, row.names=FALSE)
		clear_line('  ~ Written', fn, '\n')

	}



#	# save the date column
#	fn = baseSaveFolder %&% baseSaveFile %&% '(h).dates.csv'
#	write_csv(datecol, fn)
#	flush('  ~ Written', fn, '\n')
#


	# save the model parameters
	fn = baseSaveFolder %&% baseSaveFile %&% '(e).parameters.csv'
	write_csv(parms, fn)
	flush('  ~ Written', fn, '\n')
