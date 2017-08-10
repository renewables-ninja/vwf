##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################



	# initialise
	library(lubridate)
	all_days = NULL
	big_loop = 0

	# chart functions
	chart_coords = function(dates)
	{
		if (merra_grouping == 'monthly')
		{
			all_groups = ymd(floor_date(dates, 'month'))
		}

		if (merra_grouping == 'quarterly')
		{
			y = year(dates)
			m = month(dates)
			m = 1 + (floor((m-1)/ 3) * 3)
			all_groups = ymd( paste(y, m, '01') )
		}

		all_d = (dates - all_groups + 1) / ddays(1)

		data.frame(x=all_groups, y=all_d)
	}



	while (1)
	{
		# search for progress files...
		fn = list.files(baseSaveFolder, pattern='~progress~', full.names=TRUE)

		# sit and wait...
		if (length(fn) == 0)
		{
			clear_line('No progress files found...')
			Sys.sleep(update_time)
			next
		}
		

		# count how many good files we have
		all_progress = NULL

		# read and save progress in each
		for (i in 1:length(fn))
		{
			progress = readLines(fn[i])
			progress = suppressWarnings(ymd(progress))
			progress = progress[ !is.na(progress) ]
			all_progress = c(all_progress, format(progress))
		}


		# GUI
		if (big_loop == 0)
		{
			# build the list of all dates
			all_days = readLines(fn[i])[2]
			date_1 = get_text_before(all_days, ' to ')
			date_2 = get_text_after(all_days, ' to ')
			all_days = seq(ymd(date_1), ymd(date_2), 'day')

			# establish a clean plot
			all_coords = chart_coords(all_days)
			plot(all_coords, cex=1.5, pch=15, col='grey80', xlab='Month', ylab='Day')

			all_years = seq(ymd(date_1), ymd(date_2), 'year')
			axis(1, labels=FALSE, at=all_years)
			axis(3, labels=FALSE, at=all_years)
		}

		# add on our progress
		good_coords = chart_coords(ymd(all_progress))
		points(good_coords, cex=1.5, pch=15)



		# TUI
		n = length(all_progress)
		N = nrow(all_coords)
		p = sprintf("%1.2f%%", 100*n/N)
		msg = paste0('[', n, ' of ', N, '] ', p)

		clear_line(msg)

		
		# wait...
		big_loop = big_loop + 1
		Sys.sleep(update_time)

	}

