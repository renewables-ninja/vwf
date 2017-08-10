#############################################################
#####       ____    _                       _____       #####
#####      |  _ \  | |                     |  __ \      #####
#####      | |_) | | |   __ _   _ __ ___   | |__) |     #####
#####      |  _ <  | |  / _` | | '_ ` _ \  |  _  /      #####
#####      | |_) | | | | (_| | | | | | | | | | \ \      #####
#####      |____/  |_|  \__,_| |_| |_| |_| |_|  \_\     #####
#####                                                   #####
#############################################################
#####  INCLUDE BLAM.R  -  18-05-2017  -  IAIN STAFFELL  #####
#############################################################
#########################  CONTENTS  ########################
#############################################################




#############################################################
#########  GENERAL STUFF 
##
##  %&%																Concatenate strings like a %&% b
##  %notin%															Opposite of %in%
##
##  flush(...)														Flush a message to console
##  clear_line(...)													Erase the last line of console text and flush a new message
##
##  rc(text=FALSE, header=FALSE)									Read data from clipboard, returns 1d vector or 2d table depending on what's there
##  																- Converts to numeric by default, set text=TRUE if you paste in text
##																	- Set header=TRUE to make first row of the clipboard into your colnames
##											
##  wc()															Write a 1d vector or 2d table to clipboard, for easy pasting into Excel
##
##  xc()															Execute the contents of the clipboard (faster and cleaner than pasting code in)
##
##  memory_usage(...) / mem_use(...)								List objects and their memory use (pass a string to search by object name)
##
##  sleep(seconds)													ZZzz..
##
##
##
#############################################################
#########  READ DATA
##  
##  read_data(filename)												Lazy man's read - will read big csv, rdata or rds as required
## 
##  read_csv(filename)												Read in data 3x faster than read.csv()
##
##  read_big_csv(filename)											Read in data 10x faster than read.csv(), but with longer initialise time, good for >100 MB
##
##  read_zipped_csv(filename)										Read a ZIP file which contains a single CSV file, in-memory decompression, no junk files made
##
##  read_data_bind_rows(filenames, ...)								Read in several files, binding data on rows or columns...
##  read_data_bind_cols(filenames, ...)
##
##
##
##  write_data(data, filename, format)								Lazy man's write - will save as csv, rdata or rds as required.
##
##  write_csv(data, filename)										Removes some of the oddities of write.csv()
##
##  csv_to_rds(filename)											Convert CSV data to RDS (r binary) format, which is smaller and faster
##  rds_to_csv(filename)											
##
##  merge_csv_files(inputFiles, outputFile)							Merge a set of CSV files into one file
##
##
##
#############################################################
#########  GRAPHICS
##
##  good_graphics(mar, mgp, padding, captions)						Set up good graphics paramaeters
##                                                                  - Pass your own mar(b,l,t,r) or mgp(caption,label,0)
##                                                                  - Change padding=TRUE if you want to keep that, or captions=FALSE if you like them rotated
##
##  good_png(filename, height, width, res)							Print a good png
##																	- height, width and res are optional (defaults give 8pt text at 8cm width ish)
##
##  plot_p(x, y, ...)												Plot points as transparent greys
##  plot_l(x, y, ...)												Plot as a line
##
##  boxplot.pars													Make boxplots pretty with boxplot(y ~ x, pars=boxplot.pars)
##
##  heat.colours, rainbow.colours, blue.colours						Nicer sets of colours
##
##  colour_ramp_data(pal=NULL, range=NULL)							Generate a colour ramp for your data (automatically normalises it, etc.)
##
##  stepped_line_graph(x, y)										Build a stepped data set from x and y which can be plotted like a histogram in Excel
##  stepped_line_graphs(x, col)										Build a stepped data set with multiple columns, 'col' is the name of the date/x column
##
##  log_gridlines(min, max)											Return the values of major and minor gridlines for a log chart (e.g. major = 10, 100, 1000; minor = 20, 30, ...)
##
##  hist_and_fit(data, x, y, title)									Plot a histogram and line fit that you have created with pnormal(), pweibull(), etc.
##  hist_and_normal_fit(data, breaks, title)						Plot a historgram and automatically fit a normal distribution to it
##  hist_log_x(data, ...)											Plot a histogram with a logged x axis
##  hist_log_y(data, ...)											Plot a histogram with a logged y axis
##
##
##
#############################################################
#########  MAPS
##
##  get_world_map(shapeFile)										Returns a map from a given shapeFile, or the rWorldMap library if none passed
##  get_europe_map(resolution, disembodied)
##  get_europe_north_africa_map(resolution)
##
##  plot_map_points(lon, lat, ...)									Plot a map with points at the given lon and lat
##																	- See below for the 100 other options
##
##
#############################################################
#########  TEXT TOOLS
##
##  strrev('Hello World!')											Reverse a string
##
##  substr_reverse = function(x, n)									Get a substring from the end backwards
##
##  str_replace('e', 'u', 'Hello World!')							Replace 'x' for 'y' in 'string'
##
##  get_text_before(string, token, last=F)							Return the text in 'string' that comes before 'token'
##																	- First instance by default, or the last instance if last=T
##  
##  get_text_after(string, token, last=F)							Ditto
##
##  get_text_between(string, token1, token2)						Return the text between 'token1' and 'token2'
##																	- Uses the first instance of each
##
##  is_string_numeric(data)											Test if a string (or array of strings) holds valid numbers
##
##  keyval_to_df(keyval, splitter)									Convert an 1d array of key-value pairs into a 1 row data frame
##  all_keyval_to_df(keyval_array, splitter)						Convert an array of arrays of key-value pairs into a 2d data frame
##
##  percent(val, dp=2)                                              Convert one/some numbers into percentage format
##
##
##
#############################################################
#########  ARRAYS AND DATA FRAMES
##  
##  push(array, item)												Push and pop for vectors
##  pop(array)														(yes, i'm that lazy)
##
##  insert_row(DF, row, r)											Insert 'row' into 'DF' at row number 'r'
##
##  xbind(array, array, dimension)									Bind two multi-dimensional arrays on a specific dimension (as opposed to rbind, cbind)
##
##  nth_largest(array, n)											Return nth biggest or 
##  nth_smallest(array, n)											smallest number in an array
##
##  which.closest(array, x)											Return the index of the array element that is closest to x
##
##  mult_cols_by_vector(matrix, vector)								Multiply each column of a matrix by each element of the vector (i.e. each row gets multiplied by the whole vector)
##  mult_rows_by_vector(matrix, vector)								Multiply each row of a matrix by each element of the vector (i.e. each column gets multiplied by the whole vector)
##
##  midPoints(x)														Return the midpoint of all adjacent elements (e.g. midPoints(1:5) = 1.5:4.5
##
##  freq_table(data)												Return a frequency table in ascending count order
##
##  move_column(data, myCol, where, relCol)							Move columns within a dataframe - e.g. data = move_column(data, 'a_column', 'before', 'another_column')
##
##
## 	moving_average(data, n)											Calculate the moving average for a vector of data, with n smoothing width either side of the value
##
##  biggest_diff(data, lag)											Calculate the largest swing within a given number of periods
##
##  na.everyone(dataframe, cols)									Copy NA values across all columns in a data frame
##
##  list_to_data_frame(l)											...
##
##  aggregate_n(values, n, FUN)										Aggregate a vector or data-frame into fixed-sized groups
##
##
##
#############################################################
#########  TIME-SERIES DATA
##
##  aggregate_yearly(x, y, FUN)										Aggregate a time-series to yearly level.
##           _monthly()													x = a lubridate time series
##           _weekly()													y = the data series
##           _daily()													FUN = aggregate function (defaults to mean)
##
##  seasonal_diurnal(x, y, FUN)
##  plot_diurnal(dd)
##
##  dmy_hs(string)													Additional options for lubridate
##  ymd_hs(string)
##
##	lag_data(data, steps)											Lag values in a vector of data by n steps by shifting them backwards (accepts non-integer steps)
##	lead_data(data, steps)											Lead values in a vector of data by shifting them forwards
##
##  align_data(df1, col1, df2, col2)								Align two data frames so that df1$col1 and df2$col2 are identical.  Useful in lining up time-series data.
##
##  tz_search('City')												Search for the timezone name that's relevant to your location
##
##  tz_shift(data, zone)											Calculate the shift needed to move your data into that timezone (works on a series of input times, accounts for daylight savings)
##
##  fill_missing_dates(x, datecol, period, fill)					Pad out your dataframe to have a complete time series of a given period (defaults to days).  Optionally, fill all missing timesteps with data = fill
##
##
##
#############################################################
#########  MATHS AND STATISTICS
##
##  mode(data)														Calculate the mode (most common)
##
##  weighted_mean(data, weights)									Mean and stdev with weighted data points
##  weighted_sd(data, weights)
##
##  stderr(data)													Calculate the standard error on a mean	
##  ci95(data)														Calculate the 95% confidence interval (based on the stderr)
##
##  r2(fit)															Return the adjusted R² value from an lm() fit
##
##  coef.err(fit)													Return the coefficients and standard errors from an lm() fit
##  lm.coef.err(formula)											Run an lm() fit and return its coeffients and errors
##
##  rms(residuals)													Calculate the root mean square from a set of residuals - e.g. call rms(fit - data)
##
##  rescale(data, min, max)											Rescale a vector to be in the range of [min, max]
##
##  constrained_distro(distro, length, min, max, ...)				Generate a random distribtion that is constrained within set limits
##
##
##





###################################################################################################################
##################      GENERAL STUFF      ########################################################################
###################################################################################################################



	`%&%` = function(a, b) paste0(a, b)

	`%notin%` = Negate(`%in%`)



	# console - clear line and flush line
	clear_line = function(...)
	{
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")

		if (length(list(...)) > 0)
		{
			cat(...)
			flush.console()
		}
	}

	flush = function(...)
	{
		cat(...)
		flush.console()
	}



	#####
	## ##  read data from clipboard
	## ##  write data to clipboard
	#####

	# if reading in text, set text=TRUE to return character rather than numeric data
	# if reading in multiple columns set header=TRUE to take your first row and set as column names
	rc = function(text=FALSE, header=FALSE)
	{
		# get the contents
		x = readClipboard()

		# deal with one column
		if (length( grep('\t', x)) == 0)
		{
			# convert to numeric data
			if (text == FALSE)
			{
				x = as.numeric(x)
			}

			return (x)
		}

		# deal with multiple columns
		if (length( grep('\t', x)) > 0)
		{
			x = read.table('clipboard', stringsAsFactors=FALSE)

			# turn the header row into column names
			if (header)
			{
				colnames(x) = x[1, ]
				x = x[-1, ]
				rownames(x) = 1:nrow(x)
			}

			# convert to numeric data
			if (text == FALSE)
			{
				cols = colnames(x)
				x[ , cols] = sapply(x[ , cols], as.numeric)
			}

			return (x)
		}
	}




	# write an object to clipboard
	# this uses various trickery to get around the clipboard size limit!
	wc = function(x, row.names=FALSE)
	{
		# write a 1d vector
		if (is.null(dim(x)))
		{
			writeClipboard( as.character(x) )
			return()
		}

		# write a 2d data frame
		if (length(dim(x)) == 2)
		{
			# this tries the 16MB clipboard
			tryCatch(
			{
				write.table(x, "clipboard-16384", sep="\t", row.names=row.names)
			},

			# if it fails, it does some funky business (as strings are unlimited??)
			warning = function(cond)
			{
				# convert x to a data.frame of char 
				for(col in 1:ncol(x))
					x[ , col] = as.character(x[ , col])

				# now convert its transpose into a string - so we get c(1st row), c(2nd row), ...
				x = as.data.frame( t(x), stringsAsFactors=FALSE)
				x = toString(x)

				# convert the delimiter from comma to tab
				x = gsub('\", \"', '\t', x, fixed=TRUE)

				# convert EOL to a newline character
				x = gsub('\"), c(\"', '\n', x, fixed=TRUE)

				# chop off the first c(\" and the last \")
				x = substr(x, 4, nchar(x)-2)

				# now paste your goodies into excel
				writeClipboard(x)
			})

			return()
		}
	}




	# execute the contents of the clipboard
	# this makes things work nicer, stop() and wait_for_key() both work, you don't get massive print-outs of code, etc...
	xc = function() { source('clipboard') }



	# make the console wait until a key is pressed
	wait_for_key = function(txt='Press [enter] to continue')
	{
		invisible(readline(prompt=txt))
	}



	##  list objects and memory usage
	##  beefed up from http://tinyurl.com/pgxwl6c

	## memory_usage()
	## memory_usage("b", n=20)
	mem_use = function(...) { memory_usage(...) }
	memory_usage = function(pattern, pos=1, order.by="Size", decreasing=TRUE, n=10)
	{
		napply = function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
		names = ls(pos = pos, pattern = pattern)
		obj.class = napply(names, function(x) as.character(class(x))[1])
		obj.mode = napply(names, mode)
		obj.type = ifelse(is.na(obj.class), obj.mode, obj.class)
		obj.prettysize = napply(names, function(x) { capture.output(print(object.size(x), units = "auto")) })
		obj.size = napply(names, object.size)
		obj.dim = t(napply(names, function(x) as.numeric(dim(x))[1:2]))
		vec = is.na(obj.dim)[, 1] & (obj.type != "function")
		obj.dim[vec, 1] = napply(names, length)[vec]
		out = data.frame(obj.size, obj.prettysize, obj.type, obj.dim)
		names(out) = c("Size", "PrettySize", "Type", "Rows", "Columns")
		out = out[order(out[[order.by]], decreasing=decreasing), ]
		out[ , 1] = out[ , 2]
		out = out[ , -2]
		if (n>0)
			out = head(out, n)
		out
	}



	## sleep with a bit of animated output...
	sleep = function(seconds)
	{
		zzz = function(t)
		{
			flush('Z'); Sys.sleep(t);
			flush('Z'); Sys.sleep(t);
			flush('z'); Sys.sleep(t);
			flush('z'); Sys.sleep(t);
			flush('.'); Sys.sleep(t);
			flush('.'); Sys.sleep(t);
			flush('\b\b\b\b\b\b'); Sys.sleep(t);			
		}

		# by default, you should print the 7 characters every 3.5 seconds...
		loops = ceiling(seconds / 3.5)
		t = seconds / loops / 7
		for (i in 1:loops) zzz(t)
	}





###################################################################################################################
##################      READ BIG DATA      ########################################################################
###################################################################################################################


	# cope with me having data in several locations...
	# if my usb stick isn't plugged in (E:/WORK) try reading data from various hard drive backups..
	fnchk = function(filename, stopOnError=TRUE)
	{
		# is SONY available?
		fn = filename
		if (file.exists(fn)) return(fn)

		# am i at work?
		fn = gsub('E:/', 'M:/', filename)
		if (file.exists(fn)) return(fn)

		# am i at home?
		fn = gsub('E:/', 'C:/Users/istaffel/BLAM!/', filename)
		if (file.exists(fn)) return(fn)

		# am i on my laptop?
		fn = gsub('E:/', 'D:/!SONY!/', filename)
		if (file.exists(fn)) return(fn)

		# is this a dropbox path at home?
		fn = gsub('istaffel', 'TiTS', filename)
		if (file.exists(fn)) return(fn)

		# is this a dropbox path at work?
		fn = gsub('TiTS', 'istaffel', filename)
		if (file.exists(fn)) return(fn)


		# spit an error
		if (stopOnError) stop('OH NO - FNCHK() CANNOT FIND ' %&% filename %&% '\n')

		# or just return junk
		return(fn)
	}




	# read csv, rdata or rds as required...
	read_data = function(filename, verbose=FALSE)
	{
		xtn = get_text_after(filename, '.', last=TRUE)

		if (tolower(xtn) == 'csv')
		{
			data = read_big_csv(filename, verbose)
			return(data)
		}

		if (tolower(xtn) == 'rdata')
		{
			load(filename)
			return(data)
		}

		if (tolower(xtn) == 'rds')
		{
			data = readRDS(filename)
			return(data)
		}

		stop('Error in read_data(', filename, ')\n')
	}


	# any time you read data use these tweaked options for 3x speed!
	read_csv = function(filename, ...)
	{
		filename = fnchk(filename)

		# read the first few lines to understand the column classes
		heading = read.csv(filename, header=TRUE, stringsAsFactors=FALSE, nrows=10, ...)
		classes = sapply(heading, class)

		# class integers as numeric (in case not all of them are ints)
		classes[ classes == 'integer' ] = 'numeric'

		data = tryCatch(
			# read the whole file, stating what classes we expect for 6x speed
			read.csv(filename, header=TRUE, colClasses=classes, comment.char='', ...),

			# if that doesn't work then just read normally (sometimes colClasses causes an error in scan())
			error = function(e) { read.csv(filename, header=TRUE, stringsAsFactors=FALSE) }
		)

		return(data)
	}


	# any time you read more than 100MB use this for 100x speed!
	# use data.table=TRUE to return a data.table
	read_big_csv = function(filename, data.table=FALSE, verbose=FALSE)
	{
		filename = fnchk(filename)

		suppressPackageStartupMessages(library(data.table))
		data = fread(filename, sep=',', na.strings=c('NA','N/A',''), showProgress=verbose, data.table=data.table)

		return(data)
	}


	# read a csv file stuffed inside a zip file
	read_zipped_csv = function(file, ...)
	{
		zipFileInfo = unzip(file, list=TRUE)
		contents = as.character(zipFileInfo$Name)

		if (nrow(zipFileInfo) > 1)
			stop("read_zipped_csv: More than one data file inside your file:", file, "\n")
		
		read.csv( unz(file, contents), stringsAsFactors=FALSE, ...)
	}



	# read several files binding on rows or columns
	read_data_bind_rows = function(filenames, ...)
	{
		# library(plyr)
		# do.call(rbind.fill, lapply(filenames, function(x) read_data(x, ...)))

		library(plyr)
		LIST = list()
		n = length(filenames)

		# read in all files to a list (this doesn't require memory reallocation)
		for (i in 1:n)
		{
			LIST[[i]] = read_data(filenames[i])
			sz = capture.output(print(object.size(LIST), units = "auto"))
			clear_line('Read', sz, '-', i, 'of', n, '-', filenames[i])
		}

		# convert from list to DF (yay, efficiency...)
		clear_line('Reformatting as a data.frame...')
		DF = do.call(rbind.fill, LIST)
		clear_line()
		DF
	}


	read_data_bind_cols = function(filenames, ...)
	{
		do.call(cbind, lapply(filenames, function(x) read_data(x, ...)))
	}








	# write csv, rdata or rds as required...
	write_data = function(data, filename, format)
	{
		# check and correct the extension of our filename
		xtn = get_text_after(filename, '.', last=TRUE)
		xtn = tolower(xtn)
		format = tolower(format)

		if (xtn != format)
		{
			filename = get_text_before(filename, '.', last=TRUE)
			filename = filename %&% '.' %&% format
		}

		if (format == 'csv')
		{
			write_csv(data, filename, row.names=FALSE)
			return()
		}

		if (format == 'rds')
		{
			saveRDS(data, filename)
			return()
		}

		if (format == 'rdata')
		{
			save(data, file=filename)
			return()
		}

		stop('Error in write_data(', filename, ',', format, ')\n')
	}


	# write csv:
	#   accepts col.names=FALSE as an option
	#   you may also want row.names=FALSE
	#   defaults to blank NA strings
	#   accepts append=TRUE as an option
	#
	write_csv = function(data, filename, col.names=TRUE, na='', append=FALSE, ...)
	{
		if (append == TRUE)
		{
			write.table(data, filename, append=TRUE, sep=',', col.names=FALSE, na=na, ...)
			return()
		}

		if (col.names == TRUE)
		{
			write.csv(data, filename, na=na, ...)
			return()
		}

		if (col.names == FALSE)
		{
			write.table(data, filename, sep=',', col.names=FALSE, na=na, ...)
			return()
		}
	}



	# convert csv to rds and vice versa
	#   pass the name of the file to convert 
	#   the format of the input file ('csv', 'rds', 'rdata')
	#   and the format to convert it to
	#
	#   optionally, say which columns contain dates (which should be converted to lubridate)
	#
	convert_data_format = function(filename_in, format_in, format_out, date_cols=NULL)
	{
		# figure out filenames
		filename_out = gsub(        format_in,          format_out,  filename_in)
		filename_out = gsub(tolower(format_in), tolower(format_out), filename_out)
		filename_out = gsub(toupper(format_in), toupper(format_out), filename_out)

		if (filename_out == filename_in)
			stop("convert_data_format(", filename_in, ", ", format_in, ", ", format_out, "): I don't want to overwrite your shit!")

		# read data
		flush("Reading", filename_in)
		data = read_data(filename_in)

		# convert dates to POSIX
		if (!is.null(date_cols))
		{
			clear_line('Converting date_cols...')
			library(lubridate)

			for (d in date_cols)
			{
				test = data[1, d]

				# test 2000-01-01 00:00:00
				t = suppressWarnings( ymd_hms(test) )
				if (!is.na(t))
				{
					data[ , d] = ymd_hms(data[ , d])
					next
				}

				# test 01/01/2000 00:00
				t = suppressWarnings( dmy_hm(test) )
				if (!is.na(t))
				{
					data[ , d] = dmy_hm(data[ , d])
					next
				}

				# test 2000-01-01 00:00
				t = suppressWarnings( ymd_hm(test) )
				if (!is.na(t))
				{
					data[ , d] = ymd_hm(data[ , d])
					next
				}

				# test 01/01/2000 00:00:00
				t = suppressWarnings( dmy_hms(test) )
				if (!is.na(t))
				{
					data[ , d] = dmy_hms(data[ , d])
					next
				}

				# if its not one of those formats... ugh, ignore it
			}
		}

		clear_line('Writing', filename_out)
		write_data(data, filename_out, format_out)

		clear_line()
	}

	csv_to_rds = function(filename_in, ...) { convert_data_format(filename_in, 'csv', 'rds', ...) }
	rds_to_csv = function(filename_in, ...) { convert_data_format(filename_in, 'rds', 'csv', ...) }




	##
	##  merge a set of csv files together, writing to disk
	##  this is done on-disk rather than in-memory, allowing you to create very big files
	##
	merge_csv_files = function(inputFiles, outputFile)
	{
		# check all the input files exist
		if (FALSE %in% file.exists(inputFiles))
			stop("merge_csv_files: some of your input files don't exist!!!\n\n")

		# write the first month with column headers
		data = read_big_csv(inputFiles[1])

		cat('Merging file:', basename(inputFiles[1]))
		flush()

		write_csv(data, outputFile, row.names=FALSE)

		# write the rest of them, appending
		for (i in 2:length(inputFiles))
		{
			data = read_big_csv(inputFiles[i])
		
			clear_line()
			cat('Merging file:', basename(inputFiles[i]))
			flush()

			write_csv(data, outputFile, row.names=FALSE, append=TRUE)
		}

		clear_line()
		cat('Saved files to:', outputFile, '\n')
		flush()
	}




###################################################################################################################
##################      GRAPHICS       ############################################################################
###################################################################################################################


	###
	# #  set up good graphics parameters..  
	# #    pass your own values for mar (margins) or mgp (distance between axes and labels)
	# #    options to keep the interior padding, or rotated y-axis labels
	###
		good_graphics = function
		(
			mar = c(3, 3, 1.5, 1.5),
			mgp = c(1.75, 0.5, 0),
			padding = FALSE,
			captions = TRUE
		)
		{
			# maximise graphics space
			par(bg="white")					# white background for drawing pngs
			par(mar=mar)					# shrink the margins (bottom, left, top, right)
			par(tck=0.01)					# tick marks go inwards
			par(mgp=mgp)					# move axis captions and labels closer
			par(font.lab=2)					# bold axis captions
			
			if (captions == TRUE)  par(las=1)				# make all tick-mark labels horizontal
			if (captions == FALSE) par(las=0)

			if (padding == FALSE) par(xaxs="i", yaxs="i")	# remove padding between the graph and the axes)
			if (padding == 'y')   par(xaxs="i", yaxs="r")	# just remove padding from the x axis (so y axis is padded)
			if (padding == TRUE)  par(xaxs="r", yaxs="r")
		}



	###
	# #  print a decent quality png of your graph
	# #    pass the filename to save as
	# #    pass your own height and width (in pixels) and resolution (in ppi)
	# #    the defaults equate to ~8pt font at 8.5cm width
	###
		good_png = function
		(
			filename,
			height = 2500,
			width = 2500,
			res = 400
		)
		{
			dev.print(png, file=filename, height=height, width=width, res=res)
		}



	##
	##  simple plot functions for lazy fuckers
	##

	plot_p = function(x, y=NULL, pch=16, cex=0.75, col=rgb(0,0,0,0.1), xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, xlim=NULL, ylim=NULL, ...)
	{
		# deal with passing a single variable
		if (is.null(y))
		{
			if (is.vector(x))
			{
				y = x
				x = seq_along(y)

			} else {

				y = x[ , 2]
				x = x[ , 1]
			}

		}

		# default axis limits
		if (is.null(xlim)) xlim = range(x, na.rm=TRUE)
		if (is.null(ylim)) ylim = range(y, na.rm=TRUE)

		# modified axis limits
		if (!is.null(xmin)) xlim[1] = xmin
		if (!is.null(xmax)) xlim[2] = xmax
		if (!is.null(ymin)) ylim[1] = ymin
		if (!is.null(ymax)) ylim[2] = ymax

		# plot
		plot(x, y, pch=pch, cex=cex, col=col, xlim=xlim, ylim=ylim, ...)
	}





	plot_l = function(x, y=NULL, xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, xlim=NULL, ylim=NULL, ...)
	{
		# deal with passing a single variable
		if (is.null(y))
		{
			if (is.vector(x))
			{
				y = x
				x = seq_along(y)

			} else {

				y = x[ , 2]
				x = x[ , 1]
			}

		}

		# default axis limits
		if (is.null(xlim)) xlim = range(x, na.rm=TRUE)
		if (is.null(ylim)) ylim = range(y, na.rm=TRUE)

		# modified axis limits
		if (!is.null(xmin)) xlim[1] = xmin
		if (!is.null(xmax)) xlim[2] = xmax
		if (!is.null(ymin)) ylim[1] = ymin
		if (!is.null(ymax)) ylim[2] = ymax

		# plot
		plot(x, y, type='l', xlim=xlim, ylim=ylim, ...)
	}



	# plot_l = function(x, y=NULL, ...)
	# {
	# 	if (!is.null(y))
	# 		plot(x, y, type='l', ...)
	#
	# 	if (is.null(y))
	# 		plot(x, type='l', ...)
	# }



	##
	##  add transparency to a given colour (e.g. named one)
	##  http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
	##
	alphaColour = function(someColor, alpha=100)
	{
		newColor =col2rgb(someColor)
		apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2], blue=curcoldata[3], alpha=alpha, maxColorValue=255)})
	}



	# a list of parameters to make boxplots nice
	# see http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/bxp.html
	boxplot.pars = list(boxfill = 'grey90', whisklty=1, whiskcol='grey40', outpch=16, outcex=0.4, outcol='grey70')






	heat.colours = c('#3F168A', '#2049D0', '#3288BD', '#66C2A5', '#ABDDA4', '#E6F598', '#FFFFBF', '#FEE08B', '#FDAE61', '#F46D43', '#D53E4F', '#9E0142')
	blue.colours = c('#F7FBFF', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B')

	# go see the random hacks in E:\WORK\Code\R\colour ramps - perceptual hue colour wheel.R
	rainbow.colours.10 = c('#CC00FF', '#8F00FF', '#000AFF', '#00A3FF', '#00FFC2', '#8FFF00', '#FAFF00', '#FFB800', '#FF7A00', '#F00000', '#AA0000')
	rainbow.colours.8 = c('#9E00FF', '#000AFF', '#00A3FF', '#00FFC2', '#70FF00', '#FFF500', '#FF7A00', '#FF0000', '#AA0000')
	rainbow.colours = rainbow.colours.8





	# function to turn your data into a ramp
	# accepts a vector of numbers, returns a vector of hex RGB colours
	#   optionally pass the palette to use (defaults to rainbow.colours)
	#   optionally pass the min-max range to enforce on your values
	# 
	colour_ramp_data = function(x, pal=NULL, range=NULL)
	{
		# normalise our input data
		normalise = function(x, range)
		{
			if (!is.null(range))
			{
				min = min(range)
				max = max(range)

			} else {

				min = min(x, na.rm=TRUE)
				max = max(x, na.rm=TRUE)
			}

		 	x = (x - min) / (max - min)
		 	x[ x < 0 ] = 0
		 	x[ x > 1 ] = 1
		 	x
		}

		x = normalise(x, range)

		# create a palette if needs be
		if (length(pal) < 2)
			pal = rainbow.colours

		# convert the x data to rgb
		myRamp = colorRampPalette(pal)(length(pal))
		cols = colorRamp(myRamp)(x)

		# convert to an rgb array to hex strings
		rgb_to_hex = function(x) { if (is.na(x[1])) return(NA); rgb(x[1], x[2], x[3], maxColorValue=255) }
		apply(cols, 1, rgb_to_hex)
	}



	#
	#  turn standard x-y data series into a stepped x-y series
	#  with vertical transitions at the mid-point between each data point
	#
	#  x and y must be of equal length
	#
	#  this lets you plot a line chart in excel that looks like line(type='s') in R
	#
	#  e.g. 
	#    h = hist(data, plot=FALSE)
	#    h = stepped_line_graph(h$mids, h$counts)
	#    plot(h, type='l')
	#
	stepped_line_graph = function(x, y, mids=TRUE)
	{
		# check
		n = length(x)
		if (n != length(y))
			stop(paste('stepped_line_graph: x and y vectors must have equal length. x =', length(x), '- y =', length(y), '\n'))

		# find the x midpoints
		if (mids)
		{
			dx = diff(x) / 2
			push(dx, tail(dx, 1))
			x = c(x[1]-dx[1], x+dx)
		}

		# or bolt something on the end
		if (!mids)
		{
			dx = diff(x)
			push(x, tail(x,1)+tail(dx,1))
		}

		# build a stepped x sequence
		x = rep(x, each=2)
		x = tail( head(x, -1), -1)

		# build a stepped y sequence
		y = rep(y, each=2)

		data.frame(x=x, y=y)
	}

	# run on multiple columns
	# pass the data frame and column name
	stepped_line_graphs = function(df, x, mids=TRUE)
	{
		# get the data columns
		y = colnames(df)
		y = y[y %notin% x]

		if (length(y) == 0)
			stop('stepped_line_graphs(): no data columns found\n')

		# run through the first
		out = stepped_line_graph(df[ , x], df[ , y[1]], mids)
		colnames(out) = c(x, y[1])

		if (length(y) == 1)
			return(out)

		# run through the rest
		for (i in 2:length(y))
		{
			o = stepped_line_graph(df[ , x], df[ , y[i]], mids)
			out = cbind(out, o[ , 2])
		}

		colnames(out) = c(x, y)

		return(out)
	}




	# return the value of grid lines on a log chart between min and max
	log_gridlines = function(min, max)
	{
		x = list()

		log_min = floor(log10(min))
		log_max = ceiling(log10(max))

		m = log_min:log_max
		m = 10 ^ m

		x$major = m
		x$minor = c(m*2, m*3, m*4, m*5, m*6, m*7, m*8, m*9)
		x$minor = sort(x$minor)

		x$major = x$major[ x$major > min & x$major < max ]
		x$minor = x$minor[ x$minor > min & x$minor < max ]

		x
	}





	# plot a histogram with a line fit
	# data = values to histogram
	#    x = sequence of breaks for the histogram
	#  fit = the pnorm(), pweibull(), etc. function
	hist_and_fit = function(data, x, fit, title='')
	{
		# pre-calculate the histogram
		h = hist(data, breaks=x, plot=FALSE)

		# calculate the density of our fit
		fit = diff(fit) * length(data)

		# use that to determine the ylimits
		ylim = c(0, max(h$counts, fit))

		# and plot
		plot(h, ylim=ylim, main=title, col='grey95')
		lines(midPoints(x), fit, col='red', lwd=2)
	}



	# hist_and_fit for the lazy man
	# automatically fit a normal distribution to your data
	# pass the data, the number of breaks (if you want), and the title (if you want)
	# returns the mean and standard deviation
	hist_and_normal_fit = function(data, breaks=NA)
	{
		if (is.na(breaks)) h = hist(data, plot=FALSE)
		else h = hist(data, breaks=breaks, plot=FALSE)

		x = h$breaks
		mu = mean(data, na.rm=TRUE)
		sd = sd(data, na.rm=TRUE)
		y = pnorm(x, mu, sd)
		title = paste(signif(mu, 3), '\U00B1', signif(sd, 3))

		hist_and_fit(data, x, y, title)

		c(mu, sd)
	}



	# histogram with logged x axis
	# isn't very good - it should have funny shaped bars perhaps?
	#
	hist_log_x = function(data, ...)
	{
		# plot your logged data without an x axis
		data = log10(data)
		hist(data, xaxt='n', ...)

		# design a nice x axis
		t = axTicks(1)
		axis(1, at=t, labels=prettyNum(10**t))
	}



	# histogram with logged y axis
	#
	hist_log_y = function(data, ...)
	{
		# calculate the histogram
		h = hist(data, plot=FALSE, ...)

		# log the counts and add on a nubbin so that counts of 1 are visible
		nubbin = 0.05
		h$counts = log10(h$counts) + nubbin

		# plot without a y-axis
		plot(h, yaxt='n', ylim=c(0, max(h$counts, na.rm=TRUE)*1.02))

		# get the tick locations - round them off so you get nice numbers - then add on our nubbin
		t = axTicks(2)
		t = signif(10**t, 1)
		t = log10(t) + nubbin

		# print the axis values (without our nubbin)
		axis(2, at=t, labels=prettyNum(10**(t-nubbin)))
	}




##################################################################################################################
##################      MAP TOOLS      ###########################################################################
##################################################################################################################


	##
	##  Grab a world map from either:
	##     the shapefile of your choosing
	##     rworldmap's getMap with the resolution of your choosing
	##
	##  return the spatial data frame thing...
	##
	##  my usual maps are:
	##     E:/WORK/Z Data/Maps/Natural Earth/50m Countries/ne_50m_admin_0_countries.shp
	##     E:/WORK/Z Data/Maps/Natural Earth/Admin Map Subunits/ne_10m_admin_0_map_subunits.shp
	##     E:/WORK/Z Data/Maps/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp
	##
	get_world_map = function(shapeFile=NULL, res='high', wgsProjection=TRUE)
	{
		# use a custom shapefile
		if (!is.null(shapeFile))
		{
			# if we used this shapefile previously
			if (exists('world.map.shape') & exists('world.map'))
			{
				# then don't load it again!
				if (world.map.shape == shapeFile)
				{
					return(world.map) 
				}
			}

			# load the shapefile
			suppressPackageStartupMessages(library(maptools))
			world.map = readShapeSpatial(shapeFile)

			# not necessary, but yields nicer default axis labels and aspect ratio
			if (wgsProjection == TRUE)
				proj4string(world.map) = "+proj=longlat +datum=WGS84"

			# remember the shapefile we used
			world.map.shape <<- shapeFile
		}

		# otherwise use rWorldMap
		if (!exists('world.map'))
		{
			suppressPackageStartupMessages(library(rworldmap))
			world.map = getMap(res)
		}

		return(world.map)
	}



	##
	##  Grab a map of Europe, with non-members cut out
	##  and far-flung colonies cropped out using a bounding box
	##  
	##  optionally set disembodied=TRUE to remove CIS and Turkey, making EU look like an island of paradise..
	##
	##  only works with the rWorldMap maps for now...
	##
	get_europe_map = function(resolution='high', disembodied=FALSE)
	{
		# load a world map
		suppressPackageStartupMessages(library(rworldmap))
		world.map <<- getMap(resolution)
		

		# figure out which countries are in europe

		# identify countries by their general region
		eu = world.map@data$REGION == 'Europe'
		eu[is.na(eu)] = FALSE

		# filter out ones that aren't there
		if (disembodied)
		{
			not.really.europe = c('RUS', 'BLR', 'UKR', 'GRL', 'TUR', 'ARM', 'AZE', 'GEO')
			eu[world.map$ISO3 %in% not.really.europe] = FALSE
		}

		# filter down our world map
		europe.map = world.map[eu, ]

		# filter out the colonies
		suppressPackageStartupMessages(library(raster))

		if (disembodied)
			europe.map = crop(europe.map, extent(-25, 32, 34, 72))

		if (!disembodied)
			europe.map = crop(europe.map, extent(-25, 32+10, 34, 72))


		return(europe.map)
	}

	get_europe_north_africa_map = function(resolution='high')
	{
		# load a world map
		suppressPackageStartupMessages(library(rworldmap))
		world.map <<- getMap(resolution)
		

		# figure out which countries are in europe

		# identify countries by their general region
		eu = world.map@data$REGION == 'Europe'
		eu[is.na(eu)] = FALSE

		# filter out ones that aren't there
		not.really.europe = c('RUS', 'BLR', 'UKR', 'GRL', 'TUR', 'ARM', 'AZE', 'GEO')
		eu[world.map$ISO3 %in% not.really.europe] = FALSE

		# also grab north africa
		na = world.map@data$ISO3 %in% c('MAR', 'DZA', 'LBY', 'EGY')

		# filter down our world map
		europe.map = world.map[eu | na, ]


		# filter out the colonies
		suppressPackageStartupMessages(library(raster))
		europe.map = crop(europe.map, extent(-25, 37, 18, 72))


		return(europe.map)
	}

	# iso3 should come from world.map@data$ISO3
	# extent should be c(left, right, bottom, top)
	get_country_map = function(iso3, extent=NULL, resolution='high')
	{
		# load a world map
		suppressPackageStartupMessages(library(rworldmap))
		world.map <<- getMap(resolution)

		# filter down to our countries
		keep = world.map@data$ISO3 %in% iso3
		my.map = world.map[keep, ]

		# crop to a specific extent
		if (length(extent) == 4)
		{
			library(raster)
			my.map = crop(my.map, extent(extent))
		}

		return (my.map)
	}





	##
	##  Plot a map with points at your specified coordinates
	##   - pass longitude and latitude as numeric vectors
	##   - optionally set the boundaries of the map with lonBounds and latBounds = c(minimum, maximum)
	##     otherwise these are calculated automatically to cover all points
	##   - optionally set how many degrees of padding to add around auto-generated bounds
	##   - optionally specify the border and fill colour for countries
	##   - optionally specify the aspect ratio to use (1.5 default for UK)
	##   - optionally specify your own shapefile to render
	##   - optionally specify the height of the dev() window
	##   - optionally send other parameters on to points()
	##
	##   - returns the ratio of width to height you should use if printing the file as a png
	##
	##   - will create and store map shape in the world.map global variable to save time on future calls...
	##
	##
	##  if you want to plot a blank map with no points (useful to get the right boundaries) then just set col=NULL
	##
	##  note that this makes a new graphics window with new par().  this will let you continue editing the map, 
	##  but if you start making new plots in this window they will look funny.  just call dev.new() afterwards.
	##
	##
	##  TO DO:
	##    - different projections, motherfucker!!!
	##
	##    - if i want to repeat this with colour fills for each country
	##      see http://stackoverflow.com/questions/1260965/developing-geographic-thematic-maps-with-r
	##
	plot_map_points = function(lon, lat, xlim=range(lon, na.rm=TRUE), ylim=range(lat, na.rm=TRUE), lonBounds=NULL, latBounds=NULL, padding=1,
		                       mapBorder='grey75', mapFill='grey95', mapLwd=1,
		                       mapAspect=1.5, shapeFile=NULL, height=7, new.plot=TRUE, add.points=FALSE, ...)
	{
		# get our world map and save globally for future
		world.map <<- get_world_map(shapeFile)

		# add some padding around the map
		xlim = range(xlim-padding, xlim+padding)
		ylim = range(ylim-padding, ylim+padding)

		# work out the aspect ratio
		ar = diff(xlim) / diff(ylim) / mapAspect

		# build a new plot window with the correct ratio
		if (new.plot == TRUE)
		{
			dev.new(height=height, width=height*ar)
		}

		if (add.points == FALSE)
		{
			# remove our plot margins
			par(bg="white")
			par(mar = rep(0, 4))

			# plot the map
			plot(world.map, xlim=xlim, ylim=ylim, border=mapBorder, col=mapFill, asp=mapAspect, lwd=mapLwd)
		}

		# plot the points
		points(lon, lat, ...)

		# return our ideal width:height ratio
		# then call good_png(height=1000, width=1000*ar)
		ar
	}




###################################################################################################################
##################      TEXT TOOLS      ###########################################################################
###################################################################################################################


	# reverse a string
	strrev = function(x) { reverse_string(x) }
	reverse_string = function(x)
	{
		sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
	}

	# get a subsection of a string, working from the end backwards
	substr_reverse = function(x, n)
	{
		substr(x, nchar(x)-n+1, nchar(x))
	}

	# replace 'x' for 'y' in a string
	str_replace = function(...)
	{
		gsub(..., fixed=TRUE)
	}


	# get text before, after, or between tokens
	get_text_before = function(string, token, last=FALSE)
	{
		chunks = strsplit(string, token, fixed=TRUE)

		if (last == FALSE)
			fun = function(x) { x[1] } 

		if (last == TRUE)
			fun = function(x) { paste(head(x, -1), collapse=token) }

		sapply(chunks, fun)
	}


	get_text_after = function(string, token, last=FALSE)
	{
		chunks = strsplit(string, token, fixed=TRUE)

		if (last == TRUE)
			fun = function(x) { tail(x, 1) } 

		if (last == FALSE)
			fun = function(x) { paste(tail(x, -1), collapse=token) }

		sapply(chunks, fun)
	}


	get_text_between = function(string, token1, token2)
	{
		string = get_text_after(string, token1)
		string = get_text_before(string, token2)
		string
	}




	# convert an array of key-val pairs into a dataframe
	# keyval = array('name=Iain', 'hair=NA')
	# splitter = '='
	keyval_to_df = function(keyval, splitter)
	{
		# parse your string to a data frame
		df = as.data.frame(sapply(strsplit(keyval, splitter), rbind), stringsAsFactors=FALSE)
		
		# apply the header
		names(df) = df[1,]
		df = df[-1,]

		return(df)
	}

	# convert an array of arrays of key-val pairs into a dataframe
	all_keyval_to_df = function(keyval_array, splitter)
	{
		# initialise results storage
		DF = NULL

		# run through each keyval string
		for (keyval in keyval_array)
		{
			df = keyval_to_df(keyval, splitter)
			DF = merge(DF, df, all=TRUE, sort=TRUE)
		}

		return(DF)
	}



	# convert one/some numbers into percentage format
	percent = function(val, dp=2)
	{
		paste0(round(100*val, dp), "%")
	}




###################################################################################################################
##################      ARRAY TOOLS      ##########################################################################
###################################################################################################################


	# push a value onto the end of a vector
	# in case you're too lazy to type array[length(array)+1] = item...
	push = function(array, item)
	{
		new_array = c(array, item)
		eval.parent(substitute(array <- new_array))
	}

	# pop an item off the end of a vector
	# and return the item
	pop = function(array)
	{
		item = array[ length(array) ]
		new_array = array[ -length(array) ]
		eval.parent(substitute(array <- new_array))
		item
	}


	# http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
	# rbind a new row into a specific place of a data frame
	insert_row = function(DF, row, r)
	{
		s = seq(r, nrow(DF))
		DF[s+1, ] = DF[s, ]
		DF[r, ] = row
		DF
	}



	#
	#  multi-dimensional rbind/cbind equivalent...
	#
	#  binds two arrays on the xth dimension
	#  the arrays must have the same number of dimensions, and this can be any number
	#
	#  for example:
	#  xbind(a[x,y,z1], b[x,y,z2], 3) returns c[x,y,z1+z2]...
	#
	#  xbind(a[x,y], b[x,y], 1) == rbind(a[x,y], b[x,y])
	#  xbind(a[x,y], b[x,y], 2) == cbind(a[x,y], b[x,y])
	#
	xbind = function(a, b, x)
	{

		# check we are passing arrays
		if (!is.array(a) | !is.array(b) | !is.numeric(x))
			stop('serious? this ting deals with arrays...')

		# check the number of dimensions
		n = length(dim(a))

		if (length(dim(b)) != n)
			stop('xbind only does arrays with the same number of dimensions fam!\n')

		
		# check the sizes are correct in other dimensions
		for (i in 1:n)
		{
			if (i == x)
				next

			if (dim(a)[i] != dim(b)[i])
				stop('blud are you mad? xbind only needs your arrays to have the same size in the non-x dimension!\n')
		}

		
		# work out the interleaving
		cuts = 1

		for (i in n:1)
		{
			if (i == x)
				break

			cuts = cuts * dim(a)[i]
		}

		
		# convert to a set of 1d vectors
		aa = matrix(a, ncol=cuts)
		bb = matrix(b, ncol=cuts)

		
		# join together and reshape
		cc = rbind(aa, bb)

		newdim = (1:n %in% x) * 1
		newdim = dim(a) + dim(b) * newdim

		c = array(cc, dim=newdim)

		
		# add on the dimension names
		for (i in 1:n)
		{
			if (i == x)
			{
				dimnames(c)[[i]] = c(dimnames(a)[[i]], dimnames(b)[[i]])
			}

			if (i != x)
			{
				dimnames(c)[[i]] = dimnames(a)[[i]]
			}
		}


		# bumbaraasclart
		# n.a.bass
		c
	}

	if (0)
	{
		a = array(c(111, 211, 121, 221, 112, 212, 122, 222), dim=c(2,2,2))
		a[1, , ]
		a[ , 1, ]
		a[ , , 1]

		b = array(c(311, 411, 321, 421, 312, 412, 322, 422), dim=c(2,2,2))
		c = array(c(131, 231, 141, 241, 132, 232, 142, 242), dim=c(2,2,2))
		d = array(c(113, 213, 123, 223, 114, 214, 124, 224), dim=c(2,2,2))

		xbind(a, b, 1)
		xbind(a, c, 2)
		xbind(a, d, 3)
	}



	# return the array indices for the min/max element
	# e.g. x = array(runif(10000), dim=c(10,10,100)); which.min.arr(x);
	which.min.arr = function(x)
	{
		w = which.min(x)
		w = which(x == x[w], arr.ind=TRUE)
		w
	}

	which.max.arr = function(x)
	{
		w = which.max(x)
		w = which(x == x[w], arr.ind=TRUE)
		w
	}

	# return the array element that is closest to x
	which.closest = function(data, x)
	{
		which.min(abs(data - x))
	}


	# http://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column
	nth_largest = function(data, n)
	{
		n = length(data) + 1 - n
		if (n < 1) return (NA)

		if (is.unsorted(data))
			return ( sort(data, partial=n)[n] )

		return (data[n])
	}

	nth_smallest = function(data, n)
	{
		if (n < 1 | n > length(data)) return (NA)
		
		if (is.unsorted(data))
			return ( sort(data, partial=n)[n] )

		return (data[n])
	}



	# Multiply each column of a matrix by each element of the vector (i.e. each row gets multiplied by the whole vector)
	# http://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
	multiply_each_column = function(matrix, vector)
	{
		result = t( t(matrix) * vector )
		as.data.frame(result)
	}

	mult_cols_by_vector = function(matrix, vector)
	{
		result = t( t(matrix) * vector )
		as.data.frame(result)
	}

	multiply_each_row = function(matrix, vector)
	{
		result = matrix * vector
		as.data.frame(result)
	}

	mult_rows_by_vector = function(matrix, vector)
	{
		result = matrix * vector
		as.data.frame(result)
	}


	# calculate the arithmetic center of adjacent values in an array
	midPoints = function(x)
	{
		(x[-length(x)] + x[-1]) / 2
	}


	# test if a string (or array of strings) holds valid numbers
	is_string_numeric = function(data)
	{
		suppressWarnings(!is.na(as.numeric(data)))
	}


	# return a table in ascending frequency order
	freq_table = function(data)
	{
		# create the table
		tbl = as.data.frame(table(data))
		colnames(tbl) = c('data', 'freq')

		# convert data from factors to strings or numeric
		tbl$data = as.character(tbl$data)
		is_numeric = is_string_numeric(tbl$data)
		if (length(is_numeric) == sum(is_numeric))
			tbl$data = as.numeric(tbl$data)

		# return
		tbl[ order(tbl[2]), ]
	}




	#
	#  move a column in a data frame to a different position
	#
	#  data = the data frame
	#  myCol = the column(s) you wish to move
	#  where = 'first', 'last', 'before', 'after'
	#  relCol = the relative column to move before or after
	#
	#  e.g. move_column(data, c('b','c'), 'before', 'e')
	#
	#  http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
	#
	move_column = function(data, myCol, where = "last", relCol = NULL)
	{
		otherCols = setdiff(names(data), myCol)

		x = switch(where,

			first = data[c(myCol, otherCols)],

			last = data[c(otherCols, myCol)],

			before =
			{
				if (is.null(relCol)) stop("must specify relCol column")
				if (length(relCol) > 1) stop("relCol must be a single character string")
				data[append(otherCols, values = myCol, after = (match(relCol, otherCols)-1))]
			},

			after =
			{
				if (is.null(relCol)) stop("must specify relCol column")
				if (length(relCol) > 1) stop("relCol must be a single character string")
				data[append(otherCols, values = myCol, after = (match(relCol, otherCols)))]
			}
		)

		x
	}



	# calculate the moving average of a vector of data
	moving_average = function(data, n=5)
	{
		# integer filter width
		if (n%%1 == 0)
			return( as.numeric( filter(data, rep(1/n, n), sides=2) ) )

		# otherwise interpolate between the nearest integers
		nlo = floor(n)
		nhi = ceiling(n)

		a = filter(data, rep(1/nlo, nlo), sides=2)
		b = filter(data, rep(1/nhi, nhi), sides=2)

		return( as.numeric( a*(nhi-n) + b*(n-nlo) ))
	}


	moving_average_full = function(data, n=5)
	{
		data = c(head(data, n), data, tail(data, n))

		smooth = moving_average(data, n)

		smooth = head(smooth, -n)
		smooth = tail(smooth, -n)

		smooth
	}




	# do the same as diff(x, lag), except return the biggest swing
	# seen from now across the next lag elements...
	# i.e. x = c(3, 0, 3, 0); diff(x, 2) will return 0, 0
	#      but biggest_diff(x, 2) will return 3, 3
	biggest_diff = function(x, lag)
	{
		if (lag == 1)
			return( diff(x) )

		# initialise
		DX = x*0

		for (i in 1:lag)
		{
			# pop the last element off our previous array
			DX = DX[ -length(DX) ]

			# calculate this new array
			dx = diff(x, i)

			# figure out which is biggest
			w = (abs(dx) > abs(DX))
			DX[w] = dx[w]
		}

		DX
	}



	# censor all rows in a data.frame where any column is already censored
	# i.e. mirror NAs across all of the chosen columns if any of them is NA
	#
	na.everyone = function(dataframe, cols=colnames(dataframe))
	{
		# find NA in all rows/columns
		filter = is.na(dataframe[ , cols])

		# find rows where any column is NA
		filter = apply(filter, 1, any)
		filter = as.vector(filter)

		# NA all columns in those rows
		for (c in cols)
		{
			dataframe[filter, c] = NA
		}

		# return
		dataframe
	}



	list_to_data_frame = function(l)
	{
		# we need stringsAsFactors to be false...
		osaf = options('stringsAsFactors')
		options(stringsAsFactors=FALSE)

		# convert the list to the data frame
		d = do.call(rbind.data.frame, l) # this but it requires options(stringsAsFactors=FALSE) first ...

		# reset stringsAsFactors
		options(stringsAsFactors=osaf[[1]][[1]])

		d
	}



	# aggregate a vector or a data.frame into groups of a fixed size
	# e.g. aggregate_n(my_data, 2) to average half-hourly to hourly
	#
	aggregate_n = function(values, n, FUN='mean')
	{

		# work out our counter
		if (!is.data.frame(values))
			y = seq_along(values)

		if (is.data.frame(values))
			y = seq_along(values[ , 1])

		# aggregate our counter every n
		y = floor( (y-1) / n )

		# now aggregate our values
		z = aggregate(values, by=list(y), FUN)

		# return without the Group.1
		z[ , -1]

	}




###################################################################################################################
##################      TIME-SERIES      ##########################################################################
###################################################################################################################

	aggregate_yearly = function(dates, values, FUN='mean')		aggregate_ts(dates, values, 'yearly', FUN)
	aggregate_monthly = function(dates, values, FUN='mean')		aggregate_ts(dates, values, 'monthly', FUN)
	aggregate_weekly = function(dates, values, FUN='mean')		aggregate_ts(dates, values, 'weekly', FUN)
	aggregate_daily = function(dates, values, FUN='mean')		aggregate_ts(dates, values, 'daily', FUN)
	aggregate_hourly = function(dates, values, FUN='mean')		aggregate_ts(dates, values, 'hourly', FUN)

	aggregate_ts = function(dates, values, by='yearly', FUN='mean')
	{
		# forbid failure
		if (by %notin% c('daily', 'weekly', 'monthly', 'yearly', 'hourly', 'seasonal diurnal'))
			stop("aggregate_ts: don't know how you want to aggregate things dear boy...")


		# pre-process our dates into the relevant chunks
		process_dates = function(dates, by)
		{
			if (by == 'yearly')  dates = format(dates, "%Y-01-01")
			if (by == 'monthly') dates = format(dates, "%Y-%m-01")
			if (by == 'weekly')  dates = round_date(dates, 'week')
			if (by == 'daily')   dates = format(dates, "%Y-%m-%d")
			if (by == 'hourly')  dates = format(dates, "%Y-%m-%d %H:00")
			
			if (by == 'MUSE') # this gives something appropriate for MUSE and other energy systems models quarterly and three hourly...
			{
				season = floor( ((month(dates) + 1) %% 12) / 3)
				hour = 3 * floor(hour(dates) / 3)	
				dates = sprintf("Y%4d Q%i H%02i", year(dates), season, hour)
			}

			dates
		}


		# we want to process a single time series
		# dates = {.....}, values = {.....}
		if (!is.data.frame(values))
		{
			my_dates = process_dates(dates, by)
			return( aggregate_ts_once(my_dates, values, by, FUN) )
		}

		# we want to process a simple data frame
		# dates = 'colname', values = data.frame(.....)
		if (is.character(dates) & is.data.frame(values) & ncol(values) == 2)
		{
			my_dates = values[ , colnames(values) == dates]
			my_dates = process_dates(my_dates, by)

			my_values  = values[ , colnames(values) != dates]

			return( aggregate_ts_once(my_dates, my_values, by, FUN) )
		}

		# we want to process a whole data frame
		# dates = 'colname', values = data.frame(.....)
		if (is.character(dates) & is.data.frame(values))
		{
			# strip out dates and data
			my_dates = values[ , colnames(values) == dates]
			my_values  = values[ , colnames(values) != dates]
		}

		# we want to process a data frame of pure data
		if (!is.character(dates) & is.data.frame(values))
		{
			my_dates = dates
			my_values = values
		}

		cols = colnames(my_values)
		my_dates = process_dates(my_dates, by)

		# process the first column, creating the output structure
		c = cols[1]
		results = aggregate_ts_once(my_dates, my_values[ , c], by, FUN)
		colnames(results)[2] = c

		# determine frequency of updates
		i_tick = 1
		if (nrow(values) < 10000) i_tick = 10
		if (nrow(values) < 100)   i_tick = 100



		# process subsequent columns
		for (i in 2:length(cols))
		{
			c = cols[i]
			res = aggregate_ts_once(my_dates, my_values[ , c], by, FUN, lube.dates=FALSE)
			results[ , c] = res$value

			if (i %% i_tick == 0) clear_line('Aggregating', i, 'of', length(cols))
		}
		clear_line()

		return( results )
	}


	aggregate_ts_once = function(dates, values, by='yearly', FUN='mean', lube.dates=TRUE)
	{
		# do something different and special if we want a seasonal-diurnal aggregation
		if (by == 'seasonal diurnal')
		{
			hour = hour(dates)
			month = month(dates)

			season = floor( (month %% 12) / 3)

			results = aggregate(values, by=list(hour, season), FUN=mean, na.rm=TRUE)
			results = data.frame(array(results$x, dim=c(24,4)))
			colnames(results) = c('winter', 'spring', 'summer', 'autumn')

			# this hands back a custom format, so skip the rest
			return(	results )
		}

		# aggregate our data as desired
		results = aggregate(values, by=list(dates), FUN=FUN)

		# convert to a nice format
		colnames(results) = c('date', 'value')

		if (lube.dates)
		{
			if (by != 'hourly') results$date = ymd(results$date)
			if (by == 'hourly') results$date = ymd_hm(results$date)
		}

		return( results )
	}




	# yea boy 
	seasonal_diurnal = function(dates, values, FUN='mean')		aggregate_ts(dates, values, 'seasonal diurnal', FUN)

	# a function to help with plotting diurnal data
	plot_diurnal = function(diurnal, ...)
	{
		good_graphics(padding=TRUE)

		range = range(diurnal*0.98, diurnal*1.02)

		 plot(0:23, diurnal$summer, col='red', type='l', xlab="Hour", ylab="CF", ylim=range, xaxt='n', ...)
		lines(0:23, diurnal$spring, col='green')
		lines(0:23, diurnal$autumn, col='brown')
		lines(0:23, diurnal$winter, col='blue')

		axis(1, at=seq(0,24,6))
		axis(1, at=seq(0,24,3), labels=FALSE)

		axis(3, at=seq(0,24,3), labels=FALSE)

		axis(4, labels=FALSE)

	}



	# additional lubridate functions because I am exceptionally lazy
	dmy_hs = function(string) { dmy_hms(paste(string, '00', sep=':')) }
	ymd_hs = function(string) { ymd_hms(paste(string, '00', sep=':')) }






	# lag a vector of data by a specified number of (non-integer) steps
	# i.e. lagging by one will turn [4,1,2] into [ ,4,1]
	# you can lag by fractional amounts, which uses linear interpolation
	lag_data = function(data, steps)
	{
		while (steps >= 1)
		{
			# lag by 1
			data = c(NA, head(data, -1))
			steps = steps - 1
		}

		if (steps > 0)
		{
			# lag by a fraction
			data = c(NA, head(data, -1) + diff(data)*steps)
		}

		data
	}

	# opposite of lagging data
	lead_data = function(data, steps)
	{
		data = rev(data)
		data = lag_data(data, steps)
		rev(data)
	}




	#
	#  align two data frames based on the values of the given columns
	#  e.g. you have two sets of time-series data, and you want them aligned in time
	#       so that sim$date == act$date
	#       
	#       res = align_date(sim, 'date', act, 'date')
	#       sim = res[[1]]; act = res[[2]];
	#
	align_data = function(df1, col1, df2, col2)
	{
		# shrink
		df1 = df1[ (df1[ , col1] %in% df2[ , col2]), ]
		df2 = df2[ (df2[ , col2] %in% df1[ , col1]), ]

		# align
		df1 = df1[ match(df2[ , col2], df1[ , col1]), ]
		df2 = df2[ match(df1[ , col1], df2[ , col2]), ]

		list(df1, df2)
	}



	# search through the list of lubridate's timezones
	# e.g. tz_search('Paris')
	# 
	tz_search = function(str)
	{
		tz = olson_time_zones()
		tz[ grep(str, tz) ]
	}


	# calculate the shift in hours needed to move to that time-zone
	# this accounts for daylight savings time, and works on a series of lubridates
	#	
	tz_shift = function(t, zone)
	{
		z = force_tz(t, zone)
		(t - z) / dhours(1)
	}

	# e.g. 
	# t = seq(dmy('01/01/2013'), dmy('01/01/2015'), by='hour')
	# shift = tz_shift(t, 'Europe/Paris')
	# plot(shift)



	# x is your data frame
	# datecol is the column where the dates lives
	# period can be day, hour, or anythingelse that seq() accepts
	# fill optionally contains what you'd like your missing timesteps to contain (e.g. 0)
	fill_missing_dates = function(x, datecol, period='day', fill=NA)
	{
		min = min(x[ , datecol])
		max = max(x[ , datecol])

		all_dates = seq(from=min, to=max, by=period)

		m = match(all_dates, x[ , datecol])
		f = is.na(m)

		all_dates = data.frame(all_dates)

		for (c in 2:ncol(x))
		{
			all_dates[ , c] = x[m, c]
			all_dates[f, c] = fill
		}

		colnames(all_dates) = colnames(x)
		all_dates
	}




###################################################################################################################
##################      STATISTICS      ###########################################################################
###################################################################################################################


	# calculate the mode
	mode = function(x)
	{
		ux = unique(x)
		ux[which.max(tabulate(match(x, ux)))]
	}


	# already exists in the base package
	weighted_mean = function(x, w)
	{
		weighted.mean(x, w)
	}


	# calculate weighted standard deviation
	weighted_sd = function(x, w)
	{
		w = w / sum(w)
		mu = weighted.mean(x, w)
		sd = sqrt( sum(w * (x - mu)^2) )
		return(sd)
	}


	# calculate standard error on the mean
	stderr = function(x) { sqrt(var(x, na.rm=TRUE) / length(na.omit(x))) }


	# function to calculate 95% confidence interval for a normally distributed variable with unknown mean and standard deviation
	ci95 = function(x)
	{
		x = na.omit(x)
		n = length(x)

		return( qt(0.975, df=(n-1)) * sd(x) / sqrt(length(x)) )
	}


	# return the adjusted R² value from an lm() fit
	r2 = function(fit)
	{
		summary(fit)$adj.r.squared
	}


	# return the coefficients and errors of a regression as a single row
	coef.err = function(fit)
	{
		coef = summary(fit)$coefficients[ , 1]
		err = summary(fit)$coefficients[ , 2]

		cbind(t(coef), t(err))
	}

	lm.coef.err = function(formula)
	{
		fit = lm(formula)
		coef.err(fit)
	}


	# calculate root mean squared
	rms = function(x)
	{
		sqrt( mean( x^2, na.rm=TRUE) )
	}


	# rescale a vector to be in the range of [min, max]
	rescale = function(data, min=0, max=1)
	{
		# rescale to [0, 1]
		data = (data - min(data)) / diff(range(data))

		# rescale to our limits
		data = min + (data * (max - min))

		# return
		data
	}


	## generate a sequence of random numbers following a given distribution
	## but, constrain the distribution to lie within set bounds (min to max)
	## any numbers lying outside those bounds will be re-evaluated
	## 
	## e.g. constrained_distro('rnorm', 1000, 1, 2, 0, 1)
	##
	constrained_distro = function(distro, length, min, max, ...)
	{
		# e.g. rnorm(length, parm1, parm2)
		x = do.call(distro, list(length, ...) )

		while (1)
		{
			filter = (x < min | x > max)
			if (sum(filter) == 0) break
			x[filter] = do.call(distro, list(sum(filter), ...) )
		}

		x
	}



