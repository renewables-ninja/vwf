##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2012-2017  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################


	# external dependencies (that are always needed)
	suppressPackageStartupMessages(library(ncdf4))
	suppressPackageStartupMessages(library(lubridate))
	suppressPackageStartupMessages(library(rworldmap))
	suppressPackageStartupMessages(library(doParallel))
	suppressPackageStartupMessages(library(data.table))
	suppressPackageStartupMessages(library(akima))

	# external dependencies (not needed by all things, so not loaded by default)
	#library(MASS)
	#library(fields)




	# figure out the working directory for this VWF code
	if (!is.null(parent.frame(2)$ofile))
	{
		vwf.dir = dirname(parent.frame(2)$ofile) %&% '/'
	}

	# if that failed, establish the default
	if (is.null(vwf.dir))
	{
		vwf.dir = 'Q:/VWF/lib/'
	}



	# i'm a concatinator, twisted concatinator
	`%&%` = function(a, b) paste0(a, b)

	# load the background code
	source(vwf.dir %&% 'VWF.STDLIB.R')
	source(vwf.dir %&% 'VWF.NCDF.R')
	source(vwf.dir %&% 'VWF.EXTRAPOLATE.R')
	source(vwf.dir %&% 'VWF.FARMS.R')
	source(vwf.dir %&% 'VWF.PLOTS.R')
	source(vwf.dir %&% 'VWF.EXTRAS.R')

	flush('Welcome to the VWF v17.07.21\n')
