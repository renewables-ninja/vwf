

##########################
##                      ##
##   GENERAL OPTIONS    ##
##                      ##
##########################


	# our wind farms file with coordinates, turbine models, etc.
	farmOptionsFile = 'Q:/VWF/data/INPUT_CH.R'

	# our base options file with model parameters, etc.
	baseOptionsFile = 'Q:/VWF/data/BASE.OPTIONS.R'




##########################
##                      ##
##       PREPARE        ##
##                      ##
##########################


	# load the VWF model and required libraries
	#
	source('Q:/VWF/lib/VWF.R')

	# prepare the VWF model with your parameters
	#   check ability to save output files
	#   read in wind farms and power curves
	#   prep the merra data for reading in
	#   launch multi-core cluster
	#   establish spatial interpolation and bias correction code
	#
	source(baseOptionsFile)
	source('Q:/VWF/lib/VWF.MAIN.PREP.R')

	# key things you now have:
	#   windFarms  - dataframe containing our wind farms
	#   farmCurve  - dataframe containing our power curves
	#   merra_wind - object holding our merra filenames
	#   nc_wind    - object for handling netcdf files
	#   cl         - multicore cluster object




##########################
##                      ##
##    GET WIND SPEED    ##
##                      ##
##########################

	# read in pre-calculated speeds if we have them, otherwise go and interpolate & extrapolate them
	# this is a very slow stage - go fire up 3.PERFORMANCE.MONITOR.R to see how it's going..
	source('Q:/VWF/lib/VWF.MAIN.WINDSPEED.R')

	# key things you now have:
	#   windSpeed - dataframe containing hourly wind speeds for each farm
	#               this is also saved into baseSaveFolder



##########################
##                      ##
##    GET WIND POWER    ##
##                      ##
##########################

	source('Q:/VWF/lib/VWF.MAIN.WINDPOWER.R')

	# key things you now have:
	#   windSpeed  - now modified for bias correction
	#   loadFactor - dataframe containing the hourly capacity factors for each farm
	#   powerMW    - dataframe containing the hourly power output for each farm
	#   parms      - dataframe containing the parameters used for each farm



##########################
##                      ##
##     SAVE RESULTS     ##
##                      ##
##########################

	source('Q:/VWF/lib/VWF.MAIN.RESULTS.R')
	flush('\n\nFLAWLESS\n\n')
