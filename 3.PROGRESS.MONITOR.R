
	# the options files being used in your VWF calculation
	farmOptionsFile = 'Q:/VWF/data/INPUT_CH.R'
	baseOptionsFile = 'Q:/VWF/data/BASE.OPTIONS.R'


	# update frequency in seconds
	update_time = 4

	# establish the progress monitor
	# this will continually update until you exit this R instance
	source(baseOptionsFile)
	source('Q:/VWF/lib/VWF.MAIN.PROGRESS.R')
