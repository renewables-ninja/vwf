# The Virtual Wind Farm (VWF) model

`vwf` is an R project to simulate the power output from wind farms based on NASA weather data.  The wind energy simulations on [Renewables.ninja](https://www.renewables.ninja/) are based on the VWF model.



## REQUIREMENTS

An unsound mind and plenty of alcohol.  

[R](https://www.r-project.org/) or [MRO](https://mran.revolutionanalytics.com/open/) version 3+

Required libraries:
 * ncdf4
 * lubridate
 * rworldmap
 * doParallel
 * data.table
 * akima
 * MASS
 * fields



## SETUP

### Download wind speed data
First, download the necessary input data from NASA's [MERRA-2 reanalysis](https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/), specifically the [SLV tables](http://dx.doi.org/10.5067/VJAFPLI1CSIV) (M2T1NXSLV).

The easiest (and most wasteful) option is to use the 'Online archive' link and download the complete files (approx 400 MB per day).  Alternatively you could use the various subsetting tools available, selecting the DISPH, U2M, V2M, U10M, V10M, U50M and V50M variables, and whatever region and time period you are interested in.  Good luck, they rarely function correctly!

### Set up the VWF model
With the required libraries installed, the VWF code should hopefully just require you to edit the paths to the include files and your data files.  These are dotted all over the place, so you're best to search for ":/" in all the files to check you have found all paths.

### Process wind speed data
Performing the wind speed extrapolation (working out wind speed at the specific height of your turbine) is very computationally expensive, so the VWF calculation engine uses pre-compiled wind speed profile data, in the form of the `A` and `z` parameters of the logarithmic profile law.

First, you need to run one of the `1.COMPILE.WIND.PROFILE.R` files, editing the paths to your input, template and output files.  On a standard desktop, this takes around a week per year of input data.  I said it was computationally expensive.  Alternatively, perhaps we could collaborate on some fun research together?  

This process should yield a set of NetCDF files, one per day, containing the wind profile parameters for each location and hour of the day.  These are about 30 MB each, so around 10 GB per year. 



## USAGE INSTRUCTIONS

First, run either `1.COMPILE.WIND.PROFILE (merra1).R` or `1.COMPILE.WIND.PROFILE (merra2).R` to generate your wind speed profiles, as detailed above.

Second, create a simulation input file describing the wind farms you wish to simulate.  An example covering 39 wind farms in Switzerland is provided in `data\INPUT_CH.CSV` and `data\INPUT_CH.R`

Third, decide on the model parameters you wish to use.  There are various nobs and switches to tweak, with a set of 'safe' defaults listed in `data\BASE.OPTIONS.R`.  Some of these alter the calculation engine (such as how much smoothing to apply to power curves, or how to do the bias correction of wind speeds).  Some control what output you receive (do you want to save corrected wind speeds, monthly average farm output, output aggregated by turbine model, by country, etc.).  Hopefully there are enough comments in that file to help you along.

Finally, setup the model to run.  Change the path names to your specific options files in `2.CALCULATE.R`, and off you go.  You may find this is also quite slow to run (a few hours to simulate 100 farms over 30 years perhaps).  As it runs on multiple cores in parallel it isn't easy to provide a progress indicator in the main code (at least, I don't know how to).  So, you can optionally run `3.PROGRESS.MONITOR.R` in another R session, it will paint pretty pictures showing how much you have simulated...


## LICENSE
BSD 3-Clause License
Copyright (C) 2012-2017  Iain Staffell
All rights reserved.

This excludes all contents of the `power_curves` folder which remains the property of the respective copyright holders.

See `LICENSE` for more detail


## CREDITS & CONTACT

The VWF code is developed by Iain Staffell.  You can try emailing me at i.staffell@imperial.ac.uk

VWF is part of the [Renewables.ninja](https://renewables.ninja) project, developed by Stefan Pfenninger and Iain Staffell.  Use the [contacts page](https://www.renewables.ninja/about) there.

## Citation

I Staffell and S Pfenninger, 2016.  Using bias-corrected reanalysis to simulate current and future wind power output.  *Energy*, 114, 1224–1239. [doi: 10.1016/j.energy.2016.08.068](https://dx.doi.org/10.1016/j.energy.2016.08.068)
