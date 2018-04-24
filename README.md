[![Build Status](https://travis-ci.org/JGCRI/gcammaptools.svg?branch=master)](https://travis-ci.org/JGCRI/gcammaptools)

# gcammaptools

## Overview

The gcammaptools package provides functions for plotting
[GCAM](http://jgcri.github.io/gcam-doc/) data on world or regional maps.  This
includes functions for making plots for regional or gridded data, as well as
default projection and theme settings that provide a house style for GCAM plots.

## Installation

This package must be installed from the github repository using
`install_github`.  To do this, you will need to install `devtools` first if you
don't have it already.  

```R
install.packages('devtools') # if you don't have it already
devtools::install_github('JGCRI/gcammaptools')
```  
Optionally, you can build the "examples" vignette, which shows how to
do several common mapping tasks.  To build these examples, replace the
`install_github` command above with:
```R
devtools::install_github('JGCRI/gcammaptools', dependencies=TRUE,
                         build_vignettes=TRUE)
```
You can display the vignette by running
```R
vignette('examples','gcammaptools')
```

Sometimes R can't find the CRAN repository without help.  If during
the `install_github` steps you get errors about missing packages or
functions, rerun them with the `repos` option:  

```R
cran <- 'http://cran.us.r-project.org'
devtools::install_github('JGCRI/gcammaptools', build_vignettes=TRUE,
                         dependencies=TRUE, repos=cran)
```  
This should allow R to fetch the packages it needs to complete the
installation from CRAN.

## Usage

### Preparing GCAM data

The recommended way to load your GCAM data is by using the 
[rgcam](https://github.com/JGCRI/rgcam) package create a project data file from
a GCAM database, and then querying that file for the data you want to plot. 
```R
prj <- rgcam::loadProject('myproj.dat')
co2 <- rgcam::getQuery(prj, 'CO2 emissions by region', 'Reference')
```
Alternatively, you can start with any data frame that has a `region` column and
one or more data columns.

Once you have loaded the data, you must add the region identifiers used in the
map data to the data frame using the `add_region_ID` function:
```R
co2 <- add_region_ID(co2)
```

### Plotting maps

The main plotting function is `plot_GCAM`.  You can supply your own map of
region boundaries, but most of the time you will want to use the ones supplied
with the package.
`plot_GCAM`:  
```R
plot_GCAM(map.rgn32, col='value', gcam_df=co2, title='CO2 Emissions', legend=TRUE)
```
