### Run this script in an interactive session and call \code{devtools::use_data}
### on the resulting outputs.  It assumes you are at the package top level
### directory.

library('magrittr')

load.geojson <- function(file, property) {
    map <- rgdal::readOGR(file) %>% ggplot2::fortify(region=property)
}

gen.data <- function() {
    map.rgn14 <- load.geojson('inst/extdata/rgn14/GCAM_region.geojson', 'GCAM_regio')
    map.rgn32 <- load.geojson('inst/extdata/rgn32/GCAM_32_wo_Taiwan_clean.geojson', 'GCAM_ID')
    map.basin235 <- load.geojson('inst/extdata/rgnbasin/Global235_CLM_05_dissolve.geojson', 'GCAM_ID_1')
    map.chn <- load.geojson('inst/extdata/rgnchn/GCAM_China.geojson', 'GCAM_ID')
    ## TODO:  add GCAM-USA dataset here

    devtools::use_data(map.rgn14, map.rgn32, map.basin235, map.chn, overwrite=TRUE)
}

gen.internal <- function() {
    ## Read the various region lookup tables, drop region lists, and province lists
    ## 14-region has just a lookup
    lut.rgn14 <- read.csv('inst/extdata/rgn14/lookup.txt', strip.white=TRUE,
                          stringsAsFactors=FALSE)

    ## 32-region has a lookup and a drop
    lut.rgn32 <- read.csv('inst/extdata/rgn32/lookup.txt', strip.white=TRUE,
                          stringsAsFactors=FALSE)
    drop.rgn32 <- read.csv('inst/extdata/rgn32/drop-regions.txt', strip.white=TRUE,
                           stringsAsFactors=FALSE)

    ## basins have just a lookup table
    lut.basin235 <- read.csv('inst/extdata/rgnbasin/lookup.txt', strip.white=TRUE,
                             stringsAsFactors=FALSE)

    ## GCAM-china has all three
    lut.chn <- read.csv('inst/extdata/rgnchn/lookup.txt', strip.white=TRUE,
                        stringsAsFactors=FALSE)
    drop.chn <- read.csv('inst/extdata/rgnchn/drop-regions.txt', strip.white=TRUE,
                         stringsAsFactors=FALSE)
    prov.chn <- read.csv('inst/extdata/rgnchn/rgn-name-translation.csv', strip.white=TRUE,
                         stringsAsFactors=FALSE)

    ## TODO:  We need this data for GCAM-USA too

    devtools::use_data(lut.rgn14, lut.rgn32, drop.rgn32, lut.basin235,
                       lut.chn, drop.chn, prov.chn, internal=TRUE, overwrite=TRUE)
}


gen.data()
gen.internal()
