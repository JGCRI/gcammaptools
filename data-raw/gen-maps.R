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

    devtools::use_data(map.rgn14, map.rgn32, map.basin235, map.chn)
}

gen.data()
