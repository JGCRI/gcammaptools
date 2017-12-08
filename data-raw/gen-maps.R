### Run this script in an interactive session and call \code{devtools::use_data}
### on the resulting outputs.  It assumes you are at the package top level
### directory.

library('gcammaptools')
library('magrittr')
library('dplyr')

gen.data <- function() {

    ## TODO:  add GCAM-USA dataset here

    path.rgn14 <- system.file("extdata", "rgn14/GCAM_region.geojson", package = "gcammaptools")
    map.rgn14 <- import_mapdata(path.rgn14)
    # add a column with the region names so that gcam32_colors can map to it
    map.rgn14['region_name'] <- dplyr::left_join(map.rgn14, lut.rgn14, by="region_id")[3]
    map.rgn14.simple <- simplify_mapdata(map.rgn14)

    path.rgn32 <- system.file("extdata", "rgn32/reg32_spart.shp", package = "gcammaptools")
    map.rgn32 <- import_mapdata(path.rgn32)
    map.rgn32['region_name'] <- dplyr::left_join(map.rgn32, lut.rgn32, by="region_id")[3]
    map.rgn32.simple <- simplify_mapdata(map.rgn32)

    path.basin235 <- system.file("extdata", "rgnbasin/Global235_CLM_05_dissolve.geojson", package = "gcammaptools")
    map.basin235 <- import_mapdata(path.basin235)
    map.basin235.simple <- simplify_mapdata(map.basin235)

    path.chn <- system.file("extdata", "rgnchn/GCAM_China.geojson", package = "gcammaptools")
    map.chn <- import_mapdata(path.chn) %>% dplyr::select(region_name, region_id, geometry)
    map.chn.simple <- simplify_mapdata(map.chn, min_area = 0.1, degree_tolerance = 0.05)

    devtools::use_data(map.rgn14, map.rgn14.simple, map.rgn32, map.rgn32.simple,
                       map.basin235, map.basin235.simple, map.chn, map.chn.simple, overwrite=TRUE)
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


gen.internal()
gen.data()
