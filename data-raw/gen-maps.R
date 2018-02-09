### Run this script in an interactive session and call \code{devtools::use_data}
### on the resulting outputs.  It assumes you are at the package top level
### directory.

library('gcammaptools')
library('magrittr')
library('dplyr')

# This function generates six map files, represented as sf objects (see
# https://cran.r-project.org/web/packages/sf/). The default maps supported by
# gcammaptools are:
#   - rgn14:     The outdated GCAM 14-region boundaries
#   - rgn32:     The current 32 GCAM regions
#   - basin235:  The 235 water basins, which are soon replacing AEZs
#   - chn:       The current 32 GCAM regions with China broken down into 30
#                provinces
#   - usa:       The current 32 GCAM regions with USA broken down into 51
#                regions (50 states and Washington DC)
#   - countries: A standard world map containing country administrative borders
# Additionally, simplified versions of rgn14, rgn32, and basin235 are created.
gen.data <- function() {

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

    # The original geoJSON for China is 9.7MB, so it is not included. The
    # smaller version was produced by simplifying both the Chinese provinces and
    # the GCAM regions, but keeping more detail in the provinces:
    #
    # orig <- import_mapdata('path/to/original/file/GCAM_China.geojson')
    # keeps <- c("China", "Central Asia")
    # simpler <- rbind(simplify_mapdata(orig[!orig$Region %in% keeps , ], 2.5, 0.1),
    #                  simplify_mapdata(orig[orig$Region %in% keeps, ], 0, 0.01))
    # ...
    #
    path.chn <- system.file("extdata", "rgnchn/GCAM_China.geojson", package = "gcammaptools")
    map.chn <- import_mapdata(path.chn) %>% dplyr::select(region_name, region_id, geometry)

    path.usa <- system.file("extdata", "rgnusa/us_states_50m.shp", package = "gcammaptools")
    map.usa <- import_mapdata(path.usa)[,c(1,3)]
    map.usa$name <- levels(droplevels(map.usa$name))
    map.usa['region_id'] <- dplyr::left_join(map.usa, lut.usa, by=c("name" = "region"))[3]
    names(map.usa)[1] <- c("region_name")
    map.usa <- rbind(map.rgn32, map.usa)

    path.countries <- system.file("extdata", "rgnworld/ne_110m_admin_0_countries.shp", package = "gcammaptools")
    map.countries <- import_mapdata(path.countries)[,c('admin', 'geometry')]

    devtools::use_data(map.rgn14, map.rgn14.simple, map.rgn32, map.rgn32.simple,
                       map.basin235, map.basin235.simple, map.chn, map.countries,
                       overwrite=TRUE)
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
                           stringsAsFactors=FALSE, header=F)

    ## basins have just a lookup table
    lut.basin235 <- read.csv('inst/extdata/rgnbasin/lookup.txt', strip.white=TRUE,
                             stringsAsFactors=FALSE)

    ## GCAM-china has all three
    lut.chn <- read.csv('inst/extdata/rgnchn/lookup.txt', strip.white=TRUE,
                        stringsAsFactors=FALSE)
    drop.chn <- read.csv('inst/extdata/rgnchn/drop-regions.txt', strip.white=TRUE,
                         stringsAsFactors=FALSE, header=F)
    prov.chn <- read.csv('inst/extdata/rgnchn/rgn-name-translation.csv', strip.white=TRUE,
                         stringsAsFactors=FALSE)

    ## GCAM-USA has a lookup table
    lut.usa <- read.csv('inst/extdata/rgnusa/lookup.txt', strip.white=TRUE,
                        stringsAsFactors=FALSE)

    devtools::use_data(lut.rgn14, lut.rgn32, drop.rgn32, lut.basin235,
                       lut.chn, drop.chn, prov.chn, lut.usa,
                       internal=TRUE, overwrite=TRUE)
}


gen.internal()
gen.data()
