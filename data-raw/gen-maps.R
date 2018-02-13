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

    ## Both maps of the GCAM regions are built with the column 'region_name',
    # which is not necessary for plotting but is used so that gcam32_colors can
    # map to it.
    map.rgn14 <- system.file("extdata", "rgn14/GCAM_region.geojson", package = "gcammaptools") %>%
        import_mapdata() %>%
        dplyr::left_join(lut.rgn14, by = "region_id") %>%
        dplyr::rename(region_name = region) %>% sf::st_as_sf()
    map.rgn32 <- system.file("extdata", "rgn32/reg32_spart.shp", package = "gcammaptools") %>%
        import_mapdata() %>%
        dplyr::left_join(lut.rgn32, by = "region_id") %>%
        dplyr::rename(region_name = region) %>% sf::st_as_sf()
    map.rgn14.simple <- simplify_mapdata(map.rgn14)
    map.rgn32.simple <- simplify_mapdata(map.rgn32)

    ## The basin235 map is also provided in detailed and simplified forms.
    path.basin235 <- "rgnbasin/Global235_CLM_05_dissolve.geojson"
    map.basin235 <- system.file("extdata", path.basin235, package = "gcammaptools") %>%
        import_mapdata()
    map.basin235.simple <- simplify_mapdata(map.basin235)

    ## The original geoJSON for China is 9.7MB, so it is not included. The
    # smaller version was produced by simplifying both the Chinese provinces and
    # the GCAM regions, but keeping more detail in the provinces:
    #
    # orig <- import_mapdata('path/to/original/file/GCAM_China.geojson')
    # keeps <- c("China", "Central Asia", "Taiwan")
    # simpler <- rbind(simplify_mapdata(orig[!orig$Region %in% keeps , ], 2.5, 0.1),
    #                  simplify_mapdata(orig[orig$Region %in% keeps, ], 0, 0.01))
    # sf::st_write(simpler, "GCAM_China.geojson", layer = "simpler",
    #              driver = "GeoJSON")
    #
    # The geojson contains excess information, and only the region names and ids
    # are kept for the final map.
    map.chn <- system.file("extdata", "rgnchn/GCAM_China.geojson", package = "gcammaptools") %>%
        import_mapdata() %>%
        dplyr::select(region_name, region_id, geometry)

    ## GCAM USA uses the two letter state abbreviations as the state IDs, so the
    # usa lookup table is actually of more use here in the building of the map.
    # The USA map is placed on top of the 32-region map to produce a final GCAM
    # USA compatable map.
    #
    # One problem with the defualt map data is that the US-Canada border lacks
    # resolution. Because of this, many reprojections caused a gap between the
    # the two countries. The problem is addressed below by segmentizing the
    # polygon that represents mainland Canada such that it contains no segment
    # longer than the resolution specified by 'min.seg'. More information about
    # segmentization can be found here:
    # http://www.georeference.org/doc/segmentization.htm
    path.usa <- system.file("extdata", "rgnusa/us_states_50m.shp", package = "gcammaptools")
    map.usa <- import_mapdata(path.usa)[ , c('name', 'geometry')] %>%
        dplyr::mutate(name = levels(droplevels(name))) %>%
        dplyr::left_join(lut.usa, by = c("name" = "region")) %>%
        dplyr::rename(region_name = name) %>%
        rbind(dplyr::filter(map.rgn32, region_id != 1)) %>%
        sf::st_as_sf()
    canada.main <- 273 # Index in map for polygon of Canada's mainland
    min.seg <- 1       # Minimum segment length in degrees allowed for polygon
    sf::st_geometry(map.usa)[[canada.main]] <- sf::st_segmentize(sf::st_geometry(map.usa)[[canada.main]],min.seg)

    ## The world map of countries is included for completeness' sake, and cannot
    # be used with GCAM data as it currently is.
    path.countries <- system.file("extdata", "rgnworld/ne_110m_admin_0_countries.shp", package = "gcammaptools")
    map.countries <- import_mapdata(path.countries)[,c('admin', 'geometry')]

    devtools::use_data(map.rgn14, map.rgn14.simple, map.rgn32, map.rgn32.simple,
                       map.basin235, map.basin235.simple, map.chn, map.usa,
                       map.countries, overwrite=TRUE)
}

gen.internal <- function() {
    ## Read the various region lookup tables, drop region lists, and province
    # lists 14-region has just a lookup
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
