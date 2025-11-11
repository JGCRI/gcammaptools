library(tidyverse)
library(sf)
library(ggplot2)
library(necountries)
sf::sf_use_s2(FALSE)

# Note: This script allows users to create shape files. After those files are created,
# you will need to run `gen-maps.R` in order to create the mappings needed in `gcammaptools`.

# *********
# Option 1: Create a shape file that combines countries into regions

# Country-level shape files can be downloaded from: https://www.naturalearthdata.com/
country.shp <- st_read("path-to-country-level-shapefile/ne_110m_admin_0_countries.shp")

# GCAM region mapping
read_csv("path-to-GCAM-directory/input/gcamdata/inst/extdata/common/iso_GCAM_regID.csv", comment="#") %>%
    mutate(iso = toupper(iso)) ->
    iso_GCAM_regID

# Add region identifier and combine
country.shp %>%
    mutate(ISOforMatching = if_else(ISO_A3 == -99, ADM0_A3, ISO_A3)) %>%
    left_join(iso_GCAM_regID, by=c("ISOforMatching" = "iso")) %>%
    # Some ISOs are missing from the GCAM region mapping, which leads to gaps in plotting data
    mutate(GCAM_region_ID = if_else(ISOforMatching == "SOL", 2, GCAM_region_ID),
           GCAM_region_ID = if_else(ISOforMatching == "KOS", 15, GCAM_region_ID)) %>%
    group_by(GCAM_region_ID) %>%
    summarize(geometry = sf::st_union(geometry)) %>%
    rename(region_id = GCAM_region_ID) %>%
    select(region_id, geometry) ->
    rgn.shp

# Save shape file
st_write(rgn.shp, "./inst/extdata/rgn32v81/rgn32v81_spart.shp")

# *********
# Option 2: Create a shape file from the intersection of two other shape files (e.g., regions & basins)

# Read basin shape file
basins.shp <- st_read("./inst/extdata/rgnbasin/Global235_CLM_05_dissolve.geojson")

# Read region shape file
rgn.shp <- st_read("./inst/extdata/rgn32v81/rgn32v81_spart.shp")

# Combine shape files
st_intersection(rgn.shp, basins.shp) %>%
    rename(gcamreg_id = region_id,
           basin_id = region_id.1) %>%
    na.omit() ->
    lndrgn.shp

# Save shape file
st_write(lndrgn.shp, "./inst/extdata/rgnlndv81/rgnlndv81_spart.shp")
