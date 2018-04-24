#' gcammaptools:  A package for plotting GCAM data on maps
#'
#' The gcammaptools package provides functions for plotting GCAM data on world
#' or regional maps.  This includes functions for making plots for regional or
#' gridded data, as well as default projection and theme settings that provide a
#' house style for GCAM plots.
#'
#' @section Preparing GCAM data:
#'
#' The recommended way to load your GCAM data is by using the \code{rgcam}
#' package create a project data file from a GCAM database, and then querying
#' that file for the data you want to plot. Alternatively, you can start with
#' any data frame that has a `region` column and one or more data columns.
#'
#' Once you have loaded the data, you must add the region identifiers used in
#' the map data to the data frame using the \code{add_region_ID} function.
#'
#' @section Mapping GCAM data:
#'
#' To map GCAM data, you will need a simple feature collection or geojson with
#' your region boundaries.  The package provides the following commonly-used
#' base maps:
#' \itemize{
#'   \item \code{\link{map.rgn14}}: 14 region map used prior to GCAM 4.0.
#'   \item \code{\link{map.rgn32}}: 32 region map used in GCAM 4.0 and later,
#' excluding Taiwan.
#'   \item \code{\link{map.basin235}}: 235 water basins.
#'   \item \code{\link{map.chn}}: 32 global regions plus China subregions
#'   \item \code{\link{map.usa}}: 32 global regions plus US states.
#' }
#' Users can provide their own base maps.  You will need a geometry file in
#' geojson format, where each region has a \code{region_id}.  Land masses
#' that you want to draw, but which aren't part of any region should be included
#' and assigned a \code{region_id} of 0.  The \code{examples} vignette shows how
#' to load such a file and convert it for use in this package.
#'
#' Once you have your map data and your GCAM data, can generate the maps by
#' passing both to the \code{\link{plot_GCAM}} function.
#'
#' @docType package
#' @name gcammaptools
#' @import ggplot2
#' @importFrom magrittr %>%
NULL

#' Base map for 14-region GCAM
#'
#' This is the region map used in versions of GCAM prior to GCAM 4.0.  It is
#' largely obsolete, but there are still some data from those days floating
#' around in the wild
#'
#' @format Simple feature collection
"map.rgn14"

#' Simplified base map for 14-region GCAM
#'
#' The same map as \code{map.rgn14} but with only Polygons that have an area
#' greater than 2.5 square degrees and simplified Polygon borders.
#'
#' @format Simple feature collection
"map.rgn14.simple"

#' Base map for 32-region GCAM
#'
#' This is the region map used in GCAM 4.0 and subsequent.  This version of the
#' map does not include the Taiwan region.
#'
#' @format Simple feature collection
"map.rgn32"

#' Simplified base map for 32-region GCAM
#'
#' The same map as \code{map.rgn32} but with only Polygons that have an area
#' greater than 2.5 square degrees and simplified Polygon borders.
#'
#' @format Simple feature collection
"map.rgn32.simple"

#' Base map for 235 global water basins
#'
#' This is the map of the 235 global water basins.  For compatibility with the
#' other map data frames it refers to the basins as 'regions'.  Thus, any GCAM
#' data with a 'basin' column will need to have a 'region' column added.  There
#' is also some variability in how the basin names are represented, so this data
#' set will need some work.
#'
#' @format Simple feature collection
"map.basin235"

#' Simplified base map for 235 global water basins
#'
#' The same map as \code{map.basin235} but with only Polygons that have an area
#' greater than 2.5 square degrees and simplified Polygon borders.
#'
#' @format Simple feature collection
"map.basin235.simple"

#' Base map for 32-region GCAM with China subregions
#'
#' This map has the 32 GCAM regions, plus the subregions corresponding to
#' China's provinces, municipalities, autonomous regions, and SARs.
#'
#' @format Simple feature collection
"map.chn"

#' Base map for 32-region GCAM with USA states
#'
#' This map has the 32 GCAM regions, plus the subregions corresponding to
#' US states including the District of Columbia.
#'
#' @format Simple feature collection
"map.usa"

#' Base map for gridded data over national borders
#'
#' This map has the administrative borders of the world's countries. It is meant
#' for use with gridded data only, as no GCAM output matches country boundaries.
#'
#' @format Simple feature collection
"map.countries"
