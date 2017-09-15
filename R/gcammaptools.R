#' gcammaptools:  A package for plotting GCAM data on maps
#'
#' The gcammaptools package provides functions for plotting GCAM data on world
#' or regional maps.  This includes functions for parsing the output of the GCAM
#' ModelInterface, as well as functions for making the plots and default
#' projection and theme settings that provide a house style for GCAM plots.
#'
#' @section Reading GCAM data:
#'
#' The \code{\link{parse_mi_output}} function reads the output of a GCAM
#' ModelInterface batch query and turns it into a list of tables (i.e., data
#' frames).  Calling this function will typically be the first step in an
#' analysis.  You will only have to call it once for each ModelInterface batch
#' query you ran.
#'
#' The second step in the data workflow is \code{\link{process_batch_q}}.  This
#' function takes the output of \code{parse_mi_output} and extracts the results
#' of a single GCAM query as a table.  You can optionally do some simple
#' filtering and aggregation in this function as well
#'
#' Once you have a table of results, you must add a region id column using
#' \code{\link{add_region_ID}}.  This function allows you to provide an alternate
#' GCAM region mapping, along with optional subregional (i.e., state or
#' province) mappings and a table of regions to drop.
#'
#' @section Mapping GCAM data:
#'
#' To map GCAM data, you will need a spatial data frame with your region
#' boundaries.  The package provides the following commonly-used base maps:
#' \itemize{
#'   \item \code{\link{map.rgn14}}: 14 region map used prior to GCAM 4.0.
#'   \item \code{\link{map.rgn32}}: 32 region map used in GCAM 4.0 and later,
#' excluding Taiwan.
#'   \item \code{\link{map.basin235}}: 235 water basins.
#'   \item \code{\link{map.chn}}: 32 global regions plus China subregions
#'   \item \code{map.usa}: 32 global regions plus US states (XXX not yet
#' provided; coming as soon as I can dig up the shapefile).
#' }
#' Users can provid their own base maps.  You will need a geometry file in
#' geojson format, where each region has a \code{GCAM_ID} property.  Land masses
#' that you want to draw, but which aren't part of any region should be included
#' and assigned a \code{GCAM_ID} of 0.  The \code{examples} vignette shows how
#' to load such a file and convert it for use in this package.
#'
#' Once you have your map data you will need to join it to your GCAM data.  You
#' can do this by merging the two data frames on the \code{id} column.  The
#' resulting structure can be passed to the \code{\link{plot_GCAM}} function to
#' generate the maps.
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
#' @format Spatial data frame
"map.rgn14"

#' Base map for 32-region GCAM
#'
#' This is the region map used in GCAM 4.0 and subsequent.  This version of the
#' map does not include the Taiwan region.
#'
#' @format Spatial data frame
"map.rgn32"

#' Base map for 235 global water basins
#'
#' This is the map of the 235 global water basins.  For compatibility with the
#' other map data frames it refers to the basins as 'regions'.  Thus, any GCAM
#' data with a 'basin' column will need to have a 'region' column added.  There
#' is also some variability in how the basin names are represented, so this data
#' set will need some work.
#'
#' @format Spatial data frame
"map.basin235"

#' Base map for 32-region GCAM with China subregions
#'
#' This map has the 32 GCAM regions, plus the subregions corresponding to
#' China's provinces, municipalities, autonomous regions, and SARs.
#'
#' @format Spatial data frame
"map.chn"
