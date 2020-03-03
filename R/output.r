# output.r
#
# The main file that produces output from standard functions

library(raster)
library(sf)
library(rgdal)
library(rgeos)
library(ggplot2)

#' Create a map object and return/save the output
#'
#' This function is designed to take both a shape and raster object or path and create a standardized, congruent, output map.
#'
#' @param shape_path Input full path string to shape
#' @param shape_obj Pass shape object in directly instead of path
#' @param raster_path Input full path string to raster
#' @param  raster_obj Pass raster object in directly instead of path
#' @param  dpi Settable DPI for different print/screen formats
#' @param  output_file Output file path to save the resulting plot
#' @param  simplify
#' @return A ggplot object
#' @export
create_map <- function(shape_path = NULL, shape_obj = NULL, raster_path = NULL, raster_obj = NULL, raster_col = NULL,
                       dpi = 150, output_file = NULL, simplify = FALSE)
{
   # shape_path = "data/tm_world_borders_simpl-0.3.shp"
   # raster_path = "data/wc2.0_10m_tavg_01.tif"

    error <- "test"
    browser()
    tryCatch(
    {
        # Shape loading - if given a path, use that, else expect an object passed in
        if(is.null(shape_path))
        {
            if(!is.null(shape_obj))
                shape <- shape_obj
            else
                error <- "both shape null"
        }
        else if(is.null(shape_obj))
        {
            shape <- rgis::import_shapefile(shape_path, quiet = TRUE)
        }
        else
        {
            error <- "both shape NOT null"
        }


        # Raster loading - if given a path, use that, else expect an object passed in
        if(is.null(raster_path))
        {
            if(!is.null(raster_obj))
                raster <- raster_obj
            else
                error <- "both raster null"
        }
        else if(is.null(raster_obj))
        {
            raster <- rgis::import_raster(raster_path)
        }
        else
        {
            error <- "both raster NOT null"
        }

        # Compare projections and equalize if necessary
        if(raster::compareCRS(shape, raster))
            error <- "proj ok"
        else
        {
           # raster <- raster::projectRaster(raster, crs = crs(shape))
            shape <- st_transform(shape, crs(raster))
            compare_result <- raster::compareCRS(shape, raster)
            error <- compare_result
        }

        # Convert raster
        raster_df <- as.data.frame(raster, xy = TRUE)

        # Raster operations
        if(!is.null(raster_col))
          raster_df <- dplyr::mutate(raster_df, value = raster_df[[raster_col]])
        else
          error <- "No raster column defined"

        # Raster stats/information
        raster_min <- minValue(raster)
        raster_max <- maxValue(raster)
        raster_layers <- nlayers(raster)

        # Build colorscale
        #
        #

        # Build Map object
        output <- ggplot() + geom_raster(data=raster_df, aes(x=x, y=y, fill=value), alpha = 0.85) +
          ggplot2::geom_sf(data = shape) +
          coord_sf()

        # Save File

    },
    warning = function(war)
    {
        # warning handler picks up where error was generated
        error <- war
    },
    error = function(err)
    {
        # error handler picks up where error was generated
        error <- err
    })

  #  plot(raster)
  #  plot(shape)

    return(output)
}
