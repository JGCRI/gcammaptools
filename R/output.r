# output.r
#
# The main file that produces output from standard functions

library(raster)
library(sf)
library(rgdal)
library(rgeos)
library(ggplot2)
library(RColorBrewer)

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
    #browser()
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
#browser()
        # Crop shape?
        # shape <- st_crop(shape, 1.2*extent(raster))

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
        # Determine nature of data and apply appropriate color scale
        # Using manual scales for now
        map_palette <- "Purples"
        palette_direction <- -1
        palette_type <- "seq"


        # Build Map object

        output <- ggplot() + ggplot2::geom_sf(data = shape) +
          geom_raster(data=raster_df, aes(x=x, y=y, fill=value), alpha = 0.9) +
          ggplot2::scale_fill_distiller(palette = map_palette, type = palette_type, direction = palette_direction, na.value = "Gray" ) +
          ggplot2::coord_fixed(ratio = 1) +
          ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = "World Average Temperature", fill = value) +
          ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))+
          ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))

        ggplot2::ggsave(filename = output_file, device = "png", dpi = dpi, limitsize = TRUE)
        # Save File

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

#
# ggplotMap <- ggplot2::ggplot() +
#   mapWorld +
#   ggplot2::geom_raster(data = combined_data, ggplot2::aes_string(x="Lon", y = "Lat", fill=mapVar),interpolate = TRUE ) +
#   # ggplot2::geom_point(data = combined_data, ggplot2::aes(x = Lon, y = Lat, color = Neg, alpha = 0.5)) +
#   ggplot2::coord_fixed(ratio = 1) +
#   ggplot2::scale_fill_distiller(palette = mapPalette,type = "div", direction = mapDirection, na.value = "Gray" ) +
#   #viridis::scale_fill_viridis(direction = 1, option = "E" ) +
#   ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = paste0(input$mapCore, " - ", input$mapYear), fill = mapFill) +
#   ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))+
#   ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))
