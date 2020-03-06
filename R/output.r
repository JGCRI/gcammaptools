# output.r
#
# The main file that produces output from standard functions

library(raster)
library(sf)
library(rgdal)
library(rgeos)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(inlmisc)

#' Create a map object and return/save the output
#'
#' This function is designed to take both a shape and raster object or path and create a standardized, congruent, output map.
#'
#' @param shape_path Input full path string to shape
#' @param shape_obj Pass shape object in directly instead of path
#' @param raster_path Input full path string to raster
#' @param raster_obj Pass raster object in directly instead of path
#' @param dpi Settable DPI for different print/screen formats
#' @param output_file Output file path to save the resulting plot
#' @param data_classification This will be used to determine the colors/units/legend items based on the type of data (ie. temperature, precip, etc)
#' @return A ggplot object of the resulting map
#' @export
create_map <- function(shape_path = NULL, shape_obj = NULL, raster_path = NULL, raster_obj = NULL, raster_col = NULL,
                       dpi = 150, output_file = NULL, data_classification = "Temp", simplify = FALSE)
{
   # shape_path = "data/tm_world_borders_simpl-0.3.shp"
   # raster_path = "data/wc2.0_10m_tavg_01.tif"
#browser()
    error <- ""
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
            shape <- gcammaptools::import_mapdata(shape_path)
        }
        else
        {
            error <- "both shape NOT null"
        }

        if(simplify)
        {
            shape <- gcammaptools::simplify_mapdata(shape)
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

        # Crop shape - for extent changes (future
        # shape <- st_crop(shape, 1.2*extent(raster))

        # Convert raster
        raster_df <- raster::as.data.frame(raster, xy = TRUE)

        # Raster operations
        if(!is.null(raster_col))
          raster_df <- dplyr::mutate(raster_df, value = raster_df[[raster_col]])
        else
          error <- "No raster column defined"

        # Raster stats/information (future)
        raster_min <- minValue(raster)
        raster_max <- maxValue(raster)
        raster_layers <- nlayers(raster)

        # Map output variables
        lat_min <- -90
        lat_max <- 90
        lon_min <- -180
        lon_max <- 180

        # Build colorscale and options
        # Determine nature of data and apply appropriate color scale
        # Using manual scales for now
        # browser()
        if(data_classification == "Temp")
        {
          map_palette <- "RdYlBu"
          palette_direction <- -1
          palette_type <- "div"
          na_value <- "Grey"
          map_guide <- "colourbar"
          map_title <- "World Average Temperature"
          legend_title <- "Temperature \u00B0C"
          map_y_scale <-  ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))
          map_x_scale <-  ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))
          map_color_scale <-  ggplot2::scale_fill_distiller(palette = map_palette, type = palette_type, direction = palette_direction, na.value = na_value, guide = map_guide )
        }
        if(data_classification == "Precip")
        {
          map_palette <- "Purples"
          palette_direction <- -1
          palette_type <- "seq"
          na_value <- "Grey"
          map_guide <- "colourbar"
          map_title <- "World Precipitation"
          legend_title <- "Temperature \u00B0C"
          map_y_scale <-  ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))
          map_x_scale <-  ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))
          map_color_scale <-  ggplot2::scale_fill_distiller(palette = map_palette, type = palette_type, direction = palette_direction, na.value = na_value, guide = map_guide )
        }
        if(data_classification == "Land")
        {
          map_palette <- "Set1"
          palette_direction <- 1
          palette_type <- "qual"
          na_value <- "Grey"
          map_guide <- "legend"
          map_title <- "Land Usage"
          legend_title <- "Land Use Types"
          shape <- st_crop(shape, 1.2*extent(raster))
          map_x_scale <-  NULL
          map_y_scale <- NULL
          map_color_scale <- NULL
        }


        # Build Map object
        output <- ggplot() +  geom_raster(data=raster_df, aes(x=x, y=y, fill=value), alpha = 1.0) +
          ggplot2::geom_sf(data = shape, na.rm = TRUE, fill=FALSE) +
          map_color_scale +
          ggplot2::coord_sf() +
          ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = map_title, fill = legend_title) +
          map_x_scale +
          map_y_scale +
          theme(plot.title = element_text(hjust = 0.5))

         # scale_color_manual(values = wes.palette(n=3, name="GrandBudapest"))

        # Save File
        if(!is.null(output_file))
          ggplot2::ggsave(filename = paste0(output_file, "-no.png"), device = "png", dpi = dpi, limitsize = TRUE,
                        width = 15, height = 10)
        # ggplot2::ggsave(filename =paste0(output_file, ".bmp"), device = "bmp", dpi = dpi, limitsize = TRUE,
        #                 width = 15, height = 10)

    },
    error = function(err)
    {
        # error handler picks up where error was generated
        error <- err
        return(error)
    })

    return(output)
}

#test <- create_map(shape_path = "e:/repos/github/data/ri/State_Boundary_1997.shp", raster_path = "E:/Repos/github/data/ri/gaplf2011lc_v30_ri.tif", output_file = "./test", raster_col = "gaplf2011lc_v30_ri_COUNT", data_classification = "Land")
#test <- create_map(shape_path = "e:/repos/github/data/USA_adm/USA_adm0.shp", raster_path = "E:/Repos/github/data/ri/gaplf2011lc_v30_ri.tif", output_file = "./test", raster_col = "gaplf2011lc_v30_ri_COUNT", data_classification = "Land")
#test <- create_map(shape_path = "e:/repos/github/data/tm_world_borders_simpl-0.3.shp", raster_path = "E:/Repos/github/data/wc2.0_10m_tavg_01.tif", output_file = "./test", raster_col = "wc2.0_10m_tavg_01", data_classification = "Temp")
ggplotColors <- function(g){
  d <- 360/g
  h <- cumsum(c(15, rep(d,g - 1)))
  hcl(h = h, c = 100, l = 65)
}
