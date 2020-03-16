# output.r
#
# This file produces output from standard functions

library(RColorBrewer)
# library(wesanderson)
# library(inlmisc)

#' Create a map object and return/save the output
#'
#' This function is designed to take both a shape and raster object or path and create a standardized, congruent, output map.
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to shape file or an SF shape object
#' @param raster_data (Raster or Character) - Either the full path string to raster file or a raster object
#' @param raster_col (Character) - Column name that contains the raster object's output variable
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file
#' @param raster_band (Numeric) - Future variable for dealing with multi band/time series rasters etc
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param convert_zero (Boolean) - Convert raster 0 values to NA, default FALSE
#' @param dpi (Numeric) - Settable DPI for different print/screen formats
#' @param output_file (Character) - Output file path to save the resulting plot
#' @param output_file_type (Character) - Sets file extension type for saving plots. Choices restricted to ("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf)
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y)
#' @param map_xy_min_max (c(Numeric, Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax)
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Variable to hold the type of colorscale to be used
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of output map image in the form of (width, height) in inches
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis
#' @param map_y_label (Character) - Label for y axis
#' @return A ggplot object of the resulting map
#' @importFrom sf st_transform
#' @importFrom raster raster as.data.frame compareCRS minValue maxValue nlayers
#' @importFrom dplyr mutate
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_distiller ggplot geom_raster geom_sf coord_sf labs theme
#' @importFrom ggspatial layer_spatial df_spatial
#' @export
create_map <- function(shape_data = NULL, raster_data = NULL, raster_col = NULL, simplify = FALSE, raster_band = 1, bin_method = NULL,
                       bins = NULL, convert_zero = FALSE, dpi = 150, output_file = NULL, output_file_type = "png", expand_xy = c(0, 0),
                       map_xy_min_max = c(-180, 180, -90, 90), map_title = NULL,  map_palette = "RdYlBu",
                       map_width_height_in = c(15, 10), map_legend_title = NULL, map_x_label = "Lon", map_y_label = "Lat")
{
    error <- ""
    ouput <- "Default error"
    #browser()
    tryCatch(
    {

      # Create shape and raster objects via local processing functions
      shape_obj <- process_shape(shape_data, simplify)
      raster_obj <- process_raster(raster_data , raster_col, raster_band, bin_method, bins)

      # Perform shape and raster comparisons/other operations
      # Compare projections and equalize if necessary
      if(!compareCRS(shape_obj, raster_obj))
      {
          shape_obj <- st_transform(shape_obj, crs(raster_df))
      }

      # Crop shape - for extent changes (future
      # shape <- st_crop(shape, 1.2*extent(raster))

      # Convert raster
      raster_df <- df_spatial(raster_obj)
      #raster_df <- as.data.frame(raster_obj, xy=TRUE)

      # Raster operations
      if(is.null(raster_col))
      {
        error <- "No raster column defined"
      }
      else
      {
        raster_df <- mutate(raster_df, value = raster_df[[paste0("band", 1)]])
      }

      # Create mapping and output variables
      # Raster stats/information (future)
      raster_min <- minValue(raster_obj)
      raster_max <- maxValue(raster_obj)
      raster_layers <- nlayers(raster_obj)
      raster_active_band <- raster_band

      # Map output variables
      x_min <- map_xy_min_max[1]
      x_max <- map_xy_min_max[2]
      y_min <- map_xy_min_max[3]
      y_max <- map_xy_min_max[4]
      map_width <- map_width_height_in[1]
      map_height <- map_width_height_in[2]
      expand_x <- expand_xy[1]
      expand_y <- expand_xy[2]

      # Map colors/scales options
      palette_direction <- -1
      palette_type <- "div"
      na_value <- "Grey"
      map_guide <- "colourbar"
      map_x_scale <-  scale_x_continuous(limits=c(x_min, x_max), expand = expand_scale(add = expand_x), breaks=seq(x_min,x_max, abs(x_max - x_min)/12))
      map_y_scale <-  scale_y_continuous(limits=c(y_min, y_max), expand = expand_scale(add = expand_y), breaks=seq(y_min,y_max, abs(y_max - y_min)/6))
      map_color_scale <-  scale_fill_distiller(palette = map_palette, type = palette_type, direction = palette_direction, na.value = na_value, guide = map_guide)

      # Build ggplot Map object
      output <- ggplot() +  geom_raster(data=raster_df, aes(x=x, y=y, fill=value), alpha = 1.0) +
        geom_sf(data = shape_obj, na.rm = TRUE, fill=FALSE) +
        map_color_scale +
        coord_sf() +
        labs(x=map_x_label, y=map_y_label, title = map_title, fill = map_legend_title) +
        map_x_scale +
        map_y_scale +
        theme(plot.title = element_text(hjust = 0.5))

      # Save File
      if(!is.null(output_file))
      {
        gcammaptools::save_plot(output_file, output_file_type, dpi, map_width, map_height)
      }

    },
    error = function(err)
    {
        # error handler picks up error information
        error <- err
        return(error)
    })

    return(output)
}


#' Process shape
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to shape file or an SF shape object
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file
#' @export
process_shape <- function( shape_data, simplify)
{
  # Shape loading - if given a path, use that, else expect an object passed in
  if(is.null(shape_data))
  {
    error <- "Shape data is NULL"
  }
  else if(class(shape_data)[1] == "sf")
  {
    shape_obj <- shape_data
  }
  else if(class(shape_data) == "character")
  {
    shape_obj <- gcammaptools::import_mapdata(shape_data)
  }
  else
  {
    error <- "Unrecognized shape data argument."
  }

  # Optional argument to simplify polygons via the simplify_mapdata function
  if(simplify)
  {
    shape_obj <- gcammaptools::simplify_mapdata(shape_obj)
  }

  return(shape_obj)
}


#' Process raster
#'
#' @param raster_data (Raster or Character) - Either the full path string to raster file or a raster object
#' @param raster_col (Character) - Column name that contains the raster object's output variable
#' @param raster_band (Numeric) - Future variable for dealing with multi band/time series rasters etc
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param convert_zero (Boolean) - Convert raster zero values to NA
#' @importFrom rgis import_raster
#' @importFrom ggspatial layer_spatial df_spatial
#' @export
process_raster <- function( raster_data , raster_col, raster_band, bin_method, bins, convert_zero)
{
  # Raster loading - if given a path, use that, else expect an object passed in
  if(is.null(raster_data))
  {
    error <- "Raster data is NULL"
  }
  else if(class(raster_data)[1] == "RasterLayer")
  {
    raster_obj <- raster_data
  }
  else if(class(raster_data)[1] == "character")
  {
    raster_obj <- import_raster(raster_data)
  }
  else
  {
    error <- "Unrecognized raster data argument."
  }
  if(raster_band != 1)
  {
    raster_obj <- raster(raster_obj, band=raster_band)
  }
  if(convert_zero == TRUE)
  {
    raster_obj[raster_obj==0] <- NA
  }

  return(raster_obj)
}


#' Handles saving the map output to disk
#'
#' Initially one line of code, this function is likely to be expanded
#'
#' @param output_file (Character) - Output file path to save the resulting plot
#' @param output_file_type (Character) - Sets file extension type for saving plots. Choices restricted to ("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf)
#' @param dpi (Numeric) - Settable DPI for different print/screen formats
#' @param map_width (Numeric) - Map width in inches
#' @param map_height (Numeric) - Map height in inches
#' @importFrom ggplot2 ggsave
#' @export
save_plot <- function(output_file, output_file_type, dpi, map_width, map_height)
{
  ggsave(filename = paste0(output_file, ".", output_file_type), device = output_file_type, dpi = dpi, limitsize = TRUE,
         width = map_width, height = map_height)
}
# e equal interval, quantile, natural breaks to bin the data into a certain amount of classes as provided by the user
#test <- create_map(shape_data = "e:/repos/github/data/ri/State_Boundary_1997.shp", raster_data = "E:/Repos/github/data/ri/gaplf2011lc_v30_ri.tif", output_file = "./test", raster_col = "gaplf2011lc_v30_ri_COUNT")
#test <- create_map(shape_path = "e:/repos/github/data/USA_adm/USA_adm0.shp", raster_path = "E:/Repos/github/data/ri/gaplf2011lc_v30_ri.tif", output_file = "./test", raster_col = "gaplf2011lc_v30_ri_COUNT", data_classification = "Land")
#test <- create_map(shape_data = "e:/repos/github/data/tm_world_borders_simpl-0.3.shp", raster_data = "E:/Repos/github/data/wc2.0_10m_tavg_01.tif", output_file = "./test", raster_col = "wc2.0_10m_tavg_01")

