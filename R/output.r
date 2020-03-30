# output.r
#
# This file produces output from standard functions for various maps

#' Create a basic map object and return/save the output
#'
#' This function is designed to take both a shape and raster object/path and create a standardized output map.
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param raster_data (Raster or Character) - Either the full path string to a raster file or an object of type RasterLayer
#' @param raster_col (Character) - Column name that contains the raster object's output variable/value
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param raster_band (Numeric) - Future variable for dealing with multi band/time series rasters etc
#' @param convert_zero (Boolean) - Convert values within the raster data from zero to NA (default FALSE)
#' @param dpi (Numeric) - Settable DPI for different print/screen formats (default 150)
#' @param output_file (Character) - Output file path and file name/type to save the resulting plot (e.g. "c:/temp/output.png") (Types accepted: "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg")
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y)
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: -180, 180, -90, 90)
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Variable to hold the type of colorscale to be from the RColorBrewer palette (default "RdYlBu")
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction TRUE/FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (defalt 15, 10)
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default Lon)
#' @param map_y_label (Character) - Label for y axis (default Lat)
#' @return (ggplot2 or Character) - Returns a ggplot object of the resulting map or an error string if failed
#' @importFrom sf st_transform
#' @importFrom raster raster as.data.frame compareCRS minValue maxValue nlayers
#' @importFrom dplyr mutate
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_distiller ggplot geom_raster geom_sf coord_sf labs theme geom_sf_label
#' @importFrom ggspatial layer_spatial df_spatial
#' @import RColorBrewer
#' @export
create_map <- function(shape_data = NULL, shape_label_field = NULL, shape_label_size_field = "1", simplify = FALSE,
                       raster_data = NULL, raster_col = NULL,  raster_band = 1,  convert_zero = FALSE,
                       dpi = 150, output_file = NULL, expand_xy = c(0, 0),
                       map_xy_min_max = c(-180, 180, -90, 90), map_title = NULL,  map_palette = "RdYlBu", map_palette_reverse = FALSE,
                       map_width_height_in = c(15, 10), map_legend_title = NULL, map_x_label = "Lon", map_y_label = "Lat")
{
    error <- ""
    output <- "Default error"
    tryCatch(
    {
      # Create shape and raster objects via local processing functions
      shape_obj <- process_shape(shape_data, simplify, shape_label_field)

      if(!class(shape_obj) %in% "sf")
      {
        output <- shape_obj
        return(output)
      }

      # Create raster
      raster_obj <- process_raster(raster_data , raster_col, raster_band, bin_method, bins, convert_zero)

      if(!class(raster_obj) %in% "RasterLayer")
      {
        output <- raster_obj
        return(output)
      }

      # Perform shape and raster comparisons/other operations
      # Compare projections and equalize if necessary
      if(!compareCRS(shape_obj, raster_obj))
      {
          shape_obj <- st_transform(shape_obj, crs(raster_df))
      }

      # Crop shape - for extent changes (future
      # shape <- st_crop(shape, 1.2*extent(raster))

      # Raster operations
      # Convert raster
      raster_df <- df_spatial(raster_obj)  #raster_df <- as.data.frame(raster_obj, xy=TRUE) compare performance

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
      palette_direction <- 1

      if(map_palette_reverse)
      {
        palette_direction <- -1
      }

      p_x_scale <-  scale_x_continuous(limits=c(x_min, x_max), expand = expand_scale(add = expand_x), breaks=seq(x_min,x_max, abs(x_max - x_min)/12))
      map_y_scale <-  scale_y_continuous(limits=c(y_min, y_max), expand = expand_scale(add = expand_y), breaks=seq(y_min,y_max, abs(y_max - y_min)/6))
      map_color_scale <-  scale_fill_distiller(palette = map_palette, type = palette_type, direction = palette_direction, na.value = na_value, guide = map_guide)
      map_shape_options <- NULL
      map_size_guide_option <- NULL

      if(!is.null(shape_label_field))
      {
        map_shape_options <- geom_sf_label(data = shape_obj, aes_string(label = shape_label_field, fill=NULL, size = shape_label_size_field))
        if(!is.null(shape_label_size_field))
        {
          map_size_guide_option <- guides(size = FALSE)
        }
      }

      # Build ggplot Map object
      output <- ggplot() +  geom_raster(data=raster_df, aes(x=x, y=y, fill=value), alpha = 1.0) +
        geom_sf(data = shape_obj, na.rm = TRUE, fill=NA) +
        map_color_scale +
        coord_sf() +
        labs(x=map_x_label, y=map_y_label, title = map_title, fill = map_legend_title) +
        map_x_scale +
        map_y_scale +
        map_shape_options +
        theme(plot.title = element_text(hjust = 0.5)) +
        map_size_guide_option

      # Save File
      if(!is.null(output_file))
      {
        result <- gcammaptools::save_plot(output_file, dpi, map_width, map_height)
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


#' Create a choropleth map object and return/save the output
#'
#' Choropleth map
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param map_data (Data Frame or Character) - A data frame that contains the output data to map, or alternatively a full path to a CSV
#' @param data_key_field (Character) - Name of key field in data_obj for merging with shape_data
#' @param data_col (Character) - Column name that contains the data object's output variable
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (Quantile, xxx, yyy)
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param dpi (Numeric) - Settable DPI for different print/screen formats (default 150)
#' @param output_file (Character) - Output file path and file name/type to save the resulting plot (e.g. "c:/temp/output.png") (Types accepted: "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg")
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y)
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: -180, 180, -90, 90)
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Variable to hold the type of colorscale to be from the RColorBrewer palette (default "RdYlBu")
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction TRUE/FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (defalt 15, 10)
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default Lon)
#' @param map_y_label (Character) - Label for y axis (default Lat)
#' @return (ggplot2 or Character) - Returns a ggplot object of the resulting map or an error string if failed
#' @importFrom sf st_transform
#' @importFrom dplyr mutate left_join
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete scale_fill_distiller ggplot geom_raster geom_sf coord_sf labs theme geom_sf_label
#' @importFrom ggspatial layer_spatial df_spatial
#' @importFrom classInt classIntervals
#' @import RColorBrewer
#' @export
create_choropleth <- function(shape_data = NULL, shape_key_field = NULL, shape_label_field = NULL, shape_label_size_field = "1", simplify = FALSE,
                       map_data = NULL, data_key_field = NULL, data_col = NULL, bin_method = NULL, bins = NULL,
                       dpi = 150, output_file = NULL, expand_xy = c(0, 0),
                       map_xy_min_max = c(-180, 180, -90, 90), map_title = NULL,  map_palette = "RdYlBu", map_palette_reverse = FALSE,
                       map_width_height_in = c(15, 10), map_legend_title = NULL, map_x_label = "Lon", map_y_label = "Lat")
{
  error <- ""
  output <- "Default error"
  tryCatch(
    {
      # Create shape object via local processing function
      shape_obj <- process_shape(shape_data, simplify, shape_label_field)

      # Verify object is a shape object, if not it's an error and return it
      if(!class(shape_obj) %in% "sf")
      {
        output <- shape_obj
        return(output)
      }

      # Read/process data object via local processing function
      map_data_obj <- process_data(map_data)

      # Merge map and data
      # combined_df <- merge(x = shape_obj, y = data_obj, by.x = shape_key_field, by.y = data_key_field)
      combined_df <- left_join(x = shape_obj, y = map_data_obj, by = setNames(data_key_field,  shape_key_field))

      # Crop shape - ?
      # shape <- st_crop(shape, 1.2*extent(raster))

      # Map output variables
      x_min <- map_xy_min_max[1]
      x_max <- map_xy_min_max[2]
      y_min <- map_xy_min_max[3]
      y_max <- map_xy_min_max[4]
      map_width <- map_width_height_in[1]
      map_height <- map_width_height_in[2]
      expand_x <- expand_xy[1]
      expand_y <- expand_xy[2]

      # Process map colors/scales options etc.
      palette_direction <- 1

      if(map_palette_reverse)
      {
        palette_direction <- -1
      }

      palette_type <- "qual"
      na_value <- "Grey"
      map_guide <- "colourbar"
      map_x_scale <- scale_x_discrete(limits=c(x_min, x_max), expand = expand_scale(add = expand_x), breaks=seq(x_min,x_max, abs(x_max - x_min)/12))
      map_y_scale <-  scale_y_discrete(limits=c(y_min, y_max), expand = expand_scale(add = expand_y), breaks=seq(y_min,y_max, abs(y_max - y_min)/6))
      map_color_scale <-  scale_fill_brewer(palette = map_palette, type = palette_type, direction = palette_direction, na.value = na_value)
      map_shape_options <- NULL
      map_size_guide_option <- NULL

      # Process optional shape label field
      if(!is.null(shape_label_field))
      {
        map_shape_options <- geom_sf_label(data = shape_obj, aes_string(label = shape_label_field, fill=NULL, size = shape_label_size_field))
        if(!is.null(shape_label_size_field))
        {
          map_size_guide_option <- guides(size = FALSE)
        }
      }

      # Process breaks/bins
      if(!is.null(bin_method) && !is.null(bins))
      {
        data_breaks <- classIntervals(c(min(as.numeric(combined_df[[data_col]])),as.numeric(combined_df[[data_col]])), n = bins, style = bin_method)
        combined_df <- mutate(combined_df, value = cut(as.numeric(combined_df[[data_col]]), data_breaks$brks))
      }
      else
      {
        error <- "bin_method and bins cannot be NULL"
      }

      # Build ggplot Map object
      output <- ggplot(data = combined_df, aes(x=LON, y=LAT,  fill=value)) +
        geom_sf(color="grey") +
         map_color_scale +
         coord_sf() +
         labs(x=map_x_label, y=map_y_label, title = map_title, fill = map_legend_title) +
         map_x_scale +
         map_y_scale +
         map_shape_options +
         theme(plot.title = element_text(hjust = 0.5)) +
         map_size_guide_option

      # Save File
      if(!is.null(output_file))
      {
        result <- gcammaptools::save_plot(output_file, dpi, map_width, map_height)
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
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @return (SF or Character) - Returns the resulting simplified SF object or an error string if failed
#' @export
process_shape <- function(shape_data, simplify, shape_label_field)
{
  tryCatch(
  {
    # Shape loading - if given a path, use that, else expect an object passed in
    if(is.null(shape_data))
    {
      error <- "Shape data is NULL"
    }
    else if(class(shape_data) %in% "sf")
    {
      shape_obj <- shape_data
    }
    else if(class(shape_data) %in% "character")
    {
      if (file.exists(shape_data))
      {
        shape_obj <- gcammaptools::import_mapdata(shape_data)
      }
      else
      {
        error <- paste0("Cannot open Shape file ", raster_data)
        return(error)
      }
    }
    else
    {
      error <- "Unrecognized shape_data argument."
    }

    # Optional argument to simplify polygons via the simplify_mapdata function
    if(simplify)
    {
      shape_obj <- gcammaptools::simplify_mapdata(shape_obj)
    }
  },
  error = function(err)
  {
    # error handler picks up error information
    error <- err
    return(error)
  })

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
#' @importFrom raster raster crs
#' @return (Raster or Character) - Returns the resulting raster object or an error string if failed
#' @export
process_raster <- function( raster_data , raster_col, raster_band, bin_method, bins, convert_zero)
{
  # Default projection if raster is missing CRS
  default_projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  tryCatch(
  {
    # Raster loading - if given a path, use that, else expect an object passed in
    if(is.null(raster_data))
    {
      error <- "Raster data is NULL"
    }
    else if(class(raster_data) %in% "RasterLayer")
    {
      raster_obj <- raster_data
    }
    else if(class(raster_data) %in% "character")
    {
      if (file.exists(raster_data))
      {
        raster_obj <- import_raster(raster_data)
      }
      else
      {
        error <- paste0("Cannot open Raster file ", raster_data)
        return(error)
      }
    }
    else
    {
      error <- "Unrecognized raster_data argument."
    }

    # Set raster band
    if(raster_band != 1)
    {
      raster_obj <- raster(raster_obj, band=raster_band)
    }

    # Convert zeroes to NA if enabled
    if(convert_zero == TRUE)
    {
      raster_obj[raster_obj==0] <- NA
    }

    # Verify raster CRS and assign default if NA
    if(is.na(crs(raster_obj)) || is.null(crs(raster_obj)))
    {
      raster::crs(raster_obj) <- crs(default_projection)
      print("Applying default CRS to raster (raster CRS was NA or NULL)")
    }
  },
  error = function(err)
  {
    # error handler picks up error information
    error <- err
    return(error)
  })

  return(raster_obj)
}


#' Handles saving the map output to disk
#'
#' Initially one line of code, this function is likely to be expanded
#'
#' @param output_file (Character) - Output file path and file name/type to save the resulting plot (e.g. "c:/temp/output.png") Available file types ("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg")
#' @param dpi (Numeric) - Settable DPI for different print/screen formats
#' @param map_width (Numeric) - Map width in inches
#' @param map_height (Numeric) - Map height in inches
#' @return (Character) - Returns a character string with success or an error string if failed
#' @importFrom ggplot2 ggsave
#' @export
save_plot <- function(output_file, dpi, map_width, map_height)
{
  output <- "success"
  tryCatch(
  {
    # Get file type dynamically and save to path
    file_type <- substr(x = output_file, start = (nchar(output_file)-2), stop = nchar(output_file))

    if(file_type %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"))
    {
      ggsave(filename = output_file, device = file_type, dpi = dpi, limitsize = TRUE,
             width = map_width, height = map_height)
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


#' Process data
#'
#' @param map_data (Data Frame or Character) - Either the full path string to data file or a data.frame object
#' @return (Data Frame or Character) - Returns the resulting simplified SF object or an error string if failed
#' @export
process_data <- function(map_data)
{
  tryCatch(
    {
      # Map Data - if given a path to a csv, use that, else expect a data.frame object passed in
      if(is.null(map_data))
      {
        error <- "Map data is NULL"
      }
      else if(class(map_data) %in% "data.frame")
      {
        map_obj <- map_data
      }
      else if(class(map_data) %in% "character")
      {
        if (file.exists(map_data))
        {
          map_obj <- read.csv(map_data, stringsAsFactors = F)
        }
        else
        {
          error <- paste0("Cannot open data file ", map_data)
          return(error)
        }
      }
      else
      {
        error <- "Unrecognized map_data argument."
      }
    },
    error = function(err)
    {
      # error handler picks up error information
      error <- err
      return(error)
    })

  return(map_obj)
}
