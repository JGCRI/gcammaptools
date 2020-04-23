# output.r
#
# This file produces output from standard functions for various maps

#' Create a basic map object from input shape/raster and return, save (optional) the output
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
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y) (default c(0,0))
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: c(-180, 180, -90, 90))
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Variable to hold the type of colorscale to be from the RColorBrewer palette (default "RdYlBu")
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction TRUE/FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (defalt c(15, 10))
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default "Lon")
#' @param map_y_label (Character) - Label for y axis (default "Lat")
#' @return (ggplot2 or Character) - Returns a ggplot object of the resulting map or an error string if failed
#' @importFrom sf st_transform
#' @importFrom raster raster as.data.frame compareCRS minValue maxValue nlayers
#' @importFrom dplyr mutate
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_distiller ggplot geom_raster geom_sf coord_sf labs theme geom_sf_label
#' @importFrom ggspatial layer_spatial df_spatial
#' @import RColorBrewer
#' @export
custom_map <- function(shape_data = NULL, shape_label_field = NULL, shape_label_size_field = "1", simplify = FALSE,
                       raster_data = NULL, raster_col = NULL,  raster_band = 1,  convert_zero = FALSE,
                       dpi = 150, output_file = NULL, expand_xy = c(0, 0),
                       map_xy_min_max = c(-180, 180, -90, 90), map_title = NULL,  map_palette = "RdYlBu", map_palette_reverse = FALSE,
                       map_width_height_in = c(15, 10), map_legend_title = NULL, map_x_label = "Lon", map_y_label = "Lat")
{
    error <- ""
    output <- "Default error"
    tryCatch(
    {
      result <- verify_shape(shape_data, simplify, shape_label_field)

      # If shape passes verification, load and process
      if(result == "Success")
      {
        # Create shape object via local processing function
        shape_obj <- process_shape(shape_data, simplify, shape_label_field)
      }
      else
      {
        # Verify shape failed, return result
        return(result)
      }

      # Verify processed object is a shape object. If not, it's an error and return it
      if(class(shape_obj) == "character")
      {
        return(paste0("Error: There was an error processing the shape object: ", shape_obj))
      }

      # Create raster
      raster_obj <- process_raster(raster_data , raster_col, raster_band, bin_method, bins, convert_zero)

      if(class(raster_obj) == "character")
      {
        return("Error: Raster argument is not of type RasterLayer")
      }


      # Perform shape and raster comparisons/other operations
      # Compare projections and equalize if necessary
      if(!compareCRS(shape_obj, raster_obj))
      {
        # Transform shape to match raster CRS
          shape_obj <- st_transform(shape_obj, crs(raster_df))
      }

      # Crop shape - for extent changes (future
      # shape <- st_crop(shape, 1.2*extent(raster))

      # Raster operations/Convert raster
      raster_df <- df_spatial(raster_obj)  #raster_df <- as.data.frame(raster_obj, xy=TRUE) compare performance
      raster_df <- mutate(raster_df, value = raster_df[[paste0("band", 1)]])

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

      # Build map x and y scales using continuous scales
      map_x_scale <-  scale_x_continuous(limits=c(x_min, x_max), expand = expand_scale(add = expand_x), breaks=seq(x_min,x_max, abs(x_max - x_min)/12))
      map_y_scale <-  scale_y_continuous(limits=c(y_min, y_max), expand = expand_scale(add = expand_y), breaks=seq(y_min,y_max, abs(y_max - y_min)/6))
      map_color_scale <-  scale_fill_distiller(palette = map_palette, type = palette_type, direction = palette_direction, na.value = na_value, guide = map_guide)
      map_shape_options <- NULL
      map_size_guide_option <- NULL

      # Build geometry labels if enabled by user
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


#' Create a choropleth map object from shape and data object and return, save (optional) the output
#'
#' Choropleth map
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_key_field (Character) - Name of key field in shape object for merging with map_data object
#' @param shape_data_field (Character) - Optional field for utilizing a field within the shape data as the map data field. Negates the map_data variable
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param shape_xy_fields (c(Character, Character)) - Vector that specifies the x and y field names in the shape object (default c("LAT", "LON"))
#' @param shape_geom_field (Character) - Specifies field within shape object that contains needed geometry (default "geometry")
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param map_data (Data Frame or Character) - A data frame that contains the output data to map, or alternatively a full path to a CSV
#' @param data_key_field (Character) - Name of key field in data_obj for merging with shape_data
#' @param data_col (Character) - Column name that contains the data object's output variable
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (one of "quantile", "equal", "pretty", "kmeans") (default "pretty")
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param dpi (Numeric) - Settable DPI for different print/screen formats (default 150)
#' @param output_file (Character) - Output file path and file name/type to save the resulting plot (e.g. "c:/temp/output.png") (Types accepted: "pdf", "jpeg", "tiff", "png", "bmp", "svg", "eps", "ps", "tex") (default PNG)
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y) (default c(0,0))
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: c(-180, 180, -90, 90))
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Optional variable to manually set the colorscale to a specific palette from RColorbrewer
#' @param map_palette_type (Character) - Variable to load default palette by type of data ("qual" for qualitative data, "seq" for sequential data, "div" for divergent data) (default "seq")
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction TRUE/FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (default c(15, 10))
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default Lon)
#' @param map_y_label (Character) - Label for y axis (default Lat)
#' @return (ggplot2 or Character) - Returns a ggplot object of the resulting map or an error string if failed
#' @importFrom sf st_transform st_crs
#' @importFrom dplyr mutate left_join
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete scale_fill_distiller ggplot geom_raster geom_sf coord_sf labs theme geom_sf_label theme_minimal expand_scale scale_fill_brewer aes_string element_text
#' @importFrom ggspatial layer_spatial df_spatial
#' @importFrom classInt classIntervals
#' @import RColorBrewer
#' @export
choropleth <- function(shape_data = NULL, shape_key_field = NULL, shape_label_field = NULL, shape_label_size_field = "1",
                              shape_data_field = NULL, shape_xy_fields = c("LON", "LAT"), shape_geom_field = "geometry", simplify = FALSE,
                              map_data = NULL, data_key_field = NULL, data_col = NULL, bin_method = "pretty", bins = NULL,
                              dpi = 150, output_file = NULL,  expand_xy = c(0, 0),
                              map_xy_min_max = c(-180, 180, -90, 90), map_title = NULL, map_palette = NULL,
                              map_palette_reverse = FALSE, map_palette_type = "seq", map_width_height_in = c(15, 10),
                              map_legend_title = NULL, map_x_label = "Lon", map_y_label = "Lat")
{
  output <- "Default error"
  tryCatch(
    {
      # Verify shape
      result <- verify_shape(shape_data, simplify, shape_label_field)

      # If shape passes verification, load and process
      if(result == "Success")
      {
        # Create shape object via local processing function
        shape_obj <- process_shape(shape_data, simplify, shape_label_field, shape_data_field)
      }
      else
      {
        # Verification failed, return result
        return(result)
      }

      # Verify processed object is a shape object. If not, it's an error and return it
      if(class(shape_obj) == "character")
      {
        return(paste0("Error: There was an error processing the shape object: ", shape_obj))
      }

      # Read/process map data object via local processing function
      if(is.null(shape_data_field))
      {
        # Verify map_data first and if not Success then return error now
        if(verify_data(map_data) == "Success")
        {
          map_data_obj <- process_data(map_data)
          if(class(map_data_obj) == "character")
          {
            # Process_data must have caught an error
            return(map_data_obj)
          }
        }
        else
        {
          # Verification failed, return result
          return(map_data)
        }

        # Merge map and data
        combined_df <- left_join(x = shape_obj, y = map_data_obj, by = setNames(data_key_field,  shape_key_field))
      }
      else
      {
        combined_df <- as.data.frame(shape_obj)
        data_col <- shape_data_field
      }

      # Map output variables
      x_min <- map_xy_min_max[1]
      x_max <- map_xy_min_max[2]
      y_min <- map_xy_min_max[3]
      y_max <- map_xy_min_max[4]
      map_width <- map_width_height_in[1]
      map_height <- map_width_height_in[2]
      expand_x <- expand_xy[1]
      expand_y <- expand_xy[2]

      # Determine palette direction
      if(map_palette_reverse)
      {
        palette_direction <- -1
      }
      else
      {
        palette_direction <- 1
      }

      # Determine palette type
      if(map_palette_type == "qual")
      {
        palette_type <- "qual"
        palette_colors <- "Paired"
      }
      else if(map_palette_type == "div")
      {
        palette_type <- "div"
        palette_colors <- "RdYlBu"
      }
      else
      {
        palette_type <- "seq"
        palette_colors <- "Blues"
      }

      # Override palette if explicitly set in map_palette argument
      if(!is.null(map_palette))
      {
        palette_colors <- map_palette
      }

      # Set additional map options and create scales
      na_value <- "Grey"
      map_guide <- "colourbar"
      map_x_scale <- scale_x_discrete(limits=c(x_min, x_max), expand = expand_scale(add = expand_x), breaks=seq(x_min,x_max, abs(x_max - x_min)/12))
      map_y_scale <-  scale_y_discrete(limits=c(y_min, y_max), expand = expand_scale(add = expand_y), breaks=seq(y_min,y_max, abs(y_max - y_min)/6))
      map_color_scale <-  scale_fill_brewer(palette = palette_colors, type = palette_type, direction = palette_direction, na.value = na_value)
      map_shape_options <- NULL
      map_size_guide_option <- NULL
      shape_x <- shape_xy_fields[1]
      shape_y <- shape_xy_fields[2]
      shape_geom <- shape_geom_field

      # Build geometry labels if enabled by user
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
        if(bin_method %in% c("quantile", "pretty", "equal"))
        {
          data_breaks <- classIntervals(c(min(as.numeric(combined_df[[data_col]])),as.numeric(combined_df[[data_col]])), n = bins, style = bin_method)
          combined_df <- mutate(combined_df, value = cut(as.numeric(combined_df[[data_col]]), data_breaks$brks))
        }
        else
        {
          return("Error: bin_method type not in quantile, pretty, or equal")
        }
      }
      else
      {
        return("Error: bin_method and bins cannot be NULL")
      }

      # Build ggplot Map object
      output <- ggplot(data = combined_df, aes_string(x=shape_x, y=shape_y,  fill="value", geometry=shape_geom)) +
        geom_sf(color="grey") +
         map_color_scale +
         coord_sf() +
         labs(x=map_x_label, y=map_y_label, title = map_title, fill = map_legend_title) +
         map_x_scale +
         map_y_scale +
         map_shape_options +
        theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
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
      return(err)
    })

  return(output)
}


#' Create a distributed flow map object from shape and data object and return, save (optional) the output
#'
#' Distributed flow map
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_key_field (Character) - Name of key field in shape object for merging with map_data object
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param shape_xy_fields (c(Character, Character)) - Vector that specifies the x and y field names in the shape object (default c("LAT", "LON"))
#' @param shape_geom_field (Character) - Specifies field within shape object that contains needed geometry (default "geometry")
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param map_data (Data Frame or Character) - A data frame that contains the output data to map, or alternatively a full path to a CSV
#' @param data_key_field (Character) - Name of key field in data_obj for merging with shape_data
#' @param data_col (Character) - Column name that contains the data object's output variable
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (one of "quantile", "equal", "pretty", "kmeans") (default "pretty")
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param dpi (Numeric) - Settable DPI for different print/screen formats (default 150)
#' @param output_file (Character) - Output file path and file name/type to save the resulting plot (e.g. "c:/temp/output.png") (Types accepted: "pdf", "jpeg", "tiff", "png", "bmp", "svg", "eps", "ps", "tex") (default PNG)
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y) (default c(0,0))
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: c(-180, 180, -90, 90))
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Optional variable to manually set the colorscale to a specific palette from RColorbrewer
#' @param map_palette_type (Character) - Variable to load default palette by type of data ("qual" for qualitative data, "seq" for sequential data, "div" for divergent data) (default "seq")
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction TRUE/FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (default c(15, 10))
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default Lon)
#' @param map_y_label (Character) - Label for y axis (default Lat)
#' @return (ggplot2 or Character) - Returns a ggplot object of the resulting map or an error string if failed
#' @importFrom sf st_transform st_crs
#' @importFrom dplyr mutate left_join
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete scale_fill_distiller ggplot geom_raster geom_sf coord_sf labs theme geom_sf_label theme_minimal
#' @importFrom ggspatial layer_spatial df_spatial
#' @importFrom classInt classIntervals
#' @import RColorBrewer
#' @export
distributed_flow <- function(shape_data = NULL, shape_key_field = NULL, shape_label_field = NULL, shape_label_size_field = "1",
                              shape_xy_fields = c("LON", "LAT"), shape_geom_field = "geometry", simplify = FALSE,
                              map_data = NULL, data_key_field = NULL, data_col = NULL, bin_method = "pretty", bins = NULL,
                              dpi = 150, output_file = NULL,  expand_xy = c(0, 0),
                              map_xy_min_max = c(-180, 180, -90, 90), map_title = NULL, map_palette = NULL,
                              map_palette_reverse = FALSE, map_palette_type = "seq", map_width_height_in = c(15, 10),
                              map_legend_title = NULL, map_x_label = "Lon", map_y_label = "Lat")
{





}

