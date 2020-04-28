#' Error checking functions for maps package


#' Verify shape data
#'
#' This function runs a check on shape inputs and looks for errors
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_key_field (Character) - Name of key field in shape object for merging with map_data object
#' @param shape_data_field (Character) - Optional field for utilizing a field within the shape data as the map data field. Negates the map_data variable
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param shape_xy_fields (c(Character, Character)) - Vector that specifies the x and y field names in the shape object (default c("LAT", "LON"))
#' @param shape_geom_field (Character) - Specifies field within shape object that contains needed geometry (default "geometry")
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_shape <- function(shape_data, simplify, shape_label_field, shape_data_field = NULL, shape_key_field = NULL,
                         shape_label_size, shape_xy_fields, shape_geom_field)
{
    # Check shape file has projection
    # looks for the mentioned shape key field
    # looks for projection
    # check simplify for valid option (true or false)

    result <- "Success"

    # Shape loading - if given a path, use that, else expect an object passed in
    if(is.null(shape_data))
    {
        return("Error: Shape data is NULL")
    }
    else if(class(shape_data) %in% "sf")
    {
        # Verify raster CRS and assign default if NA
        if(is.na(crs(shape_data)) || is.null(crs(shape_data)))
        {
            sf::st_crs(shape_data) <- crs(default_projection)
            print("Applying default CRS to shape (shape CRS was NA or NULL)")
        }
    }
    else if(class(shape_data) %in% "character")
    {
        if (!file.exists(shape_data))
        {
            return("Error: Cannot open shape file or shape file path does not exist ")
        }
    }
    else
    {
        return("Error: Unrecognized shape_data argument.")
    }

    if(!is.null(shape_data_field))
    {
        # look for shape data field
        if(!shape_data_field %in% names(shape_data))
        {
            return("Error: Shape data field does not exist in shape object.")
        }
    }

    if(!is.null(shape_label_field))
    {
        # look for shape data field
        if(!shape_label_field %in% shape_obj)
        {
            return("Error: Shape label field does not exist in shape object.")
        }
    }

    if(!is.null(shape_key_field))
    {
        # look for shape data field
        if(!shape_key_field %in% names(shape_data))
        {
            return("Error: Shape key field does not exist in shape object.")
        }
    }

    if(!class(shape_label_size) %in% "character" )
    {
        return("Error: Shape label size argument must be of type character (e.g. '1')")
    }

    if(!simplify %in% c(TRUE, FALSE, NULL))
    {
        return("Error: Invalid value for simplify argument: Must be one of TRUE, FALSE, NULL.")
    }

    if(!shape_geom_field %in% names(shape_data))
    {
        return("Error: Shape geometry field does not exist in shape object.")
    }

    # if(class(shape_xy_fields))
    #   {
    #
    # }
    return(result)

}


#' Verify raster data
#'
#' This function runs a check on raster inputs and looks for errors
#' @param raster_data (Raster or Character) - Either the full path string to raster file or a raster object
#' @param raster_col (Character) - Column name that contains the raster object's output variable
#' @param raster_band (Numeric) - Future variable for dealing with multi band/time series rasters etc
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param convert_zero (Boolean) - Convert raster zero values to NA#
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_raster <- function(raster_data , raster_col, raster_band, bin_method, bins, convert_zero)
{
    # Verify raster_data and class
    # looks for  raster_col key field
    # looks for projection
    # verify bin method
    # verify bins

    # Default projection if raster is missing CRS
    default_projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    result <- "Success"

    # Raster loading - if given a path, use that, else expect an object passed in
    if(is.null(raster_data))
    {
        return("Error: Raster data is NULL")
    }
    else if(class(raster_data) %in% "RasterLayer")
    {
        raster_obj <- raster_data
    }
    else if(class(raster_data) %in% "character")
    {
        if (!file.exists(raster_data))
        {
            return("Error: Cannon process raster file: Raster file does not exist or raster path is malformed.")
        }
    }
    else
    {
        return("Error: Unrecognized raster_data argument.")
    }

    # Check raster_col argument to make sure it exists
    if(is.null(raster_col)) # || raster_obj[raster_col] !exists
    {
        return("Error: No raster column defined")
    }

    # Verify raster CRS and assign default if NA
    if(is.na(crs(raster_obj)) || is.null(crs(raster_obj)))
    {
        raster::crs(raster_obj) <- crs(default_projection)
        print("Applying default CRS to raster (raster CRS was NA or NULL)")
    }

    return(result)
}


#' Verify map data
#'
#' This function runs a check on map data inputs and looks for errors
#' @param map_data (Data Frame or Character) - A data frame that contains the output data to map, or alternatively a full path to a CSV
#' @param data_key_field (Character) - Name of key field in data_obj for merging with shape_data
#' @param data_col (Character) - Column name that contains the data object's output variable
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_data <- function(map_data, data_key_field, data_col)
{
    # Future
    # 	Does it have nulls?
    #  	Is the field there?
    #  	Warning that pops up to the user about null values

    result <- "Success"

    # Verify map_data
    if(!is.null(map_data))
    {
        # Verify data_key_field
        if(!is.null(data_key_field))
        {
            if(!class(data_key_field) %in% "character")
                return("Error: data_key_field argument must be of type character")
            if(!data_key_field %in% colnames(map_data))
                return("Error: data_key_field was not found in the map_data data frame")
        }

        # Verify data_col
        if(!is.null(data_col))
        {
            if(!class(data_col) %in% "character")
                return("Error: data_col argument must be of type character")
            if(!data_col %in% colnames(map_data))
                return("Error: data_col was not found in the map_data data frame")
        }

        # Verify shape key = data key

    }



    return(result)
}


#' Verify csv data
#'
#' This function runs a check on csv data inputs and looks for errors
#' @param map_data (Data Frame or Character) - Either the full path string to data file (csv) or a data.frame object
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_csv <- function(map_data)
{

    # Check csv for things like character Unicode
    # Check field they provided there etc


    result <- "Success"

    return(result)

}


#' Verify map parameters
#'
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (one of "quantile", "equal", "pretty", "kmeans") (default "pretty")
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster
#' @param dpi (Numeric) - Settable DPI for different print/screen formats (default 150)
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y) (default c(0,0))
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: c(-180, 180, -90, 90))
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Optional variable to manually set the colorscale to a specific palette from RColorbrewer
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction TRUE/FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (default c(15, 10))
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default Lon)
#' @param map_y_label (Character) - Label for y axis (default Lat)
verify_map_params <- function(bin_method, bins, dpi, expand_xy, map_xy_min_max , map_title, map_palette, map_palette_reverse,
                               map_palette_type, map_width_height_in, map_legend_title, map_x_label, map_y_label)
{
    output <- "Success"

    # Verify bin_method
    if(!bin_method %in% c("quantile", "pretty", "equal"))
    {
        return("Error: bin_method argument must be one of 'quantile', 'pretty', 'equal'")
    }

    # Verify bins
    if(is.null(bins) || class(bins) != "numeric")
    {
        return("Error: bins argument must be a positive integer and not NULL")
    }
    if(bins < 1 || bins > 100)
    {
        return("Error: bins argument must be a valid integer between 1-100")
    }

    # verify dpi
    if(!(class(dpi) %in% "numeric") || dpi < 30 || dpi > 300)
    {
        return("Error: DPI argument must be a numeric value between 30 and 300")
    }

    # Verify expand_xy
    if(!(class(expand_xy) %in% c("Vector", "numeric")) || length(expand_xy) != 2)
    {
        return("Error: expand_xy argument must be a numeric vector of length 2")
    }

    # Verify map_xy_min_max
    if(!(class(map_xy_min_max) %in% c("Vector", "numeric")) || length(map_xy_min_max) != 4)
    {
        return("Error: map_xy_min_max argument must be a numeric vector of length 4")
    }

    # Verify title
    if(!class(map_title) %in% "character" && !is.null(map_title))
    {
        return("Error: map_title must be of class character")
    }

    # Verify map_palette
    if(!is.null(map_palette) && (!class(map_palette) %in% "character" || !(map_palette %in% row.names(brewer.pal.info)) ))
    {
        return("Error: map_palette must be of class character and a valid entry in the RColorBrewer palette (see brewer.pal.info")
    }

    # Verify palette_reverse
    if(!class(map_palette_reverse) %in% "logical" )
    {
        return("Error: map_palette_reverse must be of type logical (TRUE or FALSE)")
    }

    # Verify map_width_height_in
    if(!(class(map_width_height_in) %in% c("Vector", "numeric")) || length(map_width_height_in) != 2)
    {
        return("Error: map_width_height_in argument must be a numeric vector of length 2")
    }

    # Verify map_legend_title
    if(!class(map_legend_title) %in% "character" && !is.null(map_legend_title))
    {
        return("Error: map_legend_title must be of class character")
    }

    # Verify map_x_label
    if(!class(map_x_label) %in% "character" && !is.null(map_x_label))
    {
        return("Error: map_x_label must be of class character")
    }

    # Verify map_y_label
    if(!class(map_y_label) %in% "character" && !is.null(map_y_label))
    {
        return("Error: map_y_label must be of class character")
    }

    return(output)
}
