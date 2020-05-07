#' Error checking functions for maps package


#' Verify shape data
#'
#' This function runs a check on shape inputs and looks for errors
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_key_field (Character) - Name of key field in shape object for merging with map_data object
#' @param shape_data_field (Character) - Optional field for utilizing a field within the shape data as the map data field. Negates the map_data variable
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes & fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param shape_xy_fields (c(Character, Character)) - Vector that specifies the x and y field names in the shape object (default c("LAT", "LON"))
#' @param shape_geom_field (Character) - Specifies field within shape object that contains needed geometry (default "geometry")
#' @param simplify (Boolean) - Option to reduce the number or complexity of the polygons in the shape file (default FALSE)
#' @return (Character) - Returns a success token or an error string if failed
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
verify_shape <- function(shape_data, simplify = FALSE, shape_label_field = NULL, shape_data_field = NULL, shape_key_field = NULL,
                         shape_label_size = "1", shape_xy_fields = c("LAT", "LON"), shape_geom_field = "geometry")
{
    result <- "Success"
    default_projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    # Shape loading - if given a path, use that, else expect an object passed in
    if(is.null(shape_data))
    {
        return("Error: Shape data is NULL")
    }
    else if(suppressWarnings({"sf" %in% class(shape_data)}))
    {
        # Verify shape projection and assign default if NA or NULL
        if(is.na(crs(shape_data)) || is.null(crs(shape_data)))
        {
            sf::st_crs(shape_data) <- crs(default_projection)
            print("Applying default projection to shape (shape projection was NA or NULL)")
        }
    }
    else if("character" %in% class(shape_data))
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
        if(!shape_label_field %in% names(shape_data))
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

    if(!"numeric" %in% class(shape_label_size))
    {
        return("Error: Shape label size argument must be numeric ")
    }

    if(!simplify %in% c(TRUE, FALSE, NULL))
    {
        return("Error: Invalid value for simplify argument: Must be one of TRUE, FALSE, NULL.")
    }

    if(!shape_geom_field %in% names(shape_data))
    {
        return("Error: Shape geometry field does not exist in shape object.")
    }

    return(result)

}


#' Verify raster data
#'
#' This function runs a check on raster inputs and looks for errors, missing projection, or missing key fields
#'
#' @param raster_data (Raster or Character) - Either the full path string to raster file or a raster object
#' @param raster_col (Character) - Column name that contains the raster object's output variable
#' @param raster_band (Numeric) - Future variable for dealing with multi band or time series rasters etc (default 1)
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (default "pretty")
#' @param bins (Numeric) - Number of bins in which to divide the raster (default 8)
#' @param convert_zero (Boolean) - Convert raster zero values to NA (default FALSE)
#' @return (Character) - Returns a success token or an error string if failed
#' @importFrom raster crs
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
verify_raster <- function(raster_data , raster_col, raster_band = 1, bin_method = "pretty", bins = 8, convert_zero = FALSE)
{
    result <- "Success"

    # Raster loading - if given a path, use that, else expect an object passed in
    if(is.null(raster_data))
    {
        return("Error: Raster data is NULL")
    }
    else if("RasterLayer" %in% class(raster_data))
    {
        raster_obj <- raster_data
    }
    else if("character" %in% class(raster_data))
    {
        if (!file.exists(raster_data))
        {
            return(paste0("Error: Cannot process raster file or path: ", raster_data, "\nRaster file does not exist or raster path is malformed."))
        }
    }
    else
    {
        return("Error: Unrecognized raster_data argument. Raster_data must be of type RasterLayer or a path to a raster file.")
    }

    # Check raster_col argument to make sure it exists
    if(is.null(raster_col))
    {
        return("Error: No raster column defined")
    }

    # Check raster CRS and assign default if NA
    if(is.na(crs(raster_obj)) || is.null(crs(raster_obj)))
    {
      return("Error: Raster is missing projection. Please assign a projection to raster before mapping.")
    }

    return(result)
}


#' Verify map data
#'
#' This function runs a check on map data inputs and looks for errors
#'
#' @param map_data (Data Frame or Character) - A data frame that contains the output data to map, or alternatively a full path to a CSV
#' @param data_key_field (Character) - Name of key field in data_obj for merging with shape_data
#' @param data_col (Character) - Column name that contains the data object's output variable
#' @return (Character) - Returns a success token or an error string if failed
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
verify_data <- function(map_data, data_key_field, data_col)
{
    result <- "Success"

    # Verify map_data
    if(!is.null(map_data))
    {
        # Verify data_key_field
        if(!is.null(data_key_field))
        {
            if(!"character" %in% class(data_key_field))
                return("Error: `data_key_field`` argument must be of type character")
            if(!data_key_field %in% colnames(map_data))
                return("Error: `data_key_field`` was not found in the `map_data`` data frame")
        }

        # Verify data_col
        if(!is.null(data_col))
        {
            if(!"character" %in% class(data_col))
                return("Error: `data_col`` argument must be of type character")
            if(!data_col %in% colnames(map_data))
                return("Error: `data_col`` was not found in the `map_data`` data frame")
        }
    }

    return(result)
}


#' Verify map parameters
#'
#' Verify the set of arguments for choropleth and other map types
#'
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (one of "quantile", "equal", "pretty", "kmeans") (default "pretty")
#' @param bins (Numeric) - Number of bins in which to divide the raster
#' @param dpi (Numeric) - Settable DPI for different print and screen formats (default 150)
#' @param expand_xy (c(Numeric, Numeric)) - Sets expansion amount for the X and Y scale of the map - Vector (expand x, expand y) (default c(0,0))
#' @param map_xy_min_max (c(Numeric, ...)) - Vector that describes the desired extent of the map in the form of (xmin, xmax, ymin, ymax) (default: c(-180, 180, -90, 90))
#' @param map_title (Character) - Title to be displayed on the output map
#' @param map_palette (Character) - Optional variable to manually set the colorscale to a specific palette from RColorbrewer
#' @param map_palette_reverse (Boolean) - Set palette to reverse direction - TRUE or FALSE
#' @param map_width_height_in (c(Numeric, Numeric)) - Vector that describes the desired file size of the output image in the form of (width, height) in inches (default c(15, 10))
#' @param map_legend_title (Character) - Text for the legend header
#' @param map_x_label (Character) - Label for x axis (default Lon)
#' @param map_y_label (Character) - Label for y axis (default Lat)
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
verify_map_params <- function(bin_method = "pretty", bins = 8, dpi = 150, expand_xy = c(0,0), map_xy_min_max = c(-180, 180, -90, 90), map_title = "",
                              map_palette = NULL, map_palette_reverse = FALSE, map_palette_type = "seq", map_width_height_in = c(15, 10), map_legend_title = "",
                              map_x_label = "Lon", map_y_label = "Lat")
{
    output <- "Success"

    # Verify bin_method
    if(is.null(bin_method) || !"character" %in% class(bin_method) || !bin_method %in% c("quantile", "pretty", "equal"))
    {
        return("Error: `bin_method`` argument must be one of 'quantile', 'pretty', 'equal'")
    }

    # Verify bins
    if(is.null(bins) || class(bins) != "numeric")
    {
        return("Error: `bins`` argument must be a positive integer and not NULL")
    }
    if(bins < 1 || bins > 100)
    {
        return("Error: `bins`` argument must be a valid integer between 1-100")
    }

    # verify dpi
    if(!"numeric" %in% (class(dpi)) || dpi < 30 || dpi > 300)
    {
        return("Error: `dpi`` argument must be a numeric value between 30 and 300")
    }

    # Verify expand_xy
    if(!"numeric" %in% (class(expand_xy)) || length(expand_xy) != 2)
    {
        return("Error: `expand_xy`` argument must be a numeric vector of length 2")
    }

    # Verify map_xy_min_max
    if(!"numeric" %in% (class(map_xy_min_max)) || length(map_xy_min_max) != 4)
    {
        return("Error: `map_xy_min_max`` argument must be a numeric vector of length 4")
    }

    # Verify title
    if(!"character" %in% class(map_title) && !is.null(map_title))
    {
        return("Error: `map_title`` must be of class character")
    }

    # Verify map_palette
    if(!is.null(map_palette) && (!"character" %in% class(map_palette) || !map_palette %in% row.names(brewer.pal.info)) )
    {
        return("Error: `map_palette`` must be of class character and a valid entry in the RColorBrewer palette (see brewer.pal.info")
    }

    # Verify palette_reverse
    if(!"logical" %in% class(map_palette_reverse))
    {
        return("Error: `map_palette_reverse`` must be of type logical (TRUE or FALSE)")
    }

    # Verify map_width_height_in
    if(!"numeric" %in% (class(map_width_height_in)) || length(map_width_height_in) != 2)
    {
        return("Error: `map_width_height_in`` argument must be a numeric vector of length 2")
    }

    # Verify map_legend_title
    if(!"character" %in% class(map_legend_title) && !is.null(map_legend_title))
    {
        return("Error: `map_legend_title`` must be of class character or NULL")
    }

    # Verify map_x_label
    if(!"character" %in% class(map_x_label)  && !is.null(map_x_label))
    {
        return("Error: `map_x_label`` must be of class character or NULL")
    }

    # Verify map_y_label
    if(!"character" %in% class(map_y_label) && !is.null(map_y_label))
    {
        return("Error: `map_y_label`` must be of class character or NULL")
    }

    return(output)
}


#' Verify csv data
#'
#' This function runs a check on csv data inputs and looks for errors
#'
#' @param map_data (Data Frame or Character) - Either the full path string to data file (csv) or a data.frame object
#' @return (Character) - Returns a success token or an error string if failed
verify_csv <- function(map_data)
{

    # Check csv for things like character Unicode
    # Check field they provided there etc


    result <- "Success"

    return(result)

}



#' Return and output errors
#'
#' This function prints out any caught errors to the console and returns the custom error string to the calling function
#'
#' @param error (Character) - Specific error string for output
#' @param location (Character) - Location the error was caught
#' @return (Character) - Prints the error to console and also returns it to caller
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
#' @export
return_error <- function(error, location)
{
    print(paste0("There was an error at - ", location, " - building your map:"))
    print(error)

    return(error)
}
