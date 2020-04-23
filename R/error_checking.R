#' Error checking functions for maps package


#' Verify shape data
#'
#' This function runs a check on shape inputs and looks for errors
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_shape <- function(shape_data, simplify, shape_label_field, shape_data_field = NULL)
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
        if(!shape_data_field %in% shape_obj)
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

    if(!simplify %in% c(TRUE, FALSE, NULL))
    {
        return("Error: Invalid value for simplify argument: Must be one of TRUE, FALSE, NULL.")
    }

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
#' @param map_data (Data Frame or Character) - Either the full path string to data file (csv) or a data.frame object
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_data <- function(map_data)
{
    # Future
    # 	Does it have nulls?
    #  	Is the field there?
    #  	Warning that pops up to the user about null values

    result <- "Success"

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

