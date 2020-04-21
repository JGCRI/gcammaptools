#' Error checking functions for maps package


#' Verify shape data
#'
#' This function runs a check on shape inputs and looks for errors
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @return (Character) - Returns a success token or an error string if failed
#' @export
verify_shape <- function(shape_data, simplify, shape_label_field)
{
    # Check shape file has projection
    # looks for the mentioned shape key field
    # looks for projection
    # check simplify for valid option (true or false)

    result <- "Success"

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

    result <- "Success"

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

    result <- "Success"

    return(result)

}

