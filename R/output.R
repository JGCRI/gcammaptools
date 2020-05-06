# output.R
#
# Contains general output functions for the package



#' Handles saving the map output to disk
#'
#' Initially one line of code, this function is likely to be expanded
#'
#' @importFrom packageutils version_as_character
#' @export
print_version <- function() {

    print(version_as_character())
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
    output <- "Success"
    tryCatch(
    {
        if(is.null(output_file) || !"character" %in% class(output_file))
        {
            return("Error: output_file argument must be a valid character path")
        }

        # Get file type dynamically and save to path
        file_type <- substr(x = output_file, start = (nchar(output_file)-2), stop = nchar(output_file))

        if(file_type %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"))
        {
            ggsave(filename = output_file, device = file_type, dpi = dpi, limitsize = TRUE,
                   width = map_width, height = map_height)
        }
        else
        {
            return("Error: Unrecognized output file type (must be one of eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg")
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
