# gcam_data_processing.R
#
# Contains function for transforming GCAM data into a format for easy mapping.

### TODO - modify to search for appropriate lookup, province, drop files in directory.
#' Match GCAM ID to region using data from a lookup table.
#'
#' We match by ID number to avoid problems with variant spellings and the like.
#' With the optional arguments you can also omit regions for which you don't
#' want to plot the data for some reason, and you can translate the
#' abbreviations used in subregion output.
#'
#' The \code{provincefile} and \code{drops} arguments are a little clunky.  They
#' are optional, but if you are using one of the built-in map sets, then you
#' \emph{must not} specify them if they don't exist for the map set you are
#' using.  Currently, \code{rgn14} and \code{basin235} have neither drops nor
#' province abbreviations.  The \code{rgn32} set has drops, but not province
#' abbreviations.  Only the \code{chn} set has both.
#' @param datatable A table of results produced by \code{\link[rgcam]{getQuery}}
#' @param lookupfile Name of one of the predefined map sets, OR, if you're using
#' a custom map set, the file containing the region lookup table
#' @param provincefile Name of one of the predefined map sets, OR, if you're
#' using a custom map set, file containing the province lookup table, if
#' applicable.
#' @param drops Name of one of the predefined map sets, OR, if you're using
#' a custom map set, the file containing a list of regions to drop, if
#' applicable.
#' @param disaggregate A column (or vector of columns) of \code{datatable} used
#' to disaggregate regions that are not specified in the original data.
#' @return Input table modified to include a GCAM ID for reach region.
#' @importFrom utils read.csv
#' @export
add_region_ID <- function(datatable, lookupfile = rgn32, provincefile = NULL, drops = NULL, disaggregate = NULL) {

    . <- NULL # silence package notes for NSE
    naReg <- "#N/A"

    # Make sure there is a region column in the data. "Region" is okay, but will
    # be replaced with "region".
    names(datatable)[names(datatable) == "Region"] <- "region"
    if (!"region" %in% names(datatable)) {
        stop("Data must contain a 'region' column")
    }

    if ("id" %in% names(datatable)) {
        message("id column is already present and will not be modified")
        return(datatable)
    }

    if (!is.null(provincefile)) {
        datatable <- translate_province(datatable, provincefile)
    }

    if (!is.null(drops)) {
        datatable <- drop_regions(datatable, drops)
    }

    lookuptable <- if (is.symbol(lookupfile)) {
        get.internal(lookupfile, "lut")
    } else {
        read.csv(lookupfile, strip.white = T, stringsAsFactors = F)
    }

    # Add row to end of the lookup table to account for GCAM region 0
    if (!any(lookuptable$region == naReg)) {
        lookuptable <- rbind(lookuptable, c(naReg, 0))
    }

    # If the user specifies columns to disaggregate, each unique combination of
    # those columns needs to be disaggregated to contain every region.
    # Grouping and then using dplyr's do() function joins the lookuptable to
    # each group, with NA values in all columns except for region and region id
    # (and the disaggregation columns filled in after).
    if (!is.null(disaggregate)) {
        finaltable <- datatable %>%
                      dplyr::group_by_at(dplyr::vars(disaggregate)) %>%
                      dplyr::do({
                          grp <- dplyr::full_join(., lookuptable, by = "region")
                          grp[ , disaggregate] <- grp[1, disaggregate]
                          grp
                      })
    } else {
        finaltable <- dplyr::full_join(datatable, lookuptable, by = "region")
    }

    # Set column name and type for id column
    names(finaltable)[ncol(finaltable)] <- "id"
    finaltable$id <- as.numeric(finaltable$id)

    return(finaltable)
}

#' Replace subregion abbreviations with full subregion names
#'
#' Subregions are given two-letter abbreviations in GCAM output.  This function
#' uses a lookup table to restore the full names.
#'
#' @param datatable The table with the abbreviated names in it.
#' @param provincefile Name of a defined mapset OR name of a file containing the
#' lookup table.
#' @importFrom utils read.csv
translate_province <- function(datatable, provincefile) {

    if (is.symbol(provincefile)) {
        provincetable <- get.internal(provincefile, "prov")
    }
    else {
        provincetable <- read.csv(provincefile, strip.white = T, stringsAsFactors = T)
    }

    datatable <- datatable %>%
        dplyr::left_join(provincetable, by = c("region" = "province")) %>%
        dplyr::mutate(region = dplyr::if_else(is.na(province.name), region, province.name)) %>%
        dplyr::select(-province.name)

    return(datatable)
}

#' Drop regions listed in drops file from data frame.
#'
#' @param datatable A data frame containing the output of a GCAM query.
#' @param drops String; path to file containing regions to be dropped
#' @return An updated data frame with regions dropped.
drop_regions <- function(datatable, drops) {

    if (is.symbol(drops)) {
        dr <- get.internal(drops, "drop")
    }
    else {
        dr <- read.csv(drops, strip.white = T, stringsAsFactors = F)
    }

    datatable <- dplyr::filter(datatable, !region %in% dr[[1]])

    return(datatable)
}


#' Get auxiliary data for a named mapset.
#'
#' We have several standard map sets.  Each of them has several auxiliary tables
#' associated with it.  This function retrieves the auxiliary table associated
#' with the requested.  Right now this function understands \code{rgn14},
#' \code{rgn32}, \code{basin235}, and \code{chn}.
#'
#' @param mapset The name of the mapset.  Can be either a symbol or a string.
#' @param type The type of table.  Right now this is either 'lut', 'drop', or
#' 'prov'
get.internal <- function(mapset, type) {
    eval(as.symbol(paste(type, mapset, sep = ".")))
}

