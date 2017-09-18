## MAP FUNCTIONS

#' Helper to set column name for NULL.
#'
#' Set column name to substitute position when NULL.
#'
#' @param col Field name with target information to map.
#' @param sub_val Field position to substitue as column reference
set_col <- function(col, sub_val = 1) {

    if (is.null(col)) {
        return(1)
    }
    else {
        return(col)
    }
}

#' Set color scheme for ggplot object.
#'
#' Build color scheme object for ggplot.
#'
#' @param mapdata The sf object containing the spatial data.
#' @param col Field name with target information to map.
#' @param qtitle User specified title for color bar
#' @param nacolor Color for NA features
#' @param colors Color vector
#' @param colorfcn Function to create color scheme
set_color_scheme <- function(mapdata, col, qtitle, nacolor, colors, colorfcn) {

    # if column null set as index
    clm <- set_col(col)

    # use default color gradient if none specified
    if (is.null(col)) {
        clr <- nacolor
    }
    else if (!is.null(col) & (is.null(colors))) {
        clr <- DEFAULT_CHOROPLETH
    }
    else {
        clr <- colors
    }

    # use alternate title if user provided
    if (!is.null(qtitle)) {
        nm <- qtitle
    }
    else {
        nm <- clm
    }

    # for continuous data
    if (is.numeric(mapdata[[clm]])) {

        return(ggplot2::scale_fill_gradientn(name = nm, colors = clr,
                                        values = NULL, guide = GUIDE,
                                        space = SPACE, na.value = nacolor))
    }
    # for discrete
    else {

        # set color scheme using function
        if (is.null(colorfcn)) {
            clr <- qualPalette(n = length(unique(mapdata[[clm]])))
        }
        else {
            clr <- colorfcn(n = length(unique(mapdata[[clm]])))
        }

        return(ggplot2::scale_fill_manual(values = clr, name = nm))
    }
}

#' Build the coordinate zooming bounds for ggplot object.
#'
#' Either take the bounding box or the map coordinates depending on extent.
#'
#' @param mapdata The sf object containing the spatial data.
#' @param bbox Bounding box.
#' @param extent Extent provided by the user or as default.
#' @param p4s Proj4 string set by user
zoom_bounds <- function(mapdata, bbox, extent, p4s) {

    if (!isTRUE(all.equal(extent, EXTENT_WORLD))) {

        # reproject bounding box and get bounds
        bx <- reproject(bbox, prj4s = p4s) %>%
            sf::st_bbox()

        return(ggplot2::coord_sf(crs = p4s, datum = sf::st_crs(p4s),
                                 xlim = c(bx[1], bx[3]), ylim = c(bx[2], bx[4]),
                                 expand = TRUE))
    }
    else {

        # use map bounds instead of bounding box for zoom
        bx <- sf::st_bbox(mapdata)

        return(ggplot2::coord_sf(xlim = c(bx[1], bx[3]),
                                 ylim = c(bx[2], bx[4])))
    }
}

#' Get features topologically associated with extent bounds.
#'
#' Conducts a spatial join to retrieve spatial features that are topologically associated
#' (intersects, contains, within, etc.) with the provided bounds.
#'
#' @param mapdata The sf object containing the spatial data.
#' @param bbox Bounding box.
#' @param extent Extent provided by the user or as default.
#' @param col Field name with target information to map.
#' @param agr_type Inherited attribute-geometry-relationship type from plot_GCAM function params.
#' @param topo SF topologic function to define how the join will be conducted. Default
#' is to join any feature that intersects the bounding box.
filter_spatial <- function(mapdata, bbox, extent, col, agr_type=agr_type, topo=sf::st_intersects) {
  # set NULL column to index
  clm <- set_col(col)

  # set attribute-geometry-relationship for input mapdata column and bounding box feature attribute
  st_agr(mapdata) = agr_type
  st_agr(bbox) = agr_type

  # Message for st_join suppressed:  "although coordinates are longitude/latitude, it is
  #  assumed that they are planar."  This comes from the input projection being a
  #  geographic coordinate system and not a projected one when conducting topological
  #  operations such as st_intersects used in the st_join.  if 'longlat' appears in the
  #  proj string ("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") then this message
  #  will present itself due to the operation being intersect according to the source code.
  #  There is no option to quiet this.  We are getting the expected return from the
  #  operation due to harmonizing the map and bounding box projection pre-join.

  # if extent is not world conduct spatial join; else, return all
  if (!isTRUE(all.equal(extent, EXTENT_WORLD))) {
    return(suppressMessages({sf::st_join(mapdata[clm], bbox, left = FALSE)}))
  }
  # conducting the intersection here eliminates erroneous-filled poly generated at the global extent
  else {
    return(sf::st_intersection(bbox, mapdata[clm])[clm])
  }
}

#' Join GCAM data with spatial data.
#'
#' Joins GCAM data from rgam query and inner joins it to spatial data provided by the user.
#' Note:  due to conducting an inner join, only the keys that are present in both datasets
#' will be represented.
#'
#' @param mapdata The sf object containing the spatial data and a tuple identifier that
#' can be referenced in the gcam_df data frame.
#' @param mapdata_key Name of the field having a tuple identifier that can be referenced
#' in the gcam_df data frame.
#' @param gcam_df The GCAM data frame provided from the user.  This is usually generated from
#' an rgcam query.
#' @param gcam_key Name of field having a tuple identifier that can be referenced in the
#' mapdata data frame.
join_gcam <- function(mapdata, mapdata_key, gcam_df, gcam_key) {

    if (!is.null(gcam_df)) {

        # Make sure join keys are valid
        if (is.null(mapdata_key) || !(mapdata_key %in% names(mapdata))) {
            stop("You must provide a valid key for joining the spatial data")
        }
        if (is.null(gcam_key) || !(gcam_key %in% names(gcam_df))) {
            stop("You must provide a valid key for joining the GCAM data")
        }

        # add pkey fields for join
        mapdata['pkey'] <- mapdata[[mapdata_key]]
        gcam_df['pkey'] <- gcam_df[gcam_key]

        # Join the map data and gcam data using the keys provided
        # Note that using dplyr::left_join() here can cause the result to no
        # longer be an sf object as documented here: https://github.com/r-spatial/sf/issues/343 ;
        # to remedy until dplyr creates an sf join function cast back to sf obj
        mapdata <- dplyr::left_join(mapdata, gcam_df, by='pkey') %>%
                      sf::st_as_sf()
    }
    return(mapdata)
}

#' Import ESRI Shapefile or GeoJSON as sf object.
#'
#' Creates a Simple Feature (sf) object from full path string to ESRI Shapefile or
#' GeoJSON file. User defines which field is supposed to represent the ID for the data.
#'
#' @param file_pth Full path to shapefile with extention (.shp).  Shapefiles must contain at least
#' .shp, .shx, and .dbf file to function properly.
load_shp <- function(file_pth) {

    # read into an sf object
    return(sf::st_read(file_pth, quiet = TRUE))
}

#' Single import function for compatible data types.
#'
#' Imports available for sf objects, spatial data frames, ESRI Shapefiles, or
#' GeoJSON files.
#'
#' @param obj Input full path string or object
#' @param fld Field name to use as identifier
#' @param prj4s Proj4 string for projection (default WGS84)
#' @export
import_mapdata <- function(obj, fld = NULL, prj4s = wgs84) {

    # get object class
    cls <- class(obj)

    # check for sf data frame object
    if (cls[1] == "sf") {
        return(obj)
    }

    # check for file path
    else if (is.character(obj)) {

        # get file extension
        extn <- tolower(c(tools::file_ext(obj)))

        # if ESRI Shapefile or GeoJSON file
        if (extn %in% list('shp', 'geojson')) {

            # load Shapefile
            return(load_shp(file_pth = obj))
        }
        # catch unknown
        else {
            return(NULL)
        }
    }
    # check for spatial data frames
    else if (cls %in% list("SpatialPolygonsDataFrame", "SpatialPointsDataFrame",
                           "SpatialLinesDataFrame")) {

        return(sf::st_as_sf(obj))
    }
    else {
        return(NULL) # catch_error: object type not understood.
    }
}


#' Retrieve proj4 projection string.
#'
#' Provides a lookup list for default proj4 strings utilized.  Users
#' may also specify their own lookup list.  Options also include
#' providing either the EPSG, ESRI, or SR-ORG projection codes to
#' retrieve the associated proj4 string from a web query from
#' http://spatialreference.org.  Definitions for proj4 string
#' parmeters can be referenced here:
#' http://proj4.org/parameters.html#parameter-list
#'
#' @param obj Use object instead that has a predefined proj4 string.
#' @param prj_type The projection type is either 'esri', 'epsg', or 'sr-org' or 'prj4s_key'.
#' @param prj_code The projection code as an integer for EPSG, ESRI, or SR-ORG.
#' @param prj4s_key Lookup key string identifying the default projection.
#' @param lu A key = value list where key is a string and value is the
#' associated proj4 string.
get_prj4s <- function(obj = NULL, prj_type = NULL, prj_code = NULL, prj4s_key = NULL, lu = NULL) {

    # default prj4 key: string lookup
    def_lu <- list('us' = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83",
                   'africa' = "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 ",
                   'world' = "+proj=longlat +datum=WGS84 +no_defs",
                   'ch_aea' = "+proj=aea +lat_1=27 +lat_2=45 +x_0=0 +y_0=0 +lat_0=35 +lon_0=105 +ellps=WGS84 +datum=WGS84",
                   'eck3' = "+proj=eck3 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
                   )

    # use default lookup if none provided
    if (is.null(lu)) {
        lu <- def_lu
    }

    # use object if defined
    if (!is.null(obj) && (obj %in% names(lu))) {
        return(lu[[obj]])
    }
    else if (!is.null(obj)) {
        return(obj)
    }

    # if a prj4 key is provided use it
    if (prj_type == 'prj4s_key' && !is.null(prj4s_key)) {
        return(lu[[prj4s_key]])
    }
    # otherwise lookup proj4 string by url
    else if (!is.null(prj_type) && !is.null(prj_code)) {

        # create url
        url <- paste0('http://spatialreference.org/ref/', prj_type, '/', prj_code, '/proj4/')
        prj4s <- tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)

        # make sure url got a string
        if (length(prj4s) == 0) {
            warning(paste('Cannot find valid proj4 string for', prj_type, 'with projection code', prj_code))
            # Return wgs84 as default proj4 string
            return(wgs84)
        }
        return(prj4s)
    }
    else {
        return(wgs84)
    }
}

#' Helper function for to assign return Proj4 string.
#'
#' Uses user-input for projection type to identify what
#' type of URL fetch needs to be conducted to retrieve
#' a Proj4 string from http://spatialreference.org
#'
#' @param proj_type Either esri, epsg, or sr-org as string.  These correspond to
#' available reference types hosted by http://spatialreference.org/
#' @param proj The coordinate reference system number or object provided by the user
assign_prj4s <- function(proj_type, proj) {

    # change proj_type to lower case
    if (is.null(proj_type)) {
        pt <- NULL
    }
    else {
        pt <- tolower(c(proj_type))
    }

    # get proj4 string that corresponds to user selection
    if (is.null(pt)) {
        return(get_prj4s(obj = proj))
    }
    else if (pt == 'prj4s_key') {
        return(get_prj4s(prj_type = pt, prj4s_key = proj))
    } else {
        return(get_prj4s(prj_type = pt, prj_code = proj))
    }
}

#' Create sf object from numeric extent.
#'
#' Creates a sf object from numeric extent vector and applies a default WGS84
#' (EPSG:4326) coordinate reference system.
#'
#' @param b_ext Numeric extent [xmin, xmax, ymin, ymax]
#' @param buff_dist Distance in decimal degrees to expand the bounding box by in all directions.
#' @param proj4s Either the proj4 string or EPSG number of the native projection of the bounds
spat_bb <- function(b_ext, buff_dist, proj4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

  # convert bounding box to simple features polygon collection
  geom <- sf::st_sfc(sf::st_polygon(list(rbind(c(b_ext[1], b_ext[3]),
                                               c(b_ext[1], b_ext[4]),
                                               c(b_ext[2], b_ext[4]),
                                               c(b_ext[2], b_ext[3]),
                                               c(b_ext[1], b_ext[3])))))

  # make sf object; a is an id field; 1 is the arbitrary value; assign default WGS84 proj;
  #  transform projection the that of the input mapdata
  bb <- sf::st_sf(a = 1, geometry = geom) %>%
    sf::st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
    sf::st_transform(proj4s)

  # Suppress Warning: In st_buffer.sfc(st_geometry(x), dist, nQuadSegs) :
  #   st_buffer does not correctly buffer longitude/latitude data, dist needs
  #   to be in decimal degrees.
  #   This warning occurs from buffering a feature that is in a geographic
  #   coordinate system (lat/long) rather than a projection one.  This makes the
  #   the buffer not be exact due to the distance being calculated in decimal
  #   degrees rather than meters or kilometers. This is fine with us since we are
  #   simply using buffer as a way of zooming to include or exclude portions of
  #   the bounding extent in this call.

  # buffer if user desires
  if (!is.null(buff_dist)) {
    return(suppressWarnings({sf::st_buffer(bb, buff_dist)}))
  }
  else {
    return(bb)
  }
}

#' Transform the projection of an sf object
#'
#' Recalculates the projection of the input sf object to
#' the user-defined one using a Proj4 string.
#'
#' @param sdf sf object
#' @param prj4s Proj4 string
reproject <- function(sdf, prj4s) {

    # if proj is native do not transform; else transform to user defined coord sys
    if (prj4s == sf::st_crs(sdf)[2]) {
        return(sdf)
    }
    else {
        return(sf::st_transform(sdf, sf::st_crs(prj4s)))
    }
}

#---------------------------------------------------------------------------
# DATA PROCESSING FUNCTIONS
#---------------------------------------------------------------------------

#' Extract a query from a list of queries and filter to desired output.
#'
#' Extract the desired table from the structure produced by
#' \code{\link{parse_mi_output}}.  Optionally, perform some filtering
#' and transformation on the data.  (XXX: We need _way_ more
#' information here.  What gets filtered and transformed, and how does
#' it work?)
#'
#' @param batchq The structure containing the GCAM results (produced
#' by \code{\link{parse_mi_output}}).
#' @param query The name of the table to extract; i.e., the name of
#' one of the queries cointained in the GCAM output.
#' @param scen The name of the scenario.  Partial matches are allowed.
#' @param filters A named vector of filtering criteria in the form
#' \code{c(header1 = value1, header2 = value2,...)}.  Headers are the
#' names of columns in the data frame.  If aggregating data, use the
#' value 'Aggregate'.  (XXX: Needs further explanation!)
#' @param func Operation to apply to the aggregated data.  (XXX: Does
#' this mean that this option is active only when using the
#' 'Aggregate' option above?)
#' @export
process_batch_q <- function(batchq, query, scen, filters, func = sum) {

    .Deprecated('getQuery', 'rgcam',
                'Consider using the rgam package to manage GCAM data.')

    qdata <- as.data.frame(batchq[[query]])

    # Filter for scenario; allow partial lookup
    # Bug: if partial has multiple matches, will return multiple scenarios
    qdata <- qdata[grepl(scen, qdata$scenario), ]

    # Get years and aggregate value if applicable
    years <- grep("(X1)|(X2)", names(qdata), value = T)
    ag <- names(filters[filters[names(filters)] == "Aggregate"])  #Super clunky

    nms <- !(names(qdata) %in% years | names(qdata) %in% ag)

    # Filter to query of interest using filters
    for (name in names(filters)) {
        if (filters[[name]] == "Aggregate") {
            qdata <- stats::aggregate(qdata[years], by = qdata[nms], FUN = func)
            qdata[[ag]] <- "All"
        } else {
            qdata <- qdata[qdata[[name]] == filters[[name]], ]
        }
    }

    return(qdata)
}

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
#' abbreviations.  Only the \code{chn} set (and the \code{usa} set, when it is
#' finally implemented) has both.
#' @param datatable A table of results produced by \code{\link{process_batch_q}}
#' @param lookupfile Name of one of the predefined map sets, OR, if you're using
#' a custom map set, the file containing the region lookup table
#' @param provincefile Name of one of the predefined map sets, OR, if you're
#' using a custom map set, file containing the province lookup table, if
#' applicable.
#' @param drops Name of one of the predefined map sets, OR, if you're using
#' a custom map set, the file containing a list of regions to drop, if
#' applicable.
#' @return Input table modified to include a GCAM ID for reach region.
#' @importFrom utils read.csv
#' @export
add_region_ID <- function(datatable, lookupfile = lut.rgn32, provincefile = NULL, drops = NULL) {
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

    # Differentiate region-Region issue
    if ("Region" %in% names(datatable)) {
        rgn <- "Region"
    } else {
        rgn <- "region"
    }

    finaltable <- merge(datatable, lookuptable, by.x = rgn, by.y = colnames(lookuptable)[1], all.y = TRUE)

    ## Regions that weren't in the original table will show as NA.
    ## Zero them out and give them a sensible unit.
    unit <- finaltable$Units[!is.na(finaltable$Units)][1]  # pick the first available unit value; they should all be the same
    finaltable$Units[is.na(finaltable$Units)] <- unit
    ## set column name for id column
    colnames(finaltable)[ncol(finaltable)] <- "id"

    ## find NA values
    na.vals <- is.na(finaltable)
    na.vals[finaltable$id == 0,] <- FALSE    # exclude non-regions; they should stay NA
    finaltable[na.vals] <- 0
    # finaltable$id <- as.character(finaltable$id)  # other functions used id to be a char

    # Add null vector row to end to account for GCAM region 0
    nullvec <- rep(NA, ncol(finaltable))

    finaltable <- rbind(finaltable, nullvec)
    finaltable[nrow(finaltable), rgn] <- "0"  # region 0 name (should be something more descriptive?)
    finaltable$id[nrow(finaltable)] <- 0

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

    provincetable <- if (is.symbol(provincefile)) {
        get.internal(provincefile, "prov")
    } else {
        read.csv(provincefile, strip.white = T)
    }

    # Differentiate region-Region issue
    if ("Region" %in% names(datatable)) {
        rgn <- "Region"
    } else {
        rgn <- "region"
    }

    datatable$rgn <- as.character(datatable$rgn)
    provincetable$province <- as.character(provincetable$province)
    provincetable$province.name <- as.character(provincetable$province.name)

    datatable$rgn <- ifelse(is.na(provincetable$province.name[match(datatable$rgn, provincetable$province)]),
                            datatable$rgn, provincetable$province.name[match(datatable$rgn, provincetable$province)])

    return(datatable)
}

drop_regions <- function(datatable, drops) {
    ### Drop regions listed in drops file from data frame.
    ### Inputs:
    ###   datatable - a data frame of query from batch query CSV
    ###   drops - string; path to file containing regions to be dropped
    ### Outputs:
    ###   datatable - updated data frame with regions dropped.

    dr <- if (is.symbol(drops)) {
        get.internal(drops, "drop")
    } else {
        read.csv(drops, strip.white = T, header = F)
    }
    dr <- as.character(dr$V1)

    regcols <- grepl("egion", names(datatable))  # Find instances of 'region' or 'Region' columns
    ## ^-- Technically this will also trigger on 'Legion' or 'legion'

    ## XXX: Do the next step with dplyr instead of this convoluted way
    datatable[regcols] <- lapply(datatable[regcols], function(x) replace(x, x %in% dr, NA))  #Replace drop col values with NA

    datatable <- stats::na.omit(datatable)  # Remove rows containing NA

    return(datatable)
}

#---------------------------------------------------------------------------
# MAPPING UTILS
#---------------------------------------------------------------------------
#' Calculate legend breaks (intervals to put labels on legend scale)
#'
#' Given a minimum and maximum value, and number of breaks, calculate
#' evenly-spaced break values.
#'
#' @param maxval Largest value in the scale
#' @param minval Smallest value in the scale
#' @param nbreak Number of break points
#' @param nsig Number of significant digits to display in the legend.
calc.breaks <- function(maxval, minval = 0, nbreak = 5, nsig = 3) {
    step <- (maxval - minval)/(nbreak - 1)
    brk <- seq(minval, maxval, by = step) %>% signif(nsig)
    ## The rounding at the ends may have put the final label off the scale.  Fix
    ## if necessary
    if(brk[1] < minval) {
        pwr <- ceiling(log10(abs(minval)))
        unit <- 10**(-nsig)*10**pwr
        brk[1] <- signif(brk[1] + unit, nsig)
    }
    if(brk[nbreak] > maxval) {
        pwr <- ceiling(log10(abs(maxval)))
        unit <- 10**(-nsig)*10**pwr
        brk[nbreak] <- signif(brk[nbreak]-unit, nsig)
    }
    brk
}

calc.limits.map <- function(mapdata, colname, nbreaks = 5, zero.min = TRUE) {
    ### Calculate legend
    ### Inputs:
    ###   mapdata - data frame of geometry and scenario data
    ###   colname - column name of interest from which to calculate legend intervals
    ###   nbreaks - number of intervals
    ###   zero.min- force minimum value of scale to zero
    ### Outputs:
    ###   breaks - vector of values at which to include legend label

    vals <- as.numeric(mapdata[[colname]])

    max_dat <- max(vals, na.rm = TRUE)

    if (zero.min) {
        if(min(vals, na.rm = TRUE) < 0) {
            ## If there are negative values in the data set, then pinning to
            ## zero means putting the zero point in the middle of the color
            ## bar.
            mag <- max(abs(vals), na.rm = TRUE)
            max_dat <- mag
            min_dat <- -mag
        }
        else {
            min_dat = 0
        }
    }                                   # if(zero.min)
    else {
        min_dat <- min(vals, na.rm = T)
    }

    c(min_dat, max_dat)
}

#-----------------------------------------------------------------
# COLOR PALETTE FUNCTIONS
#-----------------------------------------------------------------

#' Generate a color palette for categorical data.
#'
#' Generate a palette with a specified number of entries.  This
#' function uses a ramp function to extend the palettes from
#' \code{\link{RColorBrewer}} so they can handle a larger number of
#' entries.
#'
#' @param n Number of entries desired for the palette
#' @param pal Name of the palette to base the new palette on. See
#' \code{RColorBrewer} for the palettes available.
#' @param na.val Color to use for the null region
#' @importFrom stats setNames
#' @importFrom grDevices colorRampPalette
#' @export
qualPalette <- function(n = 31, pal = "Set3", na.val = "grey50") {

    colors <- colorRampPalette(RColorBrewer::brewer.pal(8, pal))(n) %>%
        setNames(as.character(1:n))

    colors["0"] <- na.val

    return(colors)
}

#-----------------------------------------------------------------
# MAPPING FUNCTIONS
#-----------------------------------------------------------------
# theme_GCAM: Default GCAM theme function. Can be used with any ggplot2 object.
#   Derives from ggplot2 black and white theme function (theme_bw)
#
# Arguments:
#   base_size: Base font size
#   base_family: Base font type
#   legend: T or F; whether to include a legend with default legend formatting.
#
# Usage: As add-on function to any ggplot2 object.
theme_GCAM <- function(base_size = 11, base_family = "", legend = F) {

    if (legend == F) {
        theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(panel.border = element_rect(color = LINE_COLOR, fill = NA),
                                                                                    panel.background = PANEL_BACKGROUND,
                                                                                    panel.grid.major = PANEL_GRID,
                                                                                    axis.ticks = AXIS_TICKS,
                                                                                    axis.text = AXIS_TEXT,
                                                                                    legend.position = "none")
    } else if (legend == T) {
        theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(panel.border = element_rect(color = LINE_COLOR, fill = NA),
                                                                                    panel.background = PANEL_BACKGROUND,
                                                                                    panel.grid.major = PANEL_GRID,
                                                                                    axis.ticks = AXIS_TICKS,
                                                                                    axis.text = AXIS_TEXT,
                                                                                    legend.key.size = unit(0.75, "cm"),
                                                                                    legend.text = element_text(size = 10),
                                                                                    legend.title = element_text(size = 12, face = "bold"),
                                                                                    legend.position = LEGEND_POSITION,
                                                                                    legend.key = element_rect(color = "black"))
    }
}




##-----------------------------------------------------------------
## MAPS
##-----------------------------------------------------------------

#' Primary GCAM mapping function. Can handle categorical or continuous data.
#'
#' This function produces a map visualization of a data set containing
#' GCAM output data.  The required argument is a data frame of GCAM
#' results by region.  The functions \code{\link{parse_mi_output}} and
#' \code{\link{process_batch_q}} produce suitable data frames.
#'
#' For specifying the projection you can use any Proj4 string.  For convenience,
#' this package defines the following proj4 strings:
#' \itemize{
#'   \item \code{\link{eck3}} - Eckert III
#'   \item \code{\link{wintri}} - Winkel-Tripel
#'   \item \code{\link{robin}} - Robinson
#'   \item \code{\link{na_aea}} - Albers equal area (North America)
#'   \item \code{\link{ch_aea}} - Albers equal area (China)
#' }
#'
#' For orthographic projections, we compute the projection using the
#' \code{\link[ggplot2]{coord_map}} function.  To get this projection
#' pass the \code{\link{ortho}} symbol as the \code{proj} argument.
#' You will then need to pass a vector in the \code{orientation}
#' argument.  We have defined the following fequently used orientation
#' vectors:
#' \itemize{
#'   \item \code{\link{ORIENTATION_AFRICA}} - Africa
#'   \item \code{\link{ORIENTATION_LA}} - Latin America
#'   \item \code{\link{ORIENTATION_SPOLE}} - South Pole
#'   \item \code{\link{ORIENTATION_NPOLE}} - North Pole
#' }
#'
#' The \code{extent} argument gives the bounding box of the area to be
#' plotted.  Its format is \code{c(lon.min, lon.max, lat.min,
#' lat.max)}.  For convenience we have defined the following
#' frequently used map extents:
#' \itemize{
#'    \item \code{\link{EXTENT_WORLD}} - Entire world
#'    \item \code{\link{EXTENT_USA}} - Continental United States
#'    \item \code{\link{EXTENT_CHINA}} - China
#'    \item \code{\link{EXTENT_AFRICA}} - Africa
#'    \item \code{\link{EXTENT_LA}} - Latin America
#' }
#'
#' @param mapdata The data frame containing both geometric data (lat, long, id)
#' and regional metadata.  This is the only mandatory variable. If used alone,
#' will produce the default map.
#' @param col If plotting categorical/contiuous data, the name of the column to
#' plot.  Will automatically determine type of style of plot based on type of
#' data (numeric or character).
#' @param proj Map projection to use in the display map.  This should be a proj4
#' string, except for a few special cases.  There are also symbols defined for
#' some frequently used projections (e.g. \code{\link{robin}} or
#' \code{\link{na_aea}}).
#' @param proj_type Either esri, epsg, or sr-org as string.  These correspond to
#' available reference types hosted by http://spatialreference.org/
#' @param extent Numeric bounds [xmin, xmax, ymin, ymax] to zoom display to
#' @param title Text to be displayed as the plot title
#' @param legend Boolean flag: True = display map legend; False = do not display
#' legend
#' @param colors Vector of colors to use in the color scale.  If NULL, then
#' default color scheme will be used.
#' @param qtitle Text to be displayed as the legend title.
#' @param limits Vector of two values giving the range of the color bar in the
#' legend.  c(min,max)
#' @param colorfcn If plotting categorical data, the function used to generate a
#' colorscheme when colors are not provided (if NULL, use qualPalette).  If
#' \code{colors} is specified, or if the data being plotted is numerical, this
#' argument will be ignored.
#' @param nacolor Color to use for polygons with no data.  The default is
#' gray(0.75), which works well for thematic plots.  For plotting gridded data you
#' probably want something a little more neutral, like gray(0.9).
#' @param gcam_df A data frame generated from rgcam getQuery() that contains data
#' that can be linked to the map geometry data using a unique identifier.
#' @param gcam_key The field name containing a join identifier in the gcam_df data frame.
#' @param mapdata_key The field name containing a join identifier in the mapdata.
#' @param zoom A distance to buffer the bounding box extent by for on-the-fly
#' adjustments needed when fitting area to maps.
#' @param agr_type Aggregate-geometry-relationship type.  Either 'constant' (default),
#' 'aggregate', or 'identity' classified as follows:  [constant] a variable that has a
#' constant value at every location over a spatial extent; examples: soil type, climate zone, land use.
#' [aggregate]	values are summary values (aggregates) over the geometry, e.g. population density,
#' dominant land use.  [identity]	values identify the geometry: they refer to (the whole of)
#' this and only this geometry.
#' See https://cran.r-project.org/web/packages/sf/vignettes/sf1.html#how-attributes-relate-to-geometries
#' for futher explanation.
#' @param ... Other parameters passed on to \code{colorfcn}.
#' @importFrom grDevices gray
#' @examples \dontrun{
#'
#' ## Plot a map of GCAM regions; color it with a palette based on RColorBrewer's 'Set3' palette.
#'   map_32_wo_Taiwan<-rgdal::readOGR(system.file('extdata/rgn32',
#'                                                'GCAM_32_wo_Taiwan_clean.geojson',
#'                                                package = 'gcammaptools'))
#'   map_32_wo_Taiwan.fort<-ggplot2::fortify(map_32_wo_Taiwan, region = 'GCAM_ID')
#'   mp1<-plot_GCAM(map_32_wo_Taiwan.fort, col = 'id', proj = eck3,
#'                  colorfcn = qualPalette)
#'
#'   ## Plot oil consumption by region
#'   tables<-parse_mi_output(fn = system.file('extdata','sample-batch.csv',
#'                           package = 'gcammaptools'))
#'   prim_en<-process_batch_q(tables, 'primary_energy', 'Reference', c(fuel = 'a oil'))
#'   prim_en<-add_region_ID(prim_en, file.path(basedir.viz,
#'                                 system.file('extdata/rgn32', 'lookup.txt',
#'                                             package = 'gcammaptools'),
#'                                 system.file('extdata/rgn32',
#'                                             'drop-regions.txt', package = 'gcammaptools')))
#'   mp2<-plot_GCAM(map_primen, col = 'X2050', colors = c('white', 'red'),
#'                  title = 'Robinson World', qtitle = 'Oil Consumption, 2050', legend = T)
#' }
#' @export
plot_GCAM <- function(mapdata, col = NULL, proj = robin, proj_type = NULL, extent = EXTENT_WORLD,
                      title = "", legend = F, colors = NULL, qtitle = NULL, limits = NULL,
                      colorfcn = NULL, nacolor = gray(0.75), gcam_df = NULL, gcam_key = NULL,
                      mapdata_key = NULL, zoom = NULL, agr_type='constant', ...) {

  # get proj4 string that corresponds to user selection
  p4s <- assign_prj4s(proj_type, proj)

  # create sf obj bounding box from extent and define native proj; apply buffer if needed
  b <- spat_bb(b_ext = extent, buff_dist = zoom, proj4s = sf::st_crs(mapdata))

  # import spatial data; join gcam data; get only features in bounds; transform projection
  m <- import_mapdata(mapdata) %>%
    join_gcam(mapdata_key, gcam_df, gcam_key) %>%
    filter_spatial(bbox = b, extent = extent, col = col, agr_type = agr_type) %>%
    reproject(prj4s = p4s)

  # create object to control map zoom extent
  map_zoom <- zoom_bounds(m, b, extent, p4s)

  # create color scheme object
  color_scheme <- set_color_scheme(m, col = col, qtitle = qtitle, nacolor = nacolor, colors = colors, colorfcn = colorfcn)

  # generate plot object
  mp <- ggplot(m) +
    ggplot2::geom_sf(aes_string(fill = col), color = LINE_COLOR) +
    map_zoom +
    color_scheme +
    ggplot2::ggtitle(title) +
    theme_GCAM(legend = legend) +
    labs(title = title, x = XLAB, y = YLAB)

  return(mp)
}

#' Plot a gridded dataset over a base map
#'
#' This function produces a map visualization of a gridded (i.e., values
#' specified by latitude and longitude) data set.  The data will be plotted over
#' the base map supplied
#'
#' The plot data should be in the form of a table of latitude (lat), longitude
#' (lon), and data values.  The name of the data column is given as an argument
#' to the function, so you can have, for example, latitude and longitude columns
#' followed by columns for time slices.  Columns besides the coordinate and data
#' columns will be ignored.
#'
#' Unlike \code{\link{plot_GCAM}}, we don't try to take the color mapping,
#' legend title, etc. as arguments to this function.  The ggplot2 way of
#' specifying this information is way more flexible. Eventually \code{plot_GCAM}
#' will use this method too.
#'
#' To customize your color mapping, use one of
#' \itemize{
#'   \item \code{\link[ggplot2]{scale_fill_gradient}} : A gradient from one
#' color to another.
#'   \item \code{\link[ggplot2]{scale_fill_gradient2}} : A diverging gradient
#' from one color to another, passing through white in the middle.  You can set
#' the data value that gets assigned to white with the \code{midpoint}
#' argument.
#'  \item \code{\link[ggplot2]{scale_fill_gradientn}} : A smooth gradient
#' between an arbitrary selection of colors.
#' }
#' If you choose to display a legend for the color mapping, you will have to
#' give it a title using the \code{title} argument to any of the above gradient
#' functions.  You have to do this even if you want a legend with no title at
#' all.  Use an empty string in that case.
#'
#' @param plotdata Data frame with the coordinates and values to be plotted.
#' @param col Name of the column holding the data values to plot
#' @param map Base map data.  Default is GCAM 32-region
#' @param alpha Transparency of the grid data layer.  Given as a number between
#' 0 and 1, where 0 is completely transparent and 1 is completely opaque.
#' @inheritParams plot_GCAM
#' @export
plot_GCAM_grid <- function(plotdata, col, map = map.rgn32, proj = robin, extent = EXTENT_WORLD,
                           title = NULL, legend = TRUE, nacolor = gray(0.9),
                           alpha = 0.8, zoom = NULL, proj_type = NULL, qtitle = "") {

    # get proj4 string that corresponds to user selection
    p4s <- assign_prj4s(proj_type, proj)

    # create sf obj bounding box from extent and define native proj; apply buffer if needed
    b <- spat_bb(b_ext = extent, buff_dist = zoom)

    # build sf object from plotdata
    pts <- sf::st_as_sf(plotdata, coords = c("lon", "lat"), crs = sf::st_crs(p4s))[col] %>%
              filter_spatial(bbox = b, extent = extent, col = col) # %>%
              #reproject(prj4s = p4s)

    # get coords and assign to sf object
    coords <- sf::st_coordinates(pts)
    pts['lon'] <- coords[, 1]
    pts['lat'] <- coords[, 2]

    # only get borders intersecting the bounding box
    brdr <- import_mapdata(map) %>%
            filter_spatial(bbox = b, extent = extent, col = 1) #%>%
            #reproject(prj4s = p4s) %>%
            #sf::st_cast('MULTILINESTRING')

    # create object to control map zoom extent
    map_zoom <- zoom_bounds(pts, b, extent, p4s)

    # create color scheme object
    # color_scheme <- set_color_scheme(pts, col = col, qtitle = qtitle, nacolor = nacolor, colors = colors, colorfcn = colorfcn)

    mp <- ggplot(brdr) +
            geom_tile(pts, mapping = aes_string(x = 'lon', y = 'lat',  fill = col), alpha = 0.8) +
            geom_sf() +
            map_zoom # +
            #theme_GCAM(legend = legend) +
            #labs(title = title, x = XLAB, y = YLAB)

    return(mp)
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

#' Designator for the rgn14 map set
#'
#' This symbol will select the rgn14 map set
#' @export
rgn14 <- quote(rgn14)

#' Designator for the rgn32 map set
#'
#' This symbol will select the rgn32 map set
#' @export
rgn32 <- quote(rgn32)

#' Designator for the basin235 map set
#'
#' This symbol will select the basin235 map set
#' @export
basin235 <- quote(basin235)

#' Designator for the chn map set
#'
#' This symbol will select the chn map set
#' @export
chn <- quote(chn)
