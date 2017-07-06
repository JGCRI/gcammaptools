## MAP FUNCTIONS

#' Import ESRI Shapefile as SpatialDataFrame.
#'
#' Creates a SpatialDataFrame from full path string to ESRI Shapefile. User
#' defines which field is supposed to represent the ID for the data.
#'
#' @param shp_path Full path to directory that contains the Shapefile
#' @param layer_name Name of the Shapefile without the extension
#' @param field Name of field in the Shapefile containing the ID of the data
#' @param proj The coordinate reference system number or object provided by the user
load_shp <- function(shp_path, layer_name, field) {

    # read into a SpatialDataFrame
    xmap <- readOGR(shp_path, layer_name)

    # add id column to merge by using gridcode
    xmap$id <- xmap$field

    return(xmap)
}

#' Convert a DataFrame to a SpatialPolygonDataFrame
#'
#' Fill in
#'
#' @param Fill in
df_to_sdf <- function(df, longfield='long', latfield='lat', region='group', pr4s=NULL) {

    # only get needed data
    edf <- df[,c(longfield, latfield, region)]

    # split data frame by region
    s <- split(edf, df[[region]])

    # keep only coordinates as lists
    c <- lapply(s, function(x) { x[[region]] <- NULL; x })

    # create polygons of each list
    p <- sapply(c, Polygon)

    # group into polygons
    ps <- lapply(seq_along(p), function(i) { Polygons(list(p[[i]]), ID=names(c)[i]) })

    # create spatial polygons
    psp <- SpatialPolygons(ps, proj4string=CRS(pr4s))

    # create SpatialPolygonsDataFrame
    sdf <- SpatialPolygonsDataFrame(psp, data.frame(region=unique(edf[[region]]),
                                    row.names= unique(edf[[region]])))

    return(sdf)
}

#' WORKING - Single import function for data frames, spatial data frames,
#' ESRI Shapefiles, or text files.
#'
#' Fill in
#'
#' @param Fill in
import_mapdata <- function(obj, fld) {

    # get object class
    cls <- class(obj)

    # check for file path
    if (is.character(obj)) {

        # get file extension
        extn <- tolower(c(tools::file_ext(obj)))

        # if ESRI Shapefile
        if (extn == 'shp') {

            # create layer name from file basename without extension
            lyr <- basename(tools::file_path_sans_ext(obj))

            # get dirname
            shp_dir <- dirname(obj)

            # load Shapefile
            return(load_shp(shp_path=shp_dir, layer_name=lyr, field=fld))
        }
        # if text file
        else if (extn %in% list('txt', 'csv')) {

            # load text file
            return(load_txt(txt=obj, field=fld))
        }
        # catch unknown
        else {
            return(NULL)
        }
    }
    # check for DataFrame
    else if (is.data.frame(obj)) {

        # convert data frame to spatial polygons data frame
        sdf <- df_to_sdf(obj)

        return(sdf)
    }
    else if (cls %in% list("SpatialPolygonsDataFrame", "SpatialPointsDataFrame",
                          "SpatialLinesDataFrame")) {

        # assign id field to SDF
        obj$id <- obj$field

        return(obj)
    }
    else {
        return(NULL)
    }
}


#' Import text file as a data frame and add id field.
#'
#' Creates a DataFrame from full path string to file. User
#' defines which field is supposed to represent the ID for the data.
#'
#' @param txt Full path to directory that contains the file
#' @param field Name of field in the Shapefile containing the ID of the data
load_txt <- function(txt, field) {

    # read into a DataFrame
    xdat <- read.csv(txt)

    # add id column to merge by using gridcode
    xdat$id <- xdat$field

    return(xdat)
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
#' @param prj4s_key Lookup key string identifying the default projection.
#' @param epsg The EPSG projection code as an integer.
#' @param esri The ESRI projection code as an integer.
#' @param srorg The SR-ORG projection code as an integer.
#' @param lu A key=value list where key is a string and value is the
#' associated proj4 string.
get_prj4s <- function(obj=NULL, prj4s_key=NULL, epsg=NULL, esri=NULL, srorg=NULL, lu=NULL) {

    # default prj4 key: string lookup
    def_lu <- list('us' = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83",
                   'africa' = "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 ",
                   'world' = "+proj=longlat +datum=WGS84 +no_defs",
                   'china' = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

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

    # if prj4 key is provided and none other
    if ((!is.null(prj4s_key)) && (is.null(epsg)) && (is.null(esri)) && (is.null(srorg))) {
        return(lu[[prj4s_key]])
    }
    # if epsg is provided and none other
    else if ((!is.null(epsg)) && (is.null(prj4s_key)) && (is.null(esri)) && (is.null(srorg))) {

        # create url
        url = paste0('http://spatialreference.org/ref/epsg/', epsg, '/proj4/')

        # get prj4s string from EPSG code url
        return(readLines(url, warn=FALSE))
    }
    # if esri is provided and none other
    else if ((!is.null(esri)) && (is.null(prj4s_key)) && (is.null(epsg)) && (is.null(srorg))) {

        # create url
        url = paste0('http://spatialreference.org/ref/esri/', esri, '/proj4/')

        # get prj4s string from ESRI code url
        return(readLines(url, warn=FALSE))
    }
    # if srorg is provided and none other
    else if ((!is.null(srorg)) && (is.null(prj4s_key)) && (is.null(epsg)) && (is.null(esri))) {

        # create url
        url = past0('http://spatialreference.org/reg/sr-org/', srorg, '/proj4/')

        # get prj4s string from ESRI code url
        return(readLines(url, warn=FALSE))
    }
    else {
        return(FALSE)
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
        return(get_prj4s(obj=proj))
    }
    else if (pt == 'prj4s_key') {
        return(get_prj4s(prj4s_key=proj))
    }
    else if (pt == 'epsg') {
        return(get_prj4s(epsg=proj))
    }
    else if (pt == 'esri') {
        return(get_prj4s(esri=proj))
    }
    else if (pt == 'sr-org') {
        return(get_prj4s(srorg=proj))
    }
}

#' Create SpatialPolygonDataFrame from numeric extent.
#'
#' Creates a SpatialPolygonDataFrame from numeric extent vector and
#' applies a default WGS84 (EPSG:4326) coordinate reference
#' system.
#'
#' @param ext Numeric extent [xmin, xmax, ymin, ymax]
spat_bb <- function(b_ext) {

    # convert bounding box to coordinates matrix
    bb_crds <- matrix(c(b_ext[1], b_ext[3],
                        b_ext[1], b_ext[4],
                        b_ext[2], b_ext[4],
                        b_ext[2], b_ext[3],
                        b_ext[1], b_ext[3]),
                      ncol = 2, byrow = TRUE)

    # create id that will associated with the single polygon; arbitrary
    bid <- "a"

    # create a SpatialPolygons object from bounds
    bb_spat <- SpatialPolygons(list(Polygons(list(Polygon(bb_crds)), ID=bid)))

    # create data attributes
    bb_df <- data.frame(value=1, row.names=bid)

    # generate SpatialPolygonsDataFrame
    bb_sdf <- SpatialPolygonsDataFrame(bb_spat, data=bb_df)

    # assign default projection of WGS84 (EPSG:4326)
    proj4string(bb_sdf) <- CRS("+init=EPSG:4326")

    return(bb_sdf)
}

#' Reproject SpatialDataFrame.
#'
#' Recalculates the projection of the input SpatialDataFrame to
#' the user-defined one using a Proj4 string.
#'
#' @param sdf SpatialDataFrame
#' @param prj4s Proj4 string
reproject <- function(sdf, prj4s) {

    # reproject SpatialDataFrame with an assigned coordinate system
    psdf <- spTransform(sdf, CRS(prj4s))

    return(psdf)
}

#' Create graticules (lat lon lines) from bounding box.
#'
#' Creates a SpatialLinesDataFrame from a SpatialPolygonDataFrame bounding box
#' and sets its default projection to the native projection inherited from
#' the input SpatialPolygonDataFrame.
#'
#' @param bbox SpatialPolygonDataFrame bounding box (sdf@bbox)
#' @param prj4s SpatialPolygonDataFrame proj4string (sdf@proj4string)
#' @param longint The longitude length of spacing between lines in the
#' native unit of the bounding box.  Default is 20 for WGS84 degrees.
#' @param latint The latitude length of spacing between lines in the
#' native unit of the bounding box.  Default is 30 for WGS84 degrees.
gen_grat <- function(bbox=NULL, prj4s=NULL, longint=20, latint=30) {

    # create lat and lon sequences spaced by interval
    lons = seq(bbox[1], bbox[3], by = longint)
    lats = seq(bbox[2], bbox[4], by = latint)

    # generate graticule SpatialLinesDataFrame
    return(graticule::graticule(lons, lats, xlim=range(lons), ylim=range(lats)))
}

#' Harmonize coordinate reference system of data sources.
#'
#' Reprojects the map, bounding box, and graticule SpatialDataFrames
#' into a common, user-defined coordinate reference system.
#'
#' @param map_sdf SpatialPolygonDataFrame of the map data
#' @param bb_sdf SpatialPolygonDataFrame of the bounding box
#' @param grat_sdf SpatialLinesDataFrame of the graticules
#' @param prj4s Proj4 string defined by the user that identifies
#' the target coordinate reference system
harmonize_proj <- function(map_sdf, bb_sdf, grat_sdf, prj4s) {

    m_sdf <- reproject(sdf=map_sdf, prj4s=prj4s)
    b_sdf <- reproject(sdf=bb_sdf, prj4s=prj4s)
    g_sdf <- reproject(sdf=grat_sdf, prj4s=prj4s)

    return(list(m_sdf, b_sdf, g_sdf))
}

#' Helper function to create DataFrame from SpatialDataFrame.
#'
#' Creates and forifies a SpatialDataFrame to a DataFrame for use
#' in ggplot.
#'
#' @param sdf SpatialDataFrame
prep_df <- function(sdf) {

    # create a unique ID to conduct the join
    sdf$id = rownames(as.data.frame(sdf))

    # split out shapes
    sdf.shps <- ggplot2::fortify(sdf, region="id")

    # add data attributes
    sdf.df <- merge(sdf.shps, sdf, by="id", type='left') # add the attributes back

    return(sdf.df)
}

#' Prepare data for ggalt plotting.
#'
#' Creates and forifies all map data SpatialDataFrames to a DataFrames
#' for use in ggplot.
#'
#' @param harm_list List of SpatialDataFrames [mapdata, boundingbox, graticules]
fortify_all <- function(harm_list) {

    # create fortified data frames from input SpatialDataFrames for all data
    m <- prep_df(harm_list[1])
    b <- prep_df(harm_list[2])
    g <- prep_df(harm_list[3])

    return(list(m, b, g))
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
#' \code{c(header1=value1, header2=value2,...)}.  Headers are the
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
            qdata <- aggregate(qdata[years], by = qdata[nms], FUN = func)
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
#' @export
addRegionID <- function(datatable, lookupfile = lut.rgn32, provincefile = NULL, drops = NULL) {
    if (!is.null(provincefile)) {
        datatable <- translateProvince(datatable, provincefile)
    }

    if (!is.null(drops)) {
        datatable <- dropRegions(datatable, drops)
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
    na.vals[finaltable$id==0,] <- FALSE    # exclude non-regions; they should stay NA
    finaltable[na.vals] <- 0
    finaltable$id <- as.character(finaltable$id)  # other functions are expecting id to be a char

    # Add null vector row to end to account for GCAM region 0
    nullvec <- rep(NA, ncol(finaltable))

    finaltable <- rbind(finaltable, nullvec)
    finaltable[nrow(finaltable), rgn] <- "0"  # region 0 name (should be something more descriptive?)
    finaltable$id[nrow(finaltable)] <- "0"

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
translateProvince <- function(datatable, provincefile) {

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

dropRegions <- function(datatable, drops) {
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

    datatable <- na.omit(datatable)  # Remove rows containing NA

    return(datatable)
}

#---------------------------------------------------------------------------
# MAPPING UTILS
#---------------------------------------------------------------------------
get_bbox_polys <- function(dataset, bbox = EXTENT_WORLD) {
    ### Modifies map data to include only polygons that lie partially within
    ### bounding box.
    ### Inputs:
    ###   dataset - data frame of map geometry
    ###   bbox - numeric vector (long_min, long_max, lat_min, lat_max)
    ### Outputs:
    ###   newdata - data with only polygons that are at least partially in
    ###           bounding box

    dplyr::filter(dataset, in_range(long, bbox[1], bbox[2]), in_range(lat, bbox[3], bbox[4]))
}

## in_range - Test a vector to see which values are in the interval [a,b]
## params: x:  vector to test
##       a,b:  interval bounds. can be int or numeric; a<=b
in_range <- function(x, a, b) {
    x >= a & x <= b
}

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
        if(min(vals, na.rm=TRUE) < 0) {
            ## If there are negative values in the data set, then pinning to
            ## zero means putting the zero point in the middle of the color
            ## bar.
            mag <- max(abs(vals), na.rm=TRUE)
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
#' @export
qualPalette <- function(n = 31, pal = "Set3", na.val = "grey50") {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(8, pal))(n)
    colors <- setNames(colors, as.character(1:n))
    colors["0"] <- na.val

    return(colors)
}


#-----------------------------------------------------------------
# MAPPING FUNCTIONS
#-----------------------------------------------------------------
# coord_GCAM: This function unifies the ggplot2 and ggalt coordinate systems to make use
#   of both of them, depending on the projection needed. Can be used with any ggplot2 map object.
#   If projection is orthographic, will use ggplot2 coord_map functionality. If projection is not
#   orthographic, will use ggalt coord_proj functionality.
#
# Arguments
#   proj - the projection. proj4 string or pre-defined variable in diag_header.R
#   orientation - Use if using orthographic projection
#   extent - Vector of lat/lon limits c(lat0,lat1,lon0,lon1)
#   parameters - additional parameters corresponding to coord_map ggplot2 function
#   inverse ?
#   degrees - Units for lat/longitude ?
#   ellps.default - default ellipse to use with projection ?
#
# Usage: as add-on function to a ggplot2 object. Example:
#  ggplot()+
#   geom_polygon(data, aes(x,y,group))+
#   coord_GCAM(proj)
coord_GCAM <- function(proj = NULL, orientation = NULL, extent = NULL, ..., parameters = NULL,
    inverse = FALSE, degrees = TRUE, ellps.default = "sphere") {

    if (is.null(proj)) {
        # Default proj4 pstring for default GCAM projection (Robinson)
        proj <- paste0(c("+proj=robin +lon_0=0 +x_0=0 +y_0=0", "+ellps=WGS84 +datum=WGS84 +units=m +nodefs"),
            collapse = " ")
    }

    if (is.null(parameters)) {
        params <- list(...)
    } else {
        params <- parameters
    }

    # Default extent is EXTENT_WORLD (-180,180,-90,90)
    if (is.null(extent)) {
        xlim <- c(-180, 180)
        ylim <- c(-90, 90)
    } else {
        xlim <- c(extent[1], extent[2])
        ylim <- c(extent[3], extent[4])
    }

    # Use ggproto object defined in ggplot2 package if using orthographic map projection
    if (grepl("ortho", proj)) {
        ggproto(NULL, ggplot2::CoordMap, projection = proj, orientation = orientation, limits = list(x = xlim,
            y = ylim), params = params)
    } else {
        # Otherwise use ggproto object defined in ggalt package for default GCAM projections
        ggproto(NULL, ggalt::CoordProj, proj = proj, inverse = inverse, ellps.default = ellps.default,
            degrees = degrees, limits = list(x = xlim, y = ylim), params = list())
    }

}


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
        theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(panel.border = element_rect(color = LINE_COLOR,
            fill = NA), panel.background = PANEL_BACKGROUND, panel.grid = PANEL_GRID, axis.ticks = AXIS_TICKS,
            axis.text = AXIS_TEXT, legend.position = "none")
    } else if (legend == T) {
        theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(panel.border = element_rect(color = LINE_COLOR,
            fill = NA), panel.background = PANEL_BACKGROUND, panel.grid = PANEL_GRID, axis.ticks = AXIS_TICKS,
            axis.text = AXIS_TEXT, legend.key.size = unit(0.75, "cm"), legend.text = element_text(size = 10),
            legend.title = element_text(size = 12, face = "bold"), legend.position = LEGEND_POSITION,
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
#' For specifying the projection you can use any Proj4 string.
#' Projections specified this way are computed using
#' \code{\link[ggalt]{coord_proj}}.  For convenience, this package
#' defines the following proj4 strings:
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
#' @param ext Numeric bounds [xmin, xmax, ymin, ymax] to zoom display to
#' @param orientation The orientation vector.  This is only needed for
#' projections that don't use proj4.  Projections using proj4 encode this
#' information in their proj4 string.
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
#' @param altdata A DataFrame, SpatialDataFrame, or full path to an ESRI Shapefile or CSV
#' that contains data that can be linked to the map geometry data using a unique identifier.
#' @param altid The field name containing a unique identifier in the altdata data set.
#' @param mapid The field name containing a unique identifier in the mapdata data set.
#' @param ... Other parameters passed on to \code{colorfcn}.
#' @examples \dontrun{
#'
#' ## Plot a map of GCAM regions; color it with a palette based on RColorBrewer's 'Set3' palette.
#'   map_32_wo_Taiwan<-rgdal::readOGR(system.file('extdata/rgn32',
#'                                                'GCAM_32_wo_Taiwan_clean.geojson',
#'                                                package='gcammaptools'))
#'   map_32_wo_Taiwan.fort<-ggplot2::fortify(map_32_wo_Taiwan, region='GCAM_ID')
#'   mp1<-plot_GCAM(map_32_wo_Taiwan.fort, col = 'id', proj = eck3,
#'                  colorfcn=qualPalette)
#'
#'   ## Plot oil consumption by region
#'   tables<-parse_mi_output(fn = system.file('extdata','sample-batch.csv',
#'                           package='gcammaptools'))
#'   prim_en<-process_batch_q(tables, 'primary_energy', 'Reference', c(fuel='a oil'))
#'   prim_en<-addRegionID(prim_en, file.path(basedir.viz,
#'                                 system.file('extdata/rgn32', 'lookup.txt',
#'                                             package='gcammaptools'),
#'                                 system.file('extdata/rgn32',
#'                                             'drop-regions.txt', package='gcammaptools')))
#'   mp2<-plot_GCAM(map_primen, col = 'X2050', colors = c('white', 'red'),
#'                  title='Robinson World', qtitle='Oil Consumption, 2050', legend=T)
#' }
#' @export
plot_GCAM <- function(mapdata, col=NULL, proj=robin, proj_type=NULL, extent=EXTENT_WORLD,
                      orientation=NULL, title=NULL, legend=F, colors=NULL, qtitle=NULL,
                      limits=NULL, colorfcn=NULL, nacolor=gray(0.75), altdata=NULL, altid=NULL,
                      mapid=NULL, ...) {

    # mappolys <- get_bbox_polys(dataset = mapdata)

    # WORKING import data function to import all types for main mapdata SDF


    # create SpatialPolygonDataFrame from numeric bounds and project as WGS84
    b_sdf  <- spat_bb(b_ext=extent)

    # create graticule SpatialLinesDataFrame and project as WGS84
    g_sdf <- gen_grat(bbox=b_sdf@bbox, prj4s=b_sdf@proj4string)

    # get proj4 string that corresponds to user selection
    p4s <- assign_prj4s(proj_type=proj_type, proj=proj)

    # reproject map data, bounding box, and graticules to user-defined projection
    # h <- harmonize_proj(map_sdf=mapdata, bb_sdf=b_sdf, grat_sdf=g_sdf, prj4s=p4s)
    m <- prep_df(reproject(sdf=mapdata, prj4s=p4s))
    b <- reproject(sdf=b_sdf, prj4s=p4s)
    g <- prep_df(reproject(sdf=g_sdf, prj4s=p4s))

    # create ggplot
    mp <- ggplot() +
        geom_path(data=g, aes(long, lat, group=group), color=LINE_GRAT) +
        geom_polygon(data=m, aes_string("long", "lat", group="group", fill=col), color=LINE_COLOR) +
        coord_fixed(xlim=c(b@bbox[1], b@bbox[3]), ylim=c(b@bbox[2], b@bbox[4]))

    # If a column name is specified, add a color gradient or categorical colors
    if (!is.null(col)) {

        if (is.numeric(m[[col]])) {
            # Instructions for color gradient Calculate legend label increments ('breaks')
            if (is.null(limits)) {
                limits <- calc.limits.map(m, col)
            }
            breaks <- calc.breaks(limits[2], limits[1])

            # Use default colors if none specified
            if (is.null(colors))
                colors <- DEFAULT_CHOROPLETH

            # Add color scale to map
            mp <- mp + scale_fill_gradientn(name = qtitle, colors = colors, values = NULL, guide = GUIDE,
                space = SPACE, na.value = nacolor, breaks = breaks, limits = limits, labels = breaks)

        } else {
            # Instructions for categorical map Use default color scheme and color function if none
            # specified
            if (is.null(colors)) {
                if (is.null(colorfcn)) {
                  colorfcn <- qualPalette
                }
                colors <- colorfcn(n = length(unique(m[[col]])), ...)
            }

            # Add color scale to map
            mp <- mp + scale_fill_manual(values = colors, name = qtitle)
        }
    } else {
          ## If no data is being plotted, use default color scale
          mp <- mp + geom_polygon(data = m, aes_string("long", "lat",
                                  group = "group"), fill = nacolor, color =
                                      LINE_COLOR)
    }

    # Project map and add theme and labels
    mp <- mp +
            theme_GCAM(legend=legend) +
            labs(title = title, x = XLAB, y = YLAB)


    # mp <- mp + coord_GCAM(proj = p4s,
    #                       orientation = orientation,
    #                       extent = extent) +
    #                         theme_GCAM(legend = legend) +
    #                         labs(title = title, x = XLAB, y = YLAB)

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
plot_GCAM_grid <- function(plotdata, col, map = map.rgn32, proj = robin, extent
                           = EXTENT_WORLD, orientation = NULL, title = NULL,
                           legend = TRUE, nacolor = gray(0.9),
                           alpha=0.8)
{
    ## start by plotting the base map
    plt <- plot_GCAM(map, proj = proj, extent = extent,
                     orientation = orientation, title = title, legend = legend,
                     nacolor = nacolor)
    ## add the raster layer and return
    plt + geom_tile(data=plotdata, mapping=aes_string(x='lon', y='lat',
                                   fill=col), alpha=alpha)
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
