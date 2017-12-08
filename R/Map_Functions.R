## MAP FUNCTIONS

#' Build the coordinate zooming bounds for ggplot object.
#'
#' Either take the bounding box or the map coordinates depending on extent.
#'
#' @param mapdata The sf object containing the spatial data.
#' @param bbox Bounding box.
#' @param p4s Proj4 string set by user
#' @param extent Extent provided by the user. If NULL defaults to the bounds of
#'   the data.
zoom_bounds <- function(mapdata, bbox, extent, p4s) {

    # if the extent given is NULL
    if (isTRUE(all.equal(extent, EXTENT_WORLD))) {

      # use map bounds instead of bounding box for zoom
      bx <- sf::st_bbox(mapdata)

      return(ggplot2::coord_sf(xlim = c(bx[1], bx[3]),
                               ylim = c(bx[2], bx[4])))
    }
    else {

      bx <- reproject(bbox, prj4s = sf::st_crs(p4s)[[2]]) %>%
        sf::st_bbox()

      return(ggplot2::coord_sf(crs = p4s, datum = sf::st_crs(p4s),
                               xlim = c(bx[1], bx[3]),
                               ylim = c(bx[2], bx[4]), expand = TRUE))
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
filter_spatial <- function(mapdata, bbox, extent, col, agr_type='constant', topo=sf::st_intersects) {
  # set NULL column to index
  clm <- if (is.null(col)) 1 else col

  # set attribute-geometry-relationship for input mapdata column and bounding box feature attribute
  sf::st_agr(mapdata) <- agr_type
  sf::st_agr(bbox) <- agr_type

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
    return(suppressMessages({sf::st_intersection(bbox, mapdata)}))
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
#' an \code{rgcam} query.
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
                   dplyr::select(-dplyr::one_of('pkey', mapdata_key)) %>%
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
#' @param obj Input full path string or object.
#' @param fld Field name to use as identifier.
#' @param prj4s Proj4 string for projection (default WGS84).
#' @return An sf object representation of the map data.
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

#' Reduce number of polygons and size of polygons for map Shapefiles
#'
#' Takes a sf object representation of a map and simplifys it by removing
#' polygons that are under a certain size.
#'
#' ** NOTE: This function adds two polygons to the edges of the map to prevent
#'          the removal of polygons near the edges redefining the map bounds.
#'
#' @param mapdata sf object containing polygons or multipolygons to simplify.
#' @param min_area Minimum area of polygons to keep.
#' @param degree_tolerance Tolerance parameter for simplifying polygons.
#' @return The simplified sf object.
#' @export
simplify_mapdata <- function(mapdata, min_area = 2.5, degree_tolerance = 0.5) {

  . <- NULL                             # silence package notes for NSE.

  if ("MULTIPOLYGON" %in% sf::st_geometry_type(mapdata))
    mapdata <- sf::st_cast(mapdata, "POLYGON", warn = FALSE)

  # filter out all polygons in the data under the minimum area
  areafilter <- sapply(sf::st_geometry(mapdata), sf::st_area) > min_area
  filtermap <- which(areafilter) %>%
    mapdata[.,]

  filtermap <- suppressWarnings({sf::st_simplify(filtermap, preserveTopology=TRUE, dTolerance=degree_tolerance)})

  # if nothing was filtered just return original map
  if (sf::st_geometry(filtermap) %>% length == sf::st_geometry(mapdata) %>% length)
    return(mapdata)

  # When removing polygons we might be shifting the bounds of the map, which
  # would make it off-center when plotting. To account for this, we put tiny
  # polygons on the edges.
  xmin <- sf::st_bbox(mapdata)[1] %>% round
  xmax <- sf::st_bbox(mapdata)[3] %>% round
  height <- 0.0001 # small enough so it's not visible on plot

  left_edge <- matrix(c(xmin, 0, xmin, height, xmin - height, 0, xmin, 0),
                      byrow = TRUE, ncol = 2) %>%
    list() %>%
    sf::st_polygon()

  right_edge <- matrix(c(xmax, 0, xmax, height, xmax + height, 0, xmax, 0),
                      byrow = TRUE, ncol = 2) %>%
    list() %>%
    sf::st_polygon()

  # create geometry with the two edges
  edges <- sf::st_sfc(left_edge, right_edge, crs = sf::st_crs(mapdata))

  # data frame with same names as original map, so that the two can combine
  borders <- data.frame(matrix(ncol = length(names(mapdata)), nrow = 2)) %>%
      magrittr::set_names(names(mapdata))

  # extra polygons should be GCAM region 0
  if ('region_id' %in% names(borders)) borders$region_id <- c(0, 0)

  # convert to sf object and add new border polygons
  sf::st_geometry(borders) <- edges

  # add new polygons to filtered map and return
  return(rbind(borders, filtermap))
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
#' @param buff_dist Distance in decimal degrees to expand the bounding box by in
#'   all directions.
#' @param proj4s Either the proj4 string or EPSG number of the native projection
#'   of the bounds
spat_bb <- function(b_ext, buff_dist, proj4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

  # convert bounding box to simple features polygon collection
  geom <- sf::st_sfc(sf::st_polygon(list(rbind(c(b_ext[1], b_ext[3]),
                                               c(b_ext[1], b_ext[4]),
                                               c(b_ext[2], b_ext[4]),
                                               c(b_ext[2], b_ext[3]),
                                               c(b_ext[1], b_ext[3])))))

  # make sf object; a is an id field; 1 is the arbitrary value; assign default WGS84 proj;
  # transform projection to that of the input mapdata
  bb <- sf::st_sf(a = 1, geometry = geom) %>%
    sf::st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
    sf::st_transform(proj4s)

  # Suppress Warning and message: In st_buffer.sfc(st_geometry(x), dist,
  #   nQuadSegs) : st_buffer does not correctly buffer longitude/latitude data,
  #   dist needs to be in decimal degrees.
  #   This warning occurs from buffering a feature that is in a geographic
  #   coordinate system (lat/long) rather than a projection one.  This makes the
  #   the buffer not be exact due to the distance being calculated in decimal
  #   degrees rather than meters or kilometers. This is fine with us since we are
  #   simply using buffer as a way of zooming to include or exclude portions of
  #   the bounding extent in this call.

  # buffer if user desires
  if (!is.null(buff_dist)) {
    return(suppressWarnings({suppressMessages({sf::st_buffer(bb, buff_dist)})}))
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
    if (sf::st_crs(sdf)[[2]] == prj4s) {
        return(sdf)
    }
    else {
        return(sf::st_transform(sdf, prj4s))
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
#' @param datatable A table of results produced by \code{\link{getQuery}}
#' @param lookupfile Name of one of the predefined map sets, OR, if you're using
#' a custom map set, the file containing the region lookup table
#' @param provincefile Name of one of the predefined map sets, OR, if you're
#' using a custom map set, file containing the province lookup table, if
#' applicable.
#' @param drops Name of one of the predefined map sets, OR, if you're using
#' a custom map set, the file containing a list of regions to drop, if
#' applicable.
#' @param disaggregate A column of \code{datatable} used to disaggregate regions
#' that are not specified in the original data.
#' @return Input table modified to include a GCAM ID for reach region.
#' @importFrom utils read.csv
#' @export
add_region_ID <- function(datatable, lookupfile = rgn32, provincefile = NULL, drops = NULL, disaggregate = NULL) {
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

    # Don't allow "Region"; replace with "region"
    rgnidx <- which(names(datatable) == "Region")
    names(datatable)[rgnidx] <- "region"

    # Add column containing region id to end of datatable
    finaltable <- dplyr::full_join(datatable, lookuptable, by = "region")

    # Set column name for id column
    names(finaltable)[ncol(finaltable)] <- "id"

    # Add null vector row to end to account for GCAM region 0
    finaltable <- rbind(finaltable, rep(NA, ncol(finaltable)))
    finaltable[nrow(finaltable), 'region'] <- "#N/A"
    finaltable[nrow(finaltable), 'id'] <- 0

    # Regions that weren't in the original table will have NA values for all
    # data except for the region and id columns. If the user specified a
    # disaggregate column, split the new regions over all factors from that
    # column.
    if (!is.null(disaggregate)) {
        # Get values to disaggregate over
        factors <- finaltable[[disaggregate]] %>% unique %>% na.omit

        # Get rows that need disaggregating (the ones that have NAs in that col)
        na.rgns <- finaltable[which(is.na(finaltable[[disaggregate]])), ]

        # Build disaggregated dataframe and add to end of original table
        disaggr <- na.rgns[rep(seq_len(nrow(na.rgns)), each = length(factors)), ]
        disaggr[[disaggregate]] <- rep(factors, nrow(na.rgns))
        finaltable <- rbind(dplyr::setdiff(finaltable, na.rgns), disaggr)
    }

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

#' Drop regions listed in drops file from data frame.
#'
#' @param datatable A data frame of query from batch query CSV.
#' @param drops String; path to file containing regions to be dropped
#' @return An updated data frame with regions dropped.
drop_regions <- function(datatable, drops) {

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



#-----------------------------------------------------------------
# MAPPING FUNCTIONS
#-----------------------------------------------------------------

#' Default GCAM theme function
#'
#' An add-on function to any ggplot2 object. Derives from ggplot2 black and
#' white theme function (theme_bw).
#'
#' @param base_size Base font size
#' @param base_family Base font type
#' @param legend Boolean; whether to include a legend with default legend
#'   formatting.
theme_GCAM <- function(base_size = 11, base_family = "", legend = FALSE) {

    if (legend) {
        theme_bw(base_size = base_size, base_family = base_family) %+replace%
            theme(panel.border = element_rect(color = LINE_COLOR, fill = NA),
                  panel.background = PANEL_BACKGROUND,
                  panel.grid.major = PANEL_GRID,
                  axis.ticks = AXIS_TICKS,
                  axis.text = AXIS_TEXT,
                  legend.key.size = unit(0.75, "cm"),
                  legend.text = element_text(size = 10),
                  legend.title = element_text(size = 12, face = "bold"),
                  legend.position = LEGEND_POSITION,
                  legend.key = element_rect(color = "black"))

    } else {
        theme_bw(base_size = base_size, base_family = base_family) %+replace%
            theme(panel.border = element_rect(color = LINE_COLOR, fill = NA),
                  panel.background = PANEL_BACKGROUND,
                  panel.grid.major = PANEL_GRID,
                  axis.ticks = AXIS_TICKS,
                  axis.text = AXIS_TEXT,
                  legend.position = "none")
    }
}



##-----------------------------------------------------------------
## MAPS
##-----------------------------------------------------------------

#' Primary GCAM mapping function. Can handle categorical or continuous data.
#'
#' This function produces a map visualization of a data set containing GCAM
#' output data.  The required argument is a data frame of GCAM results by
#' region.  The function \code{\link[rgcam]{getQuery}} produces suitable data
#' frames.
#'
#' We don't try to take the color mapping, legend title, etc. as arguments to
#' this function.  The ggplot2 way of specifying this information is way more
#' flexible. To customize your color mapping, use one of \itemize{ \item
#' \code{\link[ggplot2]{scale_fill_manual}} : A list of colors to map to
#' categorical data. \item \code{\link[ggplot2]{scale_fill_gradient}} : A
#' gradient from one color to another. \item
#' \code{\link[ggplot2]{scale_fill_gradient2}} : A diverging gradient from one
#' color to another, passing through white in the middle.  You can set the data
#' value that gets assigned to white with the \code{midpoint} argument. \item
#' \code{\link[ggplot2]{scale_fill_gradientn}} : A smooth gradient between an
#' arbitrary selection of colors. } If you choose to display a legend for the
#' color mapping, you will have to give it a title using the \code{title}
#' argument to any of the above gradient functions.  You have to do this even if
#' you want a legend with no title at all.  Use an empty string in that case.
#'
#' For specifying the projection you can use any Proj4 string.  For convenience,
#' this package defines the following proj4 strings: \itemize{ \item
#' \code{\link{wgs84}} - WGS84 (EPSG:4326) \item \code{\link{eck3}} - Eckert III
#' \item \code{\link{robin}} - Robinson \item \code{\link{na_aea}} - Albers
#' equal area (North America) \item \code{\link{ch_aea}} - Albers equal area
#' (China) \item \code{\link{af_ortho}} - Orthographic projection over Africa }
#'
#'
#' The \code{extent} argument gives the bounding box of the area to be plotted.
#' Its format is \code{c(lon.min, lon.max, lat.min, lat.max)}.  For convenience
#' we have defined the following frequently used map extents: \itemize{ \item
#' \code{\link{EXTENT_WORLD}} - Entire world \item \code{\link{EXTENT_USA}} -
#' Continental United States \item \code{\link{EXTENT_CHINA}} - China \item
#' \code{\link{EXTENT_AFRICA}} - Africa \item \code{\link{EXTENT_LA}} - Latin
#' America }
#'
#' @param mapdata The data frame containing both geometric data (lat, long, id)
#'   and regional metadata.  This is the only mandatory variable. If used alone,
#'   will produce the default map.
#' @param col If plotting categorical/contiuous data, the name of the column to
#'   plot.  Will automatically determine type of style of plot based on type of
#'   data (numeric or character).
#' @param proj Map projection to use in the display map.  This should be a proj4
#'   string, except for a few special cases.  There are also symbols defined for
#'   some frequently used projections (e.g. \code{\link{robin}} or
#'   \code{\link{na_aea}}).
#' @param proj_type Either esri, epsg, or sr-org as string.  These correspond to
#'   available reference types hosted by \url{http://spatialreference.org/}.
#' @param extent Numeric bounds [xmin, xmax, ymin, ymax] to zoom display to.
#' @param title Text to be displayed as the plot title.
#' @param legend Boolean flag: True = display map legend; False = do not display
#'   legend.
#' @param gcam_df A data frame generated from the \code{rgcam} function
#'   \code{\link[rgcam]{getQuery}}.  Also accepts other data frames that contain
#'   data that can be linked to the map geometry data using a unique identifier.
#' @param gcam_key The field name containing a join identifier in the gcam_df
#'   data frame.
#' @param mapdata_key The field name containing a join identifier in the
#'   mapdata.
#' @param zoom A distance to buffer the bounding box extent by for on-the-fly
#'   adjustments needed when fitting area to maps.
#' @param agr_type Aggregate-geometry-relationship type.  Either 'constant'
#'   (default), 'aggregate', or 'identity' classified as follows:  [constant] a
#'   variable that has a constant value at every location over a spatial extent;
#'   examples: soil type, climate zone, land use. [aggregate]	values are summary
#'   values (aggregates) over the geometry, e.g. population density, dominant
#'   land use.  [identity]	values identify the geometry: they refer to (the
#'   whole of) this and only this geometry. See the
#'   \href{https://cran.r-project.org/web/packages/sf/vignettes/sf1.html#how-attributes-relate-to-geometries}{sf
#'   vignette} for futher explanation.
#' @importFrom grDevices gray
#' @examples \dontrun{
#'
#' ## Plot a map of GCAM regions; color it with the default theme palette.
#' plot_GCAM(map.rgn32.simple, col = 'region_name', proj = eck3) +
#'     ggplot2::scale_fill_manual(values = gcam32_colors, na.value=gray(0.75))
#'
#' ## Plot refined liquids production by region for the year 2050
#' prj <- loadProject(system.file('sample-gcam-data',
#'                                'gcam-longform-sample.dat',
#'                                package='gcammaptools'))
#' ref_liquids <- rgcam::getQuery(prj, 'Refined liquids production by region', 'Reference')
#' ref_liquids <- add_region_ID(ref_liquids, lookupfile=rgn32, drops=rgn32)
#' ref_liquids <- dplyr::filter(ref_liquids, year==2050)
#' plot_GCAM(map.rgn32.simple, col='value', proj=robin, title="Robinson World",
#'           legend=T, gcam_df=co2, gcam_key='id', mapdata_key="region_id") +
#'    ggplot2::scale_fill_gradientn(colors = c("white", "red"),
#'                                  na.value = gray(0.75),
#'                                  name="CO2 Emissions (MTC)")
#' }
#' @export
plot_GCAM <- function(mapdata, col = NULL, proj = robin, proj_type = NULL,
                      extent = EXTENT_WORLD, title = "", legend = F,
                      gcam_df = NULL, gcam_key = "id", mapdata_key = "region_id",
                      zoom = NULL, agr_type='constant') {

  # get proj4 string that corresponds to user selection
  p4s <- assign_prj4s(proj_type, proj)

  m <- import_mapdata(mapdata)

  # create sf obj bounding box from extent and define native proj; apply buffer if needed
  b <- spat_bb(b_ext = extent, buff_dist = zoom, proj4s = sf::st_crs(m))

    # import spatial data; join gcam data; get only features in bounds; transform projection
  m <- join_gcam(m, mapdata_key, gcam_df, gcam_key) %>%
    filter_spatial(bbox = b, extent = extent, col = col, agr_type = agr_type) %>%
    reproject(prj4s = p4s)

  # create object to control map zoom extent
  map_zoom <- zoom_bounds(m, b, extent, p4s)

  # generate plot object
  mp <- ggplot() +
    ggplot2::geom_sf(data = m, aes_string(fill = col), color = LINE_COLOR) +
    map_zoom +
    ggplot2::ggtitle(title) +
    theme_GCAM(legend = legend)

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
#' @param plotdata Data frame with the coordinates and values to be plotted.
#'   Must contain 'lat' and 'lon' columns.
#' @param col Name of the column holding the data values to plot
#' @param map Base map data.  Default is GCAM 32-region
#' @param alpha Transparency of the grid data layer.  Given as a number between
#'   0 and 1, where 0 is completely transparent and 1 is completely opaque.
#' @param ... Other parameters passed on to \code{plot_GCAM}.
#' @inheritParams plot_GCAM
#' @export
plot_GCAM_grid <- function(plotdata, col, map = map.rgn32, proj = robin,
                           proj_type = NULL, legend = F, alpha = 0.8, ...) {

    map.rgn32 <- gcammaptools::map.rgn32 # Silence package notes

    # make sure data has valid gridded data
    if (!('lon' %in% names(plotdata) && 'lat' %in% names(plotdata)))
        stop("gridded data must have a 'lon' column and a 'lat' column")

    # if we are in a projected crs
    if (!sf::st_is_longlat(proj)) {
        p4s <- assign_prj4s(proj_type, proj)

        # get raster extent
        e = raster::extent(c(range(plotdata$lon), range(plotdata$lat)))

        # set the number of rows in the raster equal to the number of unique
        # latitudes in the original data
        nr <- plotdata['lat'] %>% unique() %>% nrow
        nc <- plotdata['lon'] %>% unique() %>% nrow

        # build a raster that fits the data
        plotraster <- raster::raster(nrows = nr, ncols = nc, ext = e, crs = wgs84)

        points <- plotdata[ , c('lon', 'lat')]
        values <- plotdata[[col]]

        # 1. Add data values to raster cells
        # 2. Reproject the raster into the user-defined crs
        # 3. Turn the raster back into points in the new crs
        # 4. Convert back to a data.frame with the correct names so that
        #    geom_raster can plot it
        plotdata <- raster::rasterize(points, plotraster, field = values, fun = mean) %>%
                    raster::projectRaster(crs=p4s, over=TRUE) %>%
                    raster::rasterToPoints() %>%
                    data.frame() %>%
                    magrittr::set_names(c("lon", "lat", col))

    }

    # get the base map using plot_GCAM
    mp <- plot_GCAM(map, proj = proj, proj_type = proj_type, legend = legend, ...)

    # add the gridded data to the base map
    grid <- geom_raster(data = plotdata,
                        mapping = aes_string(x='lon', y='lat', fill = col),
                        alpha = alpha)

    # remove x and y axis labels and give scale a title
    lbls <- labs(x = XLAB, y = YLAB, fill=col)

    return(mp + grid + lbls)
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
