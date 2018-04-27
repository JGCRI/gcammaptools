# utils.R
#
# Contains general helper functions for the package


#' Retrieve proj4 projection string.
#'
#' Provides a lookup list for default proj4 strings utilized.  Users may also
#' specify their own lookup list.  Options also include providing either the
#' EPSG, ESRI, or SR-ORG projection codes to retrieve the associated proj4
#' string from a web query from http://spatialreference.org.  Definitions for
#' proj4 string parmeters can be referenced here:
#' http://proj4.org/parameters.html#parameter-list
#'
#' @param obj Use object instead that has a predefined proj4 string.
#' @param prj_type The projection type is either 'esri', 'epsg', or 'sr-org' or 'prj4s_key'.
#' @param prj_code The projection code as an integer for EPSG, ESRI, or SR-ORG.
#' @param prj4s_key Lookup key string identifying the default projection.
get_prj4s <- function(obj = NULL, prj_type = NULL, prj_code = NULL, prj4s_key = NULL) {

    # default prj4 key: string lookup
    lu <- list(
        'us'     = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83",
        'africa' = "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 ",
        'world'  = "+proj=longlat +datum=WGS84 +no_defs",
        'ch_aea' = "+proj=aea +lat_1=27 +lat_2=45 +x_0=0 +y_0=0 +lat_0=35 +lon_0=105 +ellps=WGS84 +datum=WGS84",
        'eck3'   = "+proj=eck3 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    )

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


#' Helper function for assigning a Proj4 string from several possible inputs.
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

#' Create a rectangluar sf polygon from numeric extent or sf bbox.
#'
#' @param ext Numeric extent [xmin, xmax, ymin, ymax] or an object of class bbox
pgon_from_extent <- function(ext) {
    # The position of xmax and ymin are switched with a bbox-defined extent
    xmin <- ext[1]
    xmax <- ext[(class(ext) == "bbox") + 2]
    ymin <- ext[(class(ext) != "bbox") + 2]
    ymax <- ext[4]

    # calling st_segmentize with a 1 degree resolution makes reprojections of
    # the rectangle look smooth
    return(sf::st_polygon(list(rbind(c(xmin, ymin), c(xmin, ymax), c(xmax, ymax),
                                     c(xmax, ymin), c(xmin, ymin)))))
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
spat_bb <- function(b_ext, buff_dist = 0, proj4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

  # create a rectangular polygon from bounding box, segmentize it to 1 degree
  # resultion so that reprojections of it look smooth, then convert it to simple
  # features polygon collection
  geom <- pgon_from_extent(b_ext) %>%
      sf::st_segmentize(1) %>%
      sf::st_sfc()

  # make sf object, assign default WGS84 proj, and transform projection to that
  # of the input mapdata
  bb <- sf::st_sf(geometry = geom) %>%
    sf::st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
    sf::st_transform(proj4s)

  # buffer if user desires
  if (buff_dist) {
    # each zoom level shrinks the map's bounding rectangle by 10% of its
    # shortest side
    x <- sf::st_bbox(bb)[3] - sf::st_bbox(bb)[1]
    y <- sf::st_bbox(bb)[4] - sf::st_bbox(bb)[2]
    zoom <- buff_dist * -0.1 * min(x, y)
    if (sf::st_is_longlat(bb)) zoom <- units::set_units(zoom, degree)

    # Suppress Warning:
    #     "In st_buffer.sfc(st_geometry(x), dist, nQuadSegs) : st_buffer does
    #     not correctly buffer longitude/latitude data, dist needs to be in
    #     decimal degrees."
    # This warning occurs from buffering a feature that is in a geographic
    # coordinate system (lat/long) rather than a projection one.  This makes the
    # the buffer not be exact due to the distance being calculated in decimal
    # degrees rather than meters or kilometers. This is fine with us since we
    # are simply using buffer as a way of zooming to include or exclude portions
    # of the bounding extent in this call.
    return(suppressWarnings({ sf::st_buffer(bb, zoom, nQuadSegs = 5) }))
  }
  else {
    return(bb)
  }
}


#' Clean spatial data.
#'
#' Removes empty, corrupt, or invalid geometries from an sfc object.
#'
#' @param sfcobj Object of class \code{sf} or \code{sfc}
remove_invalid <- function(sfcobj) {
    invalids <- sf::st_is_valid(sfcobj, reason = T)
    intrscts <- grepl("Self-intersection", invalids)

    # See note in spat_bb about warning/message
    sfcobj[intrscts, ] <- suppressWarnings({
        suppressMessages({ sf::st_buffer(sfcobj[intrscts, ], 0) })
    })

    sfcobj <- sfcobj[!is.na(invalids), ]
    return(sfcobj)
}
