# utils.R
#
# Contains general helper functions for the package


#' Process shape
#'
#' This function processes the shape argument for dynamic mapping functions
#'
#' @param shape_data (SF, SP, or Character) - Either the full path string to a shape file (with included necessary files) or an SF shape object
#' @param shape_key_field (Character) - Name of key field in shape object for merging with map_data object
#' @param shape_data_field (Character) - Optional field for utilizing a field within the shape data as the map data field. Negates the map_data variable
#' @param shape_label_field (Character) - Optional field for plotting data available from the shape attributes/fields (such as country name)
#' @param shape_label_size_field (Character) - Optional field used for computing shape label size dynamically (ie by area or amount etc.)
#' @param shape_xy_fields (c(Character, Character)) - Vector that specifies the x and y field names in the shape object (default c("LAT", "LON"))
#' @param shape_geom_field (Character) - Specifies field within shape object that contains needed geometry (default "geometry")
#' @param simplify (Boolean) - Option to reduce the number/complexity of the polygons in the shape file (default FALSE)
#' @return (SF or Character) - Returns the resulting simplified SF object or an error string if failed
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
#' @export
process_shape <- function(shape_data, simplify = FALSE, shape_label_field = NULL, shape_data_field = NULL, shape_key_field = NULL,
                          shape_label_size = "1", shape_xy_fields  = c("LON", "LAT"), shape_geom_field  = "geometry")
{
  tryCatch(
  {
    # Initial processing of shape object so that a completed shape object can be sent to the error check function
    if(is.null(shape_data))
    {
      return("Error: Shape data is NULL")
    }
    else if("character" %in% class(shape_data))
    {
      if (!file.exists(shape_data))
      {
        return("Error: Cannot open shape file or shape file path does not exist ")
      }
    }

    # Import shape data
    shape_obj <- gcammaptools::import_mapdata(shape_data)

    if(suppressWarnings({!"sf" %in% class(shape_obj)}))
    {
      return(paste0("Error: There was an unknown error processing the shape object: ", shape_obj))
    }

    result <- verify_shape(shape_obj, simplify, shape_label_field, shape_data_field, shape_key_field, shape_label_size, shape_xy_fields, shape_geom_field)

    if(result != "Success")
    {
      return(result)
    }

    # Verify raster CRS and assign default if NA
    if(is.na(crs(shape_obj)) || is.null(crs(shape_obj)))
    {
      sf::st_crs(shape_obj) <- crs(default_projection)
      print("Applying default CRS to shape (shape CRS was NA or NULL)")
    }

    # Optional argument to simplify polygons via the simplify_mapdata function
    if(simplify)
    {
      shape_obj <- gcammaptools::simplify_mapdata(shape_obj)
    }
    else
    {
      return(result)
    }
  },
  error = function(err)
  {
    # error handler picks up error information
    error <- err
    return(error)
  })

  return(shape_obj)
}


#' Process raster
#'
#' This function handles the processing of raster data for use in dynamic mapping functions
#'
#' @param raster_data (Raster or Character) - Either the full path string to raster file or a raster object
#' @param raster_col (Character) - Column name that contains the raster object's output variable
#' @param raster_band (Numeric) - Future variable for dealing with multi band/time series rasters etc (default 1)
#' @param bin_method (Character) - Method or function to use to split continuous data into discrete chunks (default "pretty")
#' @param bins (Numeric) - Number of bins/segments in which to divide the raster (default 8)
#' @param convert_zero (Boolean) - Convert raster zero values to NA (default FALSE)
#' @importFrom rgis import_raster
#' @importFrom ggspatial layer_spatial df_spatial
#' @importFrom raster raster crs
#' @return (Raster or Character) - Returns the resulting raster object or an error string if failed
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
#' @export
process_raster <- function(raster_data, raster_col, raster_band = 1, bin_method = "pretty", bins = 8, convert_zero = FALSE)
{
  tryCatch(
    {
      # Verify raster
      result <- verify_raster(raster_data , raster_col, raster_band, bin_method, bins, convert_zero)

      # If raster passes verification, load and process
      if(result == "Success")
      {
        # Import raster data
        raster_obj <- import_raster(raster_data)
      }
      else
      {
        return(result)
      }

      # Set raster band
      if(raster_band != 1)
      {
        raster_obj <- raster(raster_obj, band=raster_band)
      }

      # Convert zeroes to NA if enabled
      if(convert_zero == TRUE)
      {
        raster_obj[raster_obj==0] <- NA
      }
    },
    error = function(err)
    {
      # error handler picks up error information
      error <- err
      return(error)
    })

  return(raster_obj)
}


#' Process data
#'
#' This function handles the processing of source map data for use in dynamic mapping functions
#'
#' @param map_data (Data Frame or Character) - A data frame that contains the output data to map, or alternatively a full path to a CSV
#' @param data_key_field (Character) - Name of key field in data_obj for merging with shape_data
#' @param data_col (Character) - Column name that contains the data object's output variable
#' @param shape_obj (SF) - An SF object
#' @param shape_key_field (Character) - Name of key field in shape object for merging with map_data object
#' @param shape_data_field (Character) - Optional field for utilizing a field within the shape data as the map data field. Negates the map_data variable
#' @return (Data Frame or Character) - Returns the resulting simplified SF object or an error string if failed
#' @author Jason Evanoff, jason.evanoff@pnnl.gov
#' @export
process_data <- function(map_data, data_key_field, data_col,  shape_obj, shape_data_field, shape_key_field)
{
  tryCatch(
    {
      if(is.null(shape_data_field))
      {
        # Read/process map data object via local processing function
        map_data_obj <- process_data(map_data, data_key_field, data_col, data_key_field, shape_obj, shape_data_field, shape_key_field)

        if(class(map_data_obj) == "character")
        {
          # Process_data must have caught an error
          return_error(map_data_obj, "Process Data")
          return("Error - see console for output")
        }
        # Merge map and data
        suppressWarnings({combined_df <- left_join(x = shape_obj, y = map_data_obj, by = setNames(data_key_field,  shape_key_field))})
      }
      else
      {
        suppressWarnings({combined_df <- as.data.frame(shape_obj)})
        data_col <- shape_data_field
      }
      if(is.null(map_data))
      {
        return("Error: Map data cannot be NULL")
      }

      # Map Data - if given a path to a csv, use that, else expect a data.frame object passed in
      if(!class(map_data) %in% c("data.frame", "character"))
      {
        return("Error: map_data argument must be of type data.frame or a character path to a csv file")
      }

      if("data.frame" %in% class(map_data) )
      {
        map_obj <- map_data
      }
      else if("character" %in% class(map_data))
      {
        if (file.exists(map_data))
        {
          map_obj <- read.csv(map_data, stringsAsFactors = F)
        }
        else
        {
          return(paste0("Error: Cannot open data file ", map_data))
        }
      }
      else
      {
        return("Error: Unrecognized map_data argument.")
      }

      result <- verify_data(map_obj, data_key_field, data_col)
      if(result != "Success")
      {
        return(result)
      }
    },
    error = function(err)
    {
      # error handler picks up error information
      error <- err
      return(error)
    })

  return(map_obj)
}


#' Import ESRI Shapefile or GeoJSON as sf object.
#'
#' Creates a Simple Feature (sf) object from full path string to ESRI Shapefile
#' or GeoJSON file. User defines which field is supposed to represent the ID for
#' the data.
#'
#' @param file_pth Full path to shapefile with extention (.shp). Shapefiles must
#' contain at least .shp, .shx, and .dbf file to function properly.
load_shp <- function(file_pth) {
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
simplify_mapdata <- function(mapdata, min_area = 2.5, degree_tolerance = 0.1) {

  . <- NULL                             # silence package notes for NSE.

  if ("MULTIPOLYGON" %in% sf::st_geometry_type(mapdata))
    mapdata <- sf::st_cast(mapdata, "POLYGON", warn = FALSE)

  # filter out all polygons in the data under the minimum area
  areafilter <- sapply(sf::st_geometry(mapdata), sf::st_area) > min_area
  filtermap <- mapdata[which(areafilter), ]

  filtermap <- suppressWarnings({sf::st_simplify(filtermap, preserveTopology=TRUE, dTolerance=degree_tolerance)})

  # if nothing was filtered just return original map
  if (utils::object.size(filtermap) == utils::object.size(mapdata))
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
