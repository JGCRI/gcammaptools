# map_functions.R
#
# The main file containing functions for producing maps of spatial GCAM data.


# Loading Data ------------------------------------------------------------

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



# Map Data Transformations ------------------------------------------------

#' Get features topologically associated with extent bounds.
#'
#' Conducts a spatial join to retrieve spatial features that are topologically associated
#' (intersects, contains, within, etc.) with the provided bounds.
#'
#' @param mapdata The sf object containing the spatial data.
#' @param bbox Bounding box.
#' @param p4s The proj4 string of the final map.
#' @param extent Extent provided by the user or as default.
#' @param col Field name with target information to map.
#' @param agr_type Inherited attribute-geometry-relationship type from plot_GCAM
#' function params.
#' @param topo SF topologic function to define how the join will be conducted.
#' Default is to join any feature that intersects the bounding box.
filter_spatial <- function(mapdata, bbox, p4s, extent, col, agr_type='constant', topo=sf::st_intersects) {
  # set NULL column to index
  clm <- if (is.null(col)) 1 else col

  # set attribute-geometry-relationship for input mapdata column and bounding
  # box feature attribute
  sf::st_agr(mapdata) <- agr_type
  sf::st_agr(bbox) <- agr_type

  # Message for st_join suppressed:
  #     "although coordinates are longitude/latitude, it is assumed that they
  #     are planar."
  # This comes from the input projection being a geographic coordinate system
  # and not a projected one when conducting topological operations such as
  # st_intersects used in the st_join. If 'longlat' appears in the proj string
  # ("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") then this message will
  # present itself due to the operation being intersect according to the source
  # code. There is no option to quiet this.  We are getting the expected return
  # from the operation due to harmonizing the map and bounding box projection
  # pre-join.

  # if extent is not world conduct spatial join; else, return all
  if (!isTRUE(all.equal(extent, EXTENT_WORLD))) {
      # the filtering bbox must be rectangular in the coordinates being plotted
      # because the resulting map will be viewed in a rectangular window
      if (sf::st_is_longlat(mapdata) & !grepl("(+proj=longlat|+proj=ortho)", p4s)) {
          bbox <- sf::st_transform(bbox, p4s)
          xn <- sf::st_bbox(bbox)[['xmin']]
          xx <- sf::st_bbox(bbox)[['xmax']]
          yx <- sf::st_bbox(bbox)[['ymax']]
          yn <- sf::st_bbox(bbox)[['ymin']]
          newbbox <- list(rbind(c(xn,yn),c(xn,yx), c(xx,yx), c(xx,yn), c(xn,yn))) %>% sf::st_polygon()
          sf::st_geometry(bbox)[[1]] <- newbbox
          bbox <- sf::st_transform(bbox, sf::st_crs(mapdata), check = T)
      }

      return(suppressMessages({sf::st_join(mapdata[clm], bbox, left = FALSE)}))
  }
  # conducting the intersection here eliminates erroneous-filled poly generated
  # at the global extent
  else {
    return(suppressMessages({sf::st_intersection(mapdata, bbox)}))
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

    if (is.null(gcam_df)) {
        return(mapdata)
    }

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
               dplyr::select(-pkey) %>%
               sf::st_as_sf()

    return(mapdata)
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
  if (object.size(filtermap) == object.size(mapdata))
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


# Map Creation ------------------------------------------------------------

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
#' @param mapdata The data frame containing both geometric data (simple features
#'   collection and id) and regional metadata.  This is the only mandatory
#'   variable. If used alone, will produce the default map.
#' @param col If plotting categorical/continuous data, the name of the column to
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
#'   vignette} for further explanation.
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
                      zoom = 0, graticules = FALSE, agr_type = 'constant',
                      background_color = '#ddefff',
                      padding = all(extent == EXTENT_WORLD)) {

  # get proj4 string that corresponds to user selection
  p4s <- assign_prj4s(proj_type, proj)

  # ensure that the map is an sf object
  map <- import_mapdata(mapdata)

  # eliminate erroneous-filled polygons generated at the global extent
  wborder <- spat_bb(EXTENT_WORLD, 0, sf::st_crs(map))
  sf::st_agr(wborder) <- agr_type
  sf::st_agr(map) <- agr_type
  map <- sf::st_intersection(map, wborder)

  # create sf obj bounding box from extent and define transformed proj;
  # apply buffer if needed; this is the box defining the final view of the map,
  # so convert it to a rectangle.
  bounds <- spat_bb(b_ext = extent, buff_dist = zoom, proj4s = p4s)
  if (!all(extent == EXTENT_WORLD)) {
      xn <- sf::st_bbox(bounds)[['xmin']]
      xx <- sf::st_bbox(bounds)[['xmax']]
      yx <- sf::st_bbox(bounds)[['ymax']]
      yn <- sf::st_bbox(bounds)[['ymin']]
      newbbox <- sf::st_polygon(list(rbind(c(xn,yn), c(xn,yx), c(xx,yx), c(xx,yn), c(xn,yn))))
      sf::st_geometry(bounds)[[1]] <- newbbox
  }

  map <- sf::st_transform(map, p4s)
  map <- remove_invalid(map)
  map <- sf::st_intersection(map, bounds)
  map <- sf::st_join(map, bounds, left = FALSE)

  map <- join_gcam(map, mapdata_key, gcam_df, gcam_key)
  border <- ggplot2::geom_sf(data = bounds, fill = background_color)

  dtm <- if (graticules) wgs84 else NA
  map_opts <- ggplot2::coord_sf(expand = padding, datum = dtm)

  # generate plot object
  if (is.null(col)) gsf <- ggplot2::geom_sf(data = map, fill = '#222222', color = alpha('#888888', 0.5))
  else gsf <- ggplot2::geom_sf(data = map, aes_string(fill = col), alpha = 1, color = alpha('#000000', 0.5))
  mp <- ggplot() +
    border +
    gsf +
    map_opts +
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

        # set the number of rows and columns in the raster equal to the number
        # of unique latitudes and longitudes in the original data
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
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 13, face = "bold"),
                  legend.position = LEGEND_POSITION,
                  legend.key = element_rect(color = "black"))

    } else {
        theme_bw(base_size = base_size, base_family = base_family) %+replace%
            theme(panel.border = element_rect(color = LINE_COLOR, fill = NA),
                  panel.background = element_rect(fill = '#eeeeee'),
                  panel.grid.major = PANEL_GRID,
                  axis.ticks = AXIS_TICKS,
                  axis.text = AXIS_TEXT,
                  legend.position = "none")
    }
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

#' Designator for the usa map set
#'
#' This symbol will select the usa map set
#' @export
usa <- quote(usa)
