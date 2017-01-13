## MAP FUNCTIONS
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

gen_grat <- function(bbox = EXTENT_WORLD, longint = 20, latint = 30) {
    ### Generate graticule (long/lat lines) given bbox
    ### Inputs:
    ###   bbox - numeric vector (long_min, long_max, lat_min, lat_max)
    ###   longint - interval between longitude lines
    ###   latint - interval between latitude lines
    ### Outputs:
    ###   grat - dataframe describing of latitude and longitude lines
    ###         spaced at specified intervals

    ### TODO: We could probably realize some savings here by
    ### precalculating and caching graticules for some commonly-used
    ### configurations.

    # Generate graticule as sp matrix object
    lons = seq(bbox[1], bbox[2], by = longint)
    lats = seq(bbox[3], bbox[4], by = latint)

    grat <- graticule::graticule(lons, lats, xlim = range(lons), ylim = range(lats))

    # Convert to ggplot2-friendly format
    grat <- ggplot2::fortify(grat)

    return(grat)

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
#' \code{ggalt::coord_proj}.  For convenience, this package
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
#' \code{mapproj::coord_map} function.  To get this projection
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
#' @param extent Bounding box for the display map
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
#' @param ... Other parameters passed on to \code{mappolys}
#' @examples \dontrun{
#'
#' ##Plot a map of GCAM regions; color it with a palette based on RColorBrewer's 'Set3' palette.
#'   map_32_wo_Taiwan<-rgdal::readOGR(system.file('extdata/rgn32', 'GCAM_32_wo_Taiwan_clean.geojson',
#'                                                package='gcammaptools'))
#'   map_32_wo_Taiwan.fort<-ggplot2::fortify(map_32_wo_Taiwan, region='GCAM_ID')
#'   mp1<-plot_GCAM(map_32_wo_Taiwan.fort, col = 'id', proj = eck3, colorfcn=qualPalette)
#'
#'   ## Plot oil consumption by region
#'   tables<-parse_mi_output(fn = system.file('extdata','sample-batch.csv',package='gcammaptools'))
#'   prim_en<-process_batch_q(tables, 'primary_energy', 'Reference', c(fuel='a oil'))
#'   prim_en<-addRegionID(prim_en, file.path(basedir.viz,
#'                                 system.file('extdata/rgn32', 'lookup.txt', package='gcammaptools'),
#'                                 system.file('extdata/rgn32', 'drop-regions.txt', package='gcammaptools')))
#'   mp2<-plot_GCAM(map_primen, col = 'X2050', colors = c('white', 'red'), title='Robinson World', qtitle='Oil Consumption, 2050', legend=T)
#' }
#' @export
plot_GCAM <- function(mapdata, col = NULL, proj = robin, extent = EXTENT_WORLD,
                      orientation = NULL, title = NULL, legend = F, colors =
                          NULL, qtitle = NULL, limits = NULL, colorfcn = NULL,
                      nacolor=gray(0.75),  ...) {

    # Generate graticule (latitude/longitude lines) and clip map to extent specified.
    grat <- gen_grat()
    mappolys <- get_bbox_polys(dataset = mapdata)


    # Plot graticule and polygons
    mp <- ggplot() + geom_path(data = grat, aes(long, lat, group = group, fill = NULL), color = LINE_GRAT) +
        geom_polygon(data = mappolys, aes_string("long", "lat", group = "group", fill = col),
            color = LINE_COLOR)

    # If a column name is specified, add a color gradient or categorical colors
    if (!is.null(col)) {

        if (is.numeric(mappolys[[col]])) {
            # Instructions for color gradient Calculate legend label increments ('breaks')
            if (is.null(limits)) {
                limits <- calc.limits.map(mappolys, col)
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
                colors <- colorfcn(n = length(unique(mappolys[[col]])), ...)
            }

            # Add color scale to map
            mp <- mp + scale_fill_manual(values = colors, name = qtitle)
        }
    } else {
        # If no data is being plotted, use default color scale
        mp <- mp + geom_polygon(data = mappolys, aes_string("long", "lat", group = "group", fill = col),
            fill = nacolor, color = LINE_COLOR)
    }

    # Project map and add theme and labels
    mp <- mp + coord_GCAM(proj = proj, orientation = orientation, extent = extent) + theme_GCAM(legend = legend) +
        labs(title = title, x = XLAB, y = YLAB)

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
