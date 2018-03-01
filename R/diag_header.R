# LEGAL NOTICE
# This computer software was prepared by Battelle Memorial Institute,
# hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
# with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
# CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
# LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
# sentence must appear on any copies of this computer software.
#
# EXPORT CONTROL
# User agrees that the Software will not be shipped, transferred or
# exported into any country or used in any manner prohibited by the
# United States Export Administration Act or any other applicable
# export laws, restrictions or regulations (collectively the "Export Laws").
# Export of the Software may require some form of license or other
# authority from the U.S. Government, and failure to obtain such
# export control license may result in criminal liability under
# U.S. laws. In addition, if the Software is identified as export controlled
# items under the Export Laws, User represents and warrants that User
# is not a citizen, or otherwise located within, an embargoed nation
# (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
#     and that User is not otherwise prohibited
# under the Export Laws from receiving the Software.
#
# Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
# Distributed as open-source under the terms of the Educational Community
# License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
#
# For further details, see: http://www.globalchange.umd.edu/models/gcam/
#

# diag_header.R
#
# An automated graphing system to generate both standard and user-defined maps
# of GCAM data.
#
# Ben Bond-Lamberty, November 2012
# Last Modified: February, 2018

# -----------------------------------------------------------------------------
# Default Projections (as PROJ4 strings)

### Predefined PROJ4 projection strings
#' Proj4 string for default WGS84 (EPSG:4326) coordinate reference system
#'
#' String for specifying the default WGS84 projection in mapping functions
#' Its value is \code{'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'}
#' @export
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' Proj4 string for the Eckert III World projection
#'
#' String for specifying the Eckert III projection in mapping functions.
#' Its value is \code{'+proj=eck3'}
#' @export
eck3 <- "+proj=eck3"
#' Proj4 string for the Robinson World projection
#'
#' String for specifying the Robinson projection in mapping functions.
#' Its value is \code{'+proj=robin'}
#' @export
robin <- "+proj=robin"
#' Proj4 string for the Albers equal area projection over North America.
#'
#' String for specifying the Albers equal area projection over North
#' America in mapping functions.  This is a conic projection situatied
#' over the continental United States.  Its value is \code{'+proj=aea
#' +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
#' +datum=NAD83'}
#' @export
na_aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"
#' Proj4 string for the Albers equal area projection over China
#'
#' String for specifying the Albers equal area projection over China
#' in mapping functions.  This is a conic projection situatied over
#' China.  Its value is \code{'+proj=aea +lat_1=20 +lat_2=60 +lat_0=40
#' +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84'}
#' @export
ch_aea <- "+proj=aea +lat_1=27 +lat_2=45 +x_0=0 +y_0=0 +lat_0=35 +lon_0=105 +ellps=WGS84 +datum=WGS84"
#' Proj4 string for orthographic projection over Africa
#'
#' String for specifying the orthographic projection over Africa.  You can pass
#' this value to the \code{proj} argument of \code{\link{plot_GCAM}} to get the
#' best result.
#' @export
af_ortho <- "+proj=ortho +lat_0=10 +lon_0=19 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"


# -----------------------------------------------------------------------------
# EXTENT
# (lon0,lon1,lat0,lat1)

#' Extent vector for the entire world
#'
#' This vector can be used as the \code{extent} argument to
#' \code{\link{plot_GCAM}}.
#' @export
EXTENT_WORLD <- c(-180,180,-90,90)
#' Extent vector for the continental United States
#'
#' This vector can be used as the \code{extent} argument to
#' \code{\link{plot_GCAM}}.
#' @export
EXTENT_USA <- c(-120,-70,20,60)
#' Extent vector for China
#'
#' This vector can be used as the \code{extent} argument to
#' \code{\link{plot_GCAM}}.
#' @export
EXTENT_CHINA <- c(77,130,15,53)
#' Extent vector for Africa
#'
#' This vector can be used as the \code{extent} argument to
#' \code{\link{plot_GCAM}}.
#' @export
EXTENT_AFRICA <- c(-20,60,-40,40)
#' Extent vector for Latin America
#'
#' This vector can be used as the \code{extent} argument to
#' \code{\link{plot_GCAM}}.
#' @export
EXTENT_LA <- c(-118,-33,-56,37)


# -----------------------------------------------------------------------------
# AESTHETICS

# Default Colors
LINE_COLOR <- "#444444"
FILL_COLOR <- "#222222"
BORDER_LIGHT <- alpha("#888888", 0.5)
BORDER_DARK <- alpha("#000000", 0.5)
MAP_BACKGROUND <- "#ddefff"

# Background
PANEL_BACKGROUND <- ggplot2::element_blank()
PANEL_GRID <- ggplot2::element_line(colour = "black")
AXIS_TICKS <- ggplot2::element_blank()
AXIS_TEXT <- ggplot2::element_blank()
XLAB <- ""
YLAB <- ""


# Legend
LEGEND_POSITION = "bottom"

#' Color palette for 14-region GCAM
#'
#' This palette should be used for plots by region (whether maps, line plots, or
#' other types) to ensure consistency across plots and publications.  XXX:
#' Perhaps this sort of thing should go in a separate GCAM style package, since
#' it isn't really specific to mapping?
#' @export
gcam14_colors<- c("Africa" = "navajowhite3",
                  "Australia_NZ" = "lightpink",
                  "India" = "lightslateblue",
                  "USA" = "sandybrown",
                  "Japan" = "rosybrown1",
                  "Korea" = "brown",
                  "Eastern Europe" = "orange" ,
                  "Western Europe" = "greenyellow",
                  "Canada" = "saddlebrown",
                  "China" = "lightblue",
                  "Southeast Asia" = "gold",
                  "Latin America" = "seagreen2",
                  "Middle East" = "indianred",
                  "Former Soviet Union" = "plum2")


rgb255 <- function(r, g, b) {grDevices::rgb(r,g,b, maxColorValue=255)}
#' Color palette for 32-region GCAM
#'
#' This palette should be used for plots by region (whether maps, line plots, or
#' other types) to ensure consistency across plots and publications.
#' @export
gcam32_colors <- c(
    'Africa_Northern' = rgb255(139,69,19),
    'Africa_Eastern' = rgb255(139,115,88),
    'Africa_Southern' = rgb255(255,211,155),
    'Africa_Western' = rgb255(255,185,15),
    'South Africa' = rgb255(255,215,0),

    'Canada' = rgb255(224,238,224),
    'USA' = rgb255(77,77,77),

    'Argentina' = rgb255(0,100,0),
    'Brazil' = rgb255(154,205,50),
    'Central America and Caribbean' = rgb255(46,139,87),
    'Colombia' = rgb255(102,205,170),
    'Mexico' = rgb255(50,205,50),
    'South America_Southern' = rgb255(72,209,204),
    'South America_Northern' = rgb255(0,255,0),

    'EU-12' = rgb255(25,25,112),
    'EU-15' = rgb255(131,111,255),
    'Europe_Eastern' = rgb255(173,216,230),
    'Europe_Non_EU' = rgb255(0,104,139),
    'European Free Trade Association' = rgb255(58,95,205),

    'Russia' = rgb255(104,34,139),
    'China' = rgb255(255,0,0),
    'Middle East' = rgb255(188,143,143),
    'Australia_NZ' = rgb255(255,193,193),
    'Central Asia' = rgb255(139,0,0),
    'India' = rgb255(208,32,144),
    'Indonesia' = rgb255(139, 28, 98),
    'Japan' = hsv(0.01, 0.75, 0.65),
    'Pakistan' = rgb255(205, 181, 205),
    'South Asia' = rgb255(139, 123, 139),
    'South Korea' = rgb255(205, 92, 92),
    'Southeast Asia' = rgb255(240, 128, 128),
    'Taiwan' = rgb255(150, 150, 150)
)
