##MAP FUNCTIONS 

setwd('C:/Users/ledn787/Desktop/Visualization_Work/gcam-viz/')

#Source Scripts functions
for (fn in list.files("scripts", full.names = T)){
  source(fn)
}

source("geojson-methods/Map_Params.R")
#---------------------------------------------------------------------------
# Required libraries
#---------------------------------------------------------------------------
library("rgdal")
library("ggplot2")  #Version 2.0.0
library("ggalt") #Note: currently using version 01.2.900 from github. May have to modify fcns to be compatible w/ old version 
library("graticule")  #See if you can avoid using this -- probably a workaround. 
library("RColorBrewer")
library("maptools")

#---------------------------------------------------------------------------
# DATA PROCESSING FUNCTIONS
#---------------------------------------------------------------------------

process_batch_q<-function(batchq, query, scen, filters, func=sum){
  ### Extract a query from a list of queries and filter to desired output.
  ### Allows for simple transformations on data (sum, mean, stdev, etc)
  ### Inputs: 
  ###   batchq - the output from parse_mi_output; a list of tables of queries.
  ###   query - the name of the table you would like to select from batchq
  ###   scen - the name of the scenario; can be partial
  ###   filters - a named vector of filtering criteria, in the form: 
  ###           "c(header1 = value1, header2 = value2, ...), where headers
  ###           are the names of columns in the data frame. If aggregating data,
  ###           use the value "Aggregate". 
  ###   func - optional; specify the operation you would like to perform on the
  ###           aggregated data (e.g. mean, sum, etc.)
  ### Outputs: 
  ###   qdata - a data frame of filtered query data. 
  
  qdata<-as.data.frame(batchq[[query]])
  
  #Filter for scenario; allow partial lookup
  #Bug: if partial has multiple matches, will return multiple scenarios
  qdata<-qdata[grepl(scen, qdata$scenario),]
  
  #Fix units -- some have zero value; get rid of that. 
  unit<-as.character(unique(qdata$Units[qdata$Units!=0]))
  qdata$Units=unit
  
  #Get years and aggregate value if applicable
  years<-grep("(X1)|(X2)", names(qdata), value=T)
  ag<-names(filters[filters[names(filters)]=="Aggregate"]) #Super clunky
  
  nms<-!(names(qdata) %in% years| names(qdata) %in% ag) 
  
  #Filter to query of interest using filters 
  for (name in names(filters)){
    if (filters[[name]]=="Aggregate"){
      qdata<-aggregate(qdata[years], by=qdata[nms], FUN=func)
      qdata[[ag]]<-"All"
    }
    else{
      qdata<-qdata[qdata[[name]]==filters[[name]],]
    }
  }
  
  return(qdata)
  
}

#TODO - modify to search for appropriate lookup, province, drop files in directory. 
addRegionID<-function(datatable, lookupfile, provincefile='none', drops='none') {
  ### Match GCAM ID to region using lookup file. Last data processing step 
  ###   before joining GCAM scenario data to geoJSON map.
  ###   Could just join by region, but prefer to avoid possible spelling 
  ###   issues. 
  ### Inputs: 
  ###   datatable - a query data frame processed with process_batch_q
  ###   lookupfile - string; path to the lookup file for the geoJSON map
  ###   provincefile - string; path to the province translation file, if applicable
  ###   drops - string; path to the drop regions file, if applicable
  ### Outputs: 
  ###   finaltable - a data frame with GCAM ID attached to each region. 
  
  if (provincefile != 'none'){
    datatable<-translateProvince(datatable, provincefile)
  }
  
  if (drops != 'none'){
    datatable<-dropRegions(datatable, drops)
  }
  
  lookuptable<-read.csv(lookupfile, strip.white=T, stringsAsFactors = F)
  
  #Differentiate region-Region issue
  if ("Region" %in% names(datatable)){
    rgn<-"Region"
  } else{
    rgn<-"region"
  }
  
  finaltable<-merge(datatable, lookuptable, by.x=rgn, by.y=colnames(lookuptable)[1] )
  colnames(finaltable)[ncol(finaltable)]<-'id'
  finaltable$id<-as.character(finaltable$id)
  
  
  #Add null vector row to end to account for GCAM region 0 
  nullvec<-c('0', 2:ncol(finaltable))
  nullvec[2:ncol(finaltable)]<-NA
  
  finaltable<-rbind(finaltable, nullvec)
  finaltable$id[nrow(finaltable)]<-'0'
  
  return(finaltable)
}

translateProvince<-function(datatable, provincefile){
  ### Replace province abbreviations with full province names
  ### to ensure matching with GCAM map names. 
  ### Inputs: 
  ###   datatable - data frame of query from batch query CSV. 
  ###   provincefile - string; path to file with abbreviations and full names of regions. 
  ### Outputs: 
  ###   datatable - datatable modified so that abbreviations are now full names. 

  provincetable<-read.csv(provincefile, strip.white=T)
  
  #Differentiate region-Region issue
  if ("Region" %in% names(datatable)){
    rgn<-"Region"
  } else{
    rgn<-"region"
  }
  
  datatable$rgn<-as.character(datatable$rgn)
  provincetable$province<-as.character(provincetable$province)
  provincetable$province.name<-as.character(provincetable$province.name)
  
  datatable$rgn<-ifelse(is.na(provincetable$province.name[match(datatable$rgn, provincetable$province)]), 
                           datatable$rgn, 
                           provincetable$province.name[match(datatable$rgn, provincetable$province)])
  
  return(datatable)
} 

dropRegions<-function(datatable, drops){
  ### Drop regions listed in drops file from data frame. 
  ### Inputs: 
  ###   datatable - a data frame of query from batch query CSV
  ###   drops - string; path to file containing regions to be dropped
  ### Outputs:
  ###   datatable - updated data frame with regions dropped. 
  
  dr<-read.csv(drops, strip.white=T, header=F)
  dr<-as.character(dr$V1)

  regcols<-grepl("egion", names(datatable)) #Find instances of "region" or "Region" columns
  
  datatable[regcols]<-lapply(datatable[regcols], function(x) replace(x, x %in% dr, NA)) #Replace drop col values with NA
  
  datatable<-na.omit(datatable) #Remove rows containing NA
  
  return(datatable)
}

#---------------------------------------------------------------------------
# MAPPING UTILS
#---------------------------------------------------------------------------
get_bbox_polys<-function(dataset, bbox){
  ### Modifies map data to include only polygons that lie partially within 
  ### bounding box. 
  ### Inputs: 
  ###   dataset - data frame of map geometry
  ###   bbox - numeric vector (long_min, long_max, lat_min, lat_max)
  ### Outputs: 
  ###   newdata - data with only polygons that are at least partially in 
  ###           bounding box
  
  #Parse bbox and define functions
  fxlons<-in_range(bbox[1],bbox[2])
  fxlats<-in_range(bbox[3],bbox[4])
  
  #Find longs and lats in range
  lons<-sapply(dataset$long, function(x) fxlons(x))
  lats<-sapply(dataset$lat, function(x) fxlats(x))
  
  ids<-intersect(dataset$id[lons], dataset$id[lats])
  newdata<-dataset[dataset$id %in% ids,]
  
  return(newdata)
}

#in_range - Returns function to determine whether x is in range a,b
#params: a,b - can be int or numeric; a<=b
in_range<-function(a,b){
  function(x) x>=a && x<=b
}

gen_grat<-function(bbox=EXTENT_WORLD,longint=20,latint=30){
  ### Generate graticule (long/lat lines) given bbox
  ### Inputs: 
  ###   bbox - numeric vector (long_min, long_max, lat_min, lat_max)
  ###   longint - interval between longitude lines
  ###   latint - interval between latitude lines
  ### Outputs: 
  ###   grat - dataframe describing of latitude and longitude lines 
  ###         spaced at specified intervals
  
  require(graticule)
  
  #Generate graticule as sp matrix object 
  lons=seq(bbox[1],bbox[2],by=longint)
  lats=seq(bbox[3],bbox[4],by=latint)
  
  grat<-graticule(lons,lats,xlim=range(lons),ylim=range(lats))
  
  #Convert to ggplot2-friendly format
  grat<-fortify(grat)
  
  return(grat)
  
}

calc_breaks<-function(mapdata, colname, nbreaks=4){
  ### Calculate legend breaks (intervals to put labels on legend scale)
  ### Inputs: 
  ###   mapdata - data frame of geometry and scenario data
  ###   colname - column name of interest from which to calculate legend intervals
  ###   nbreaks - number of intervals
  ### Outputs: 
  ###   breaks - vector of values at which to include legend label
  
  #Convert data of interest to numeric
  mapdata[, colname]<-as.numeric(mapdata[,colname])
  ndat<-na.omit(mapdata)
  
  max_dat<-signif(max(ndat[colname], na.rm = T),digits = 2)
  min_dat<-signif(min(ndat[colname], na.rm= T),digits = 2)
  
  break_int<-(max_dat/(nbreaks-1))
  breaks<-seq(0,max_dat,by=break_int)
  breaks<-signif(breaks,digits=2)
  
  
  return(breaks)
}
#---------------------------------------------------------------------------
# DATA TRANSFORMATION FUNCTIONS
#---------------------------------------------------------------------------

calc_fcn<-function(dataset, colname, fcn){
  ### Generic calculation function for values in a column of a data frame.
  ### Returns a single value (i.e. sum, mean, median)
  ### Inputs: 
  ###   dataset - a data frame
  ###   colname - the name of a column in the data frame (string)
  ###   fcn - handle of function that returns a single value (ex: sum)
  
  dataset[,colname]<-as.numeric(dataset[,colname])
  dataset<-na.omit(dataset)
  
  val<-fcn(dataset[[colname]])
  
  return(val)
}

###IN PROGRESS
data_trans<-function(dataset, colname, fcn){
  ### Generic calculation function applied to each value in a column
  ### Inputs: 
  ###   dataset - a data frame
  ###   colname - the name of a column in a data frame (string)
  ###   fcn - handle of a function to be applied to each value in data frame
  
  dataset[,colname]<-as.numeric(dataset[,colname])
  dataset<-na.omit(dataset)
  
  func<-
  
  dataset[,colname]<-sapply(dataset[[colname]], function(x) fcn(x)) #Issue: is removing the NA
  
  return(dataset)
}

###The following functions are from solver-diagnostics.R in solver-diagnostics GCAM branch

fxtransform <- function(x) {
  ### Transform F(x) values for better visualization
  ###
  ### This transformation is a log scale for small values that switches to a linear 
  ###  scale for larger values.  Magnitudes less than
  ###  the solution tolerance are flushed to zero.  Magnitudes greater than
  ###  that but < 1 are divided by the tolerance, have the base-10 log taken, and
  ###  have their sign preserved.  Magnitudes > 1 are presented linearly (shifted
  ###  to be continuous with the small scale), and magnitudes >10 are clamped.
  ftol  <- 1.0e-3                     # threshold for considering a market "solved"
  signx <- sign(x)
  magx  <- abs(x)
  xx    <- ifelse(magx < ftol, 0,
                  ifelse(magx < 1, log10(magx)-log10(ftol),
                         ifelse(magx < 10, (magx-1)-log10(ftol), 9-log10(ftol))))
  signx*xx                            # return value
}

### Return a transform function that takes the magnitude and clips it to a maximum value
clipped.mag.transform <- function(maxmag=10) {
  function(x) {
    magx <- abs(x)
    ifelse(magx>maxmag, maxmag, magx)
  }
}

### Return a transform that clamps the values to two bounds
clamp.transform <- function(xmin=-100, xmax=100) {
  function(x) {
    pmax(xmin, pmin(x, xmax))
  }
}

### Return a transform function that gives sign(x)*log(x/xmin).
### x-values less than xmin are flushed to zero, and x-values greater
### than xmax are clipped to xmax
signed.log.transform <- function(xmin=1e-4, xmax=100) {
  function(x) {
    signx <- sign(x)
    absx  <- pmax( pmin(abs(x), xmax), xmin)
    signx * log10(absx/xmin)
  }
}

### Transform deltax and deltafx values for better visualization
deltatransform <- function(x, maxmag=10) {
  magx <- abs(x)
  pmin(magx, maxmag)
}

#-----------------------------------------------------------------
# COLOR PALETTE FUNCTIONS
#-----------------------------------------------------------------

fxcolormap <- function(n=51, controlpts=c(-10,-3,0,3,10)) {
  ### Create colormap for visualizing f(x) values.
  ###
  ### The hue will be piecewise over the four intervals defined by the
  ###  five control points (they need not be equally spaced).  The saturation
  ###  will ramp down from 1 to 0 on the interval from the first to the third 
  ###  control points, then back up to 1 on the rest of the interval.  If 
  ###  centered on 0 (like the default) this will result in a colormap where
  ###  the magnitude of f(x) is represented by the saturation and the hue localizes  
  ###  the value to one of four intervals.
  ###
  ### Inputs:
  ###   n - number of steps in the colormap
  ### controlpts - intervals over which the hue and saturation change.
  ###
  ### Return value:  Vector of colors
  xlo <- controlpts[1]
  x1  <- controlpts[2]
  xm  <- controlpts[3]
  x2  <- controlpts[4]
  xhi <- controlpts[5]
  
  x = seq(0,n-1) * (xhi-xlo)/(n-1) + xlo
  ## Hue is piecewise constant on four intervals
  h1 <- ifelse(x<x1, 0, 90/360)
  h2 <- ifelse(x>x2, 240/360, 180/360)
  H <- ifelse(x<xm, h1, h2)
  
  ## Use "option 2 for the saturation"
  eps <- 1.0e-8        # protect against roundoff giving vaues slightly larger than 1
  S <- ifelse(x<xm, 1-(x-xlo)/(xm-xlo), (x-xm)/(xhi-xm+eps))
  
  ## Constant 1 for value
  hsv(H, S, 1)
}



#-----------------------------------------------------------------
# MAPPING WRAPPER FUNCTIONS
#-----------------------------------------------------------------

plot_basic<-function(dta,extent=EXTENT_WORLD){
  ### Wrapper for basic mapping functions: plotting lat/lon lines (graticule)
  ### and polygons
  ### Inputs: 
  ###   dta - dataframe of geometric data (created w/ fortify function from geoJSON)
  ###   extent - bounding box
  ### Output: 
  ###   mp - ggplot2 map object
  
  grat<-gen_grat()
  
  #Get polygons that fall within bounding box
  dat<-get_bbox_polys(dataset = dta, bbox = extent)
  
  #Plot graticule and polygons
  mp<-ggplot()+
    geom_path(data=grat,aes(long,lat,group=group,fill=NULL),color=LINE_GRAT)+
    geom_polygon(data=dat, aes(long,lat,group=group),fill=RGN_FILL, color=LINE_COLOR)
  
  return(mp)
}


project_map<-function(mp, prj, extent, orientation=NULL){
  ### Transform map coordinates to specified projection
  ### Inputs: 
  ###   mp - ggplot2 map object
  ###   prj - A proj4 string or, if orthographic, the string "orthographic"
  ###   extent - bounding box of map
  ###   orientation - if using orthographic projection, orientation c(lat0,lon0,radius)
  ### Outputs: 
  ###   mp - transformed ggplot2 map object
  
  if (grepl("ortho",prj)){
    mp<-mp+
      coord_map(proj="orthographic", orientation=orientation, xlim=c(extent[1], extent[2]), 
                ylim=c(extent[3], extent[4]))
  }else{
    mp<-mp+
      coord_proj(proj=prj, xlim=c(extent[1], extent[2]), ylim=c(extent[3], extent[4]))
  }
  return(mp)
}

add_theme<-function(mp, title){
  ### Wrapper for ggplot2's theme specifications
  ### Hard-coded for now; possibly can make this customizable
  ### Inputs: 
  ###   mp - ggplot2 map object
  ###   title - string; title of map
  ### Outputs:  
  ###   mp - ggplot2 map object with theme attributes specified
  
  mp<-mp+
    theme(panel.border=element_rect(color=LINE_COLOR, fill=NA), 
          panel.background=PANEL_BACKGROUND,
          panel.grid=PANEL_GRID,
          axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT, 
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size=14, face="bold"),
          legend.position = LEGEND_POSITION, 
          legend.key=element_rect(color='black')
    )+
    labs(title=title, x=XLAB, y=YLAB)
  
  return(mp)
}

#-----------------------------------------------------------------
# MAPS
#-----------------------------------------------------------------

basemap<-function(dta, prj=robin, extent=EXTENT_WORLD, title=NULL, orientation=NULL){
  ### Wrapper for mapping functions to produce default map
  ### Inputs: 
  ###   dta - geometric dataframe (fortified geoJSON object)
  ###   prj - proj4 string (except for orthographic)
  ###   extent - bounding box
  ###   title - title of map (string)
  ###   orientation - c(lat0,lon0, radius); for orthographic projections only
  ### Outputs: 
  ###   mp - ggplot2 map object
  
  mp<-plot_basic(dta, extent)
  
  #Reproject map
  mp<-project_map(mp, prj, extent, orientation)
  
  # Thematic details
  mp<-add_theme(mp, title)
  
  return(mp)
}

map_category<-function(dta, prj, colors, colname="id", extent=EXTENT_WORLD, orientation= NULL, title=NULL, qtitle=NULL){
  ### Produces map colored by categorical data
  ### Inputs: 
  ###   dta - geometric dataframe (fortified geoJSON object)
  ###   prj - proj4 string (special case: orthographic)
  ###   colors - color palette
  ###   colname - category column
  ###   extent - bounding box
  ###   orientation - orientation - c(lat0,lon0, radius); for orthographic projections only
  ###   title - map title
  ###   qtitle - optional title for map legend
  
  #Get polygons that fall within bounding box
  mappolys<-get_bbox_polys(dataset = mapdata, bbox = extent)
  
  mp<-plot_basic(mappolys, extent)

  mp<-mp+
    geom_polygon(data=mappolys, aes_string(x='long',y='lat',group='group', factor(colname), fill=factor(colname)), 
                 color=LINE_COLOR)+
    scale_fill_manual(values=colors, name=qtitle)
  
  mp<-project_map(mp, prj, extent)
  mp<-add_theme(mp, title)
    
  return(mp)
}

map_query<-function(mapdata, colname, colors, xform= identity, values=NULL, prj=robin, 
                    extent=EXTENT_WORLD, orientation=NULL, title=NULL, qtitle=NULL){
  ### Produces map colored based on numeric data (sequential, diverging data)
  ### Inputs: 
  ###   mapdata - geometric data frame (fortified geoJSON object); typically w/ GCAM scenario data attached
  ###   colname - string; name of column of interest
  ###   colors - vector of colors to use
  ###   xform - transformation to perform on column of interest
  ###   values - 
  ###   prj, extent, orientation, title, qtitle -- defined in map_category function
  ### Outputs: 
  ###   mp - ggplot2 map object
  
  #Convert data of interest to numeric
  mapdata[, colname]<-as.numeric(mapdata[,colname])
  
  #Get polygons that fall within bounding box
  mappolys<-get_bbox_polys(dataset = mapdata, bbox = extent)
  
  mp<-plot_basic(mappolys, extent) #Mapdata or mappolys? 
  
  mappolys[[colname]]<-xform(mappolys[[colname]])
  
  #Plot data
  mp<-mp+
      geom_polygon(data = mappolys,aes_string("long", "lat", group="group", fill=colname), 
                 color=LINE_COLOR)+
    scale_fill_gradientn(name=qtitle, colors=colors, values=values, guide = GUIDE, space=SPACE,
                      na.value = NA_VAL, breaks=calc_breaks(mappolys,colname),
                      labels=c(calc_breaks(mappolys,colname)))  #Need fcn for factor to divide by
                      
  #Reproject map
  mp<-project_map(mp, prj, extent, orientation)
  mp<-add_theme(mp, title)
  
  return(mp)
  
}

#-----------------------------------------------------------------
#Map #4: Display points

# displayPoints<-function(datatable, map, colname, mapname='map'){
#   #Basic function to overlay points on a map
#   #Assumes data table is already formatted --i.e. if geoJSON point data, converted to data frame
#   mp<-basemap(map=map)
#   mp<-mp+
#     geom_point(aes_string(x='coords.x1', y='coords.x2', size=colname, 
#                           color=colname), data=datatable)+
#     scale_size_discrete(range=c(4,7))
#   return(mp)
# }
#
#-----------------------------------------------------------------
# MISC UTILS
#-----------------------------------------------------------------
# save_image - Wrapper for ggsave function with desired parameters as defaults. 
# Arguments - mp - map object that you want to save
#             fn - file name you want to save it as

save_image<-function(mp, fn){
  ggsave(filename = paste("output-files/",fn, EXTENSION, sep=""), plot = mp, dpi=DPI, width=WIDTH, height=HEIGHT)
}
    


