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
#DATA PROCESSING FUNCTIONS
#---------------------------------------------------------------------------

addRegionID<-function(datatable, lookupfile='RgnNames.txt', provincefile='none', drops='none') {
  #Add region ID to data table using lookup file
  #TODO: Generalize this to multiple types of data file returned; have result be 
  #standardized output table. 
  
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
#---------------------------------------------------------------------------

translateProvince<-function(datafile, provincefile){
  #Replace province abbreviations with full province names
  datatable<-read.csv(datafile, strip.white=T)
  provincetable<-read.csv(provincefile, strip.white=T)
  
  datatable$region<-as.character(datatable$region)
  provincetable$province<-as.character(provincetable$province)
  provincetable$province.name<-as.character(provincetable$province.name)
  
  datatable$region<-ifelse(is.na(provincetable$province.name[match(datatable$region, provincetable$province)]), 
                           datatable$region, 
                           provincetable$province.name[match(datatable$region, provincetable$province)])
  
  return(datatable)
} 

#---------------------------------------------------------------------------

dropRegions<-function(datatable, drops){
  #Drop regions listed in drops file from data frame
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
# ##TODO - create border around ellipses
# get_bbox_polys - Returns a list of polygons that intersect bounding box
# params: dataset - dataframe of map geometry created with fortify function
#        bbox - numeric vector (long_min, long_max, lat_min, lat_max)
# Issue: results change depending on bbox set. Need to fix. 

get_bbox_polys<-function(dataset, bbox){
  #Parse bbox and define functions
  fxlons<-in_range(bbox[1],bbox[2])
  fxlats<-in_range(bbox[3],bbox[4])
  
  #Find longitudes in long range
  lons<-sapply(dataset$long, function(x) fxlons(x))
  lats<-sapply(dataset$lat, function(x) fxlats(x))
  
  ids<-intersect(dataset$id[lons], dataset$id[lats])
  newdata<-dataset[dataset$id %in% ids,]
  
  return(newdata)
}

#--------------------------------------------------------------------------
#in_range - Returns function to determine whether x is in range a,b
#params: a,b - can be int or numeric; a<=b

in_range<-function(a,b){
  function(x) x>=a && x<=b
}

#------------------------------------------------------------------
#gen_grat - generate graticule (long/lat lines) given bbox
#params: bbox - numeric vector (long_min, long_max, lat_min, lat_max)
#       longint - interval between longitude lines
#       latint - interval between latitude lines

gen_grat<-function(bbox=EXTENT_WORLD,longint=20,latint=30){
  require(graticule)
  
  #Generate graticule as sp matrix object 
  lons=seq(bbox[1],bbox[2],by=longint)
  lats=seq(bbox[3],bbox[4],by=latint)
  
  grat<-graticule(lons,lats,xlim=range(lons),ylim=range(lats))
  
  #Convert to ggplot2-friendly format
  grat<-fortify(grat)
  
  return(grat)
  
}

#------------------------------------------------------------------
# calc_breaks - Calculate legend breaks
calc_breaks<-function(mapdata, colname, nbreaks=4){
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

#------------------------------------------------------------------
# calc_fcn - Generic calculation function for values in a column of a data frame. Returns 
#           a single value (i.e. sum, mean, median)
#
# Arguments - dataset- a data frame
#             colname - the name of a column in the data frame (string)
#             fcn - an operation that returns a single value

calc_fcn<-function(dataset, colname, fcn){
  dataset[,colname]<-as.numeric(dataset[,colname])
  dataset<-na.omit(dataset)
  
  val<-fcn(dataset[[colname]])
  
  return(val)
}

#------------------------------------------------------------------
# data_trans - Generic calculation function applied to each value in a column
#
data_trans<-function(dataset, colname, fcn){
  dataset[,colname]<-as.numeric(dataset[,colname])
  dataset<-na.omit(dataset)
  
  dataset[,colname]<-lapply()
  
}

#------------------------------------------------------------------
#parse_proj_string - parse projection information string and pass 
# to appropriate function (coord_map or coord_proj). 
# Done because coord_map is preferable to coord_proj (coord_proj cannot set limits)
# projection is not included. 
# Deprecated for now bc coord_proj is updated; however, they have not pushed to CRAN yet; 
# have to download from github repository using install_github from devtools package. 

#Todo: process variable number of parameters (lat_0, lat_1, lon_0, etc)
# a<-parse_proj_string(aea)
# 
# parse_proj_string<-function(proj4){
#   params<-strsplit(proj4, split=" ")
#   params<-params[[1]]
#   params<-strsplit(params, "=")
#   
#   err<-"subscript out of bounds"
#   
#   proj<-params[grepl("+proj", params)][[1]][2]
#   lat_0<-tryCatch(params[grepl("+lat_0", params)][[1]][2], error=function(err){NULL})
#   lat_1<-tryCatch(params[grepl("+lat_1", params)][[1]][2], error=function(err){NULL})
#   lon_0<-tryCatch(params[grepl("+lon_0", params)][[1]][2], error=function(err){NULL})
#   
#   if ((is.null(lat_0))|(is.null(lon_0))){
#     orientation<-NULL
#   }else{
#     orientation<-c(lat_0,lon_0,0)
#   }
#   
#   num<-ifelse(proj %in% coord_map_projs, 1, ifelse(proj %in% names(coord_map_projs), 1, 2))
#   proj<-ifelse(proj %in% names(coord_map_projs), coord_map_projs[proj][1], proj)
#   
#   vals<-c(num, proj, lat_0, lat_1, lon_0, orientation)
#   vals<-addNames("type", "proj", "lat_0", "lat_1", "lon_0", "orientation_1", "orientation_2", "orientation_3")
#   
#   if (proj %in% coord_map_projs){
#     return(c(1, proj))
#   }
#   else if (proj %in% names(coord_map_projs)){ #Return correct name for coord_map fcn
#     return(c(1,coord_map_projs[proj][[1]], lat_0, lat_1, lon_0, orientation)) 
#   }
#   else{
#     return(2) #Use coord_proj fcn and unaltered proj4 string
#   }
# }

#-----------------------------------------------------------------
# MAPPING FUNCTIONS
#-----------------------------------------------------------------
# plot_basic - Basic wrapper for ggplot2 map. Plots lat/long lines (graticule)
#             and polygons. Returns a map object for further modification. 
# params: dta - Data, fortified geoJSON object
#         extent - long/lat boundaries of map (long1, long2, lat1, lat2)

plot_basic<-function(dta,extent=EXTENT_WORLD){
  #Generate graticule
  grat<-gen_grat()
  
  #Get polygons that fall within bounding box
  dat<-get_bbox_polys(dataset = dta, bbox = extent)
  
  #Plot graticule and polygons
  mp<-ggplot()+
    geom_path(data=grat,aes(long,lat,group=group,fill=NULL),color=LINE_GRAT)+
    geom_polygon(data=dat, aes(long,lat,group=group),fill=RGN_FILL, color=LINE_COLOR)
  
  return(mp)
}

#-----------------------------------------------------------------

project_map<-function(mp, prj, extent){
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
#-----------------------------------------------------------------

add_theme<-function(mp, title){
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
# basemap - default map
# params: dta - Data; fortified geoJSON object
#         prj - projection; from list defined in Map_Params; or custom PROJ4 string
#         extent - extent of map in long/lat; numeric vector of length 4
#         title - title of map; optional
#         orientation - for orthographic maps only
# TODO - 3 dots abbreviation for extra args?

basemap<-function(dta, prj=robin, extent=EXTENT_WORLD, title=NULL, orientation=NULL){
  
  mp<-plot_basic(dta, extent)
  
  #Reproject map
  mp<-project_map(mp, prj, extent)
  
  # Thematic details
  mp<-add_theme(mp, title)
  
  return(mp)
}

#-----------------------------------------------------------------
#TODO - Qualitative map (palette)--make palette better looking
#    Colored by region   
#

qualMap<-function(dta, prj, palette, extent=EXTENT_WORLD, orientation= NULL, title=NULL){
  
  mp<-plot_basic(dta, extent)

  mp<-mp+
    geom_polygon(data=dta, aes(x=long,y=lat,group=group, factor(id), fill=factor(id)), color=LINE_COLOR)+
    scale_fill_manual(values=palette)
  
  mp<-project_map(mp, prj, extent)
  
  mp<-add_theme(mp, title)
    
  return(mp)
}

#-----------------------------------------------------------------
# map_sequential - Visualize sequential data 

map_sequential<-function(mapdata, colname, prj=robin, extent=EXTENT_WORLD, orientation=NULL, title=NULL, qtitle=NULL){
  #Convert data of interest to numeric
  mapdata[, colname]<-as.numeric(mapdata[,colname])
  
  #Get polygons that fall within bounding box
  mappolys<-get_bbox_polys(dataset = mapdata, bbox = extent)
  
  mp<-plot_basic(mapdata, extent)
  
  #Plot grat and polgyons
  mp<-mp+
      geom_polygon(data = mappolys,aes_string("long", "lat", group="group", fill=colname), 
                 color=LINE_COLOR)+
    
    #Would really like to make this modular (i.e. can figure out in fcn whether continuous,diverging)
    scale_fill_gradient(name=qtitle, low=LOW_COLOR, high=HIGH_COLOR, guide = 'colourbar', 
                      na.value = NA_VAL, breaks=calc_breaks(mappolys,colname),
                      labels=c(calc_breaks(mappolys,colname))) #Need fcn for factor to divide by
                      
  
  #Reproject map
  mp<-project_map(mp, prj, extent)
  mp<-add_theme(mp, title)
  
  return(mp)
  
}
#-----------------------------------------------------------------
# map_diverging - Visualize diverging data

map_diverging<-function(mapdata, colname, prj=robin, extent=EXTENT_WORLD, orientation=NULL, title=NULL, qtitle=NULL){
  #Convert data of interest to numeric
  mapdata[, colname]<-as.numeric(mapdata[,colname])
  
  #Get polygons that fall within bounding box
  mappolys<-get_bbox_polys(dataset = mapdata, bbox = extent)
  
  mp<-plot_basic(mapdata, extent)
  
  #Plot
  mp<-mp+
      geom_polygon(data = mappolys,aes_string("long", "lat", group="group", fill=colname), #Add calculation function for fill
                 color=LINE_COLOR)+
    
    #Would really like to make this modular (i.e. can figure out in fcn whether continuous,diverging)
    scale_fill_gradient2(name=qtitle, low="blue", mid="white", high="purple", guide = 'colourbar',
                         midpoint=calc_fcn(mappolys, colname, mean),
                        na.value = NA_VAL, breaks=calc_breaks(mappolys,colname),
                        labels=c(calc_breaks(mappolys,colname)))#Need fcn for factor to divide by
  
  mp<-project_map(mp, prj, extent)

  # Thematic details
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
    


