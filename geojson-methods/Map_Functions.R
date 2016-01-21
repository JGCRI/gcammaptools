##MAP FUNCTIONS 

setwd('C:/Users/ledn787/Desktop/Visualization_Work/gcam-viz/')

#Source Scripts functions
for (fn in list.files("scripts", full.names = T)){
  source(fn)
}

source("geojson-methods/Map_Params.R")

#Required libraries
#------------------
library("rgdal")
library("ggplot2")
library("ggalt")
library("graticule")
library("RColorBrewer")
#library("rgeos")
#library("maptools")

#-----------------
#DATA PROCESSING FUNCTIONS
#TODO - still relevant?

addRegionID<-function(datafile, lookupfile='RgnNames.txt', provincefile='none', drops='none') {
  #Add region ID to data table using lookup file
  #TODO: Generalize this to multiple types of data file returned; have result be 
  #standardized output table. 
  
  if (provincefile != 'none'){
    datatable<-translateProvince(datafile, provincefile)
  }
  else{
    datatable<-read.csv(datafile, strip.white=T)
  }
  if (drops != 'none'){
    datatable<-dropRegions(datatable, drops)
  }
  
  lookuptable<-read.csv(lookupfile, strip.white=T, stringsAsFactors = F)
  
  finaltable<-merge(datatable, lookuptable, by.x=colnames(datatable)[1], by.y=colnames(lookuptable)[1] )
  colnames(finaltable)[ncol(finaltable)]<-'id'
  finaltable$id<-as.character(finaltable$id)
  
  #Add null vector row to end to account for GCAM region 0
  nullvec<-c('0', 2:ncol(finaltable))
  nullvec[2:ncol(finaltable)]<-NA
  
  finaltable<-rbind(finaltable, nullvec)
  finaltable$id[nrow(finaltable)]<-'0'
  
  return(finaltable)
}

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


dropRegions<-function(datatable, drops){
  #Drop regions listed in drops file from data frame
  dr<-read.csv(drops, strip.white=T, header=F)
  dr<-as.character(dr$V1)
  
  datatable$region<-as.character(datatable$region)
  dr<-match(dr, datatable$region)
  dr<-na.omit(dr)
  
  datatable<-datatable[-c(dr), ]
  
  return(datatable)
}

#---------------------------------------------------------------------------
# MAPPING UTILS
#---------------------------------------------------------------------------
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
# TODO - extra args for latint, longint?

gen_grat<-function(bbox,longint=20,latint=30){
  require(graticule)
  
  #Generate graticule as sp matrix object 
  lons=seq(bbox[1],bbox[2],by=longint)
  lats=seq(bbox[3],bbox[4],by=latint)
  
  grat<-graticule(lons,lats,xlim=range(lons)+c(-20,20),ylim=range(lats)+c(-20,20))
  
  #Convert to ggplot2-friendly format
  grat<-fortify(grat)
  
  return(grat)
  
}

#-----------------------------------------------------------------
# MAPPING FUNCTIONS
#-----------------------------------------------------------------
# basemap_world - default world map
# params: dta - Data; fortified geoJSON object
#         save - bool for whether to save to output
#         prj - projection; from list defined in Map_Params; or custom PROJ4 string
#         extent - extent of map in long/lat; numeric vector of length 4
#         title - title of map; optional
#         fn - file name of map to save as
# TODO - 3 dots abbreviation for extra args?

basemap_world<-function(dta, save=0, prj=robin,extent=EXTENT_WORLD, title=NULL, fn="map1"){
  
  #Generate graticule
  grat<-gen_grat(bbox=extent)
  
  #Plot graticule
  mp<-ggplot()+
    geom_path(data=grat,aes(long,lat,group=group,fill=NULL),color=LINE_GRAT)
  
  #Plot polygons and aesthetics
  mp<-mp+
    geom_polygon(data=dta, aes(long,lat,group=group),fill=RGN_FILL, color=LINE_COLOR)+
    theme(legend.position=LEGEND_POSITION)+
    theme(panel.border=PANEL_BORDER, panel.background=PANEL_BACKGROUND,
          panel.grid=PANEL_GRID,
          axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
    labs(title=title)+
    labs(x=NULL,y=NULL)
  
  #Reproject map
  mp<-mp+
    coord_proj(proj=prj)
  
  if (save==1){
    ggsave(filename = paste("output-files/",fn, EXTENSION, sep=""), plot = mp, dpi=DPI, width=WIDTH, height=HEIGHT)
  }
  
  return(mp)
}

#-----------------------------------------------------------------
# basemap_regional - default regional map; uses Albers Equal Area projection
# TODO - automatically select lat0, lat1 based on projection name
# TODO - 3 dots abbreviation for extra args?

basemap_regional<-function(dta, extent, save=0, prj="albers", title=NULL, fn="map1"){
  #Generate graticule according to extent
  grat<-gen_grat(extent)
  
  #Get polygons that fall within bounding box
  dat<-get_bbox_polys(dataset = dta, bbox = extent)
  
  #Plot map
  mp<-ggplot()+
    geom_path(data=grat,aes(long,lat,group=group,fill=NULL),color=LINE_GRAT)+
    geom_polygon(data=dat, aes(long,lat,group=group),fill=RGN_FILL, color=LINE_COLOR)+
    theme(legend.position=LEGEND_POSITION)+
    theme(panel.border=element_rect(color=LINE_COLOR, fill=NA), panel.background=PANEL_BACKGROUND,
          panel.grid=PANEL_GRID,
          axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
    coord_map(prj, lat0=aea.lat_0, lat1=aea.lat_1, xlim = c(extent[1], extent[2]), ylim=c(extent[3],extent[4]))+
    labs(title=title)+
    labs(x=NULL,y=NULL)
    
  if (save==1){
    ggsave(filename = paste("output-files/",fn, EXTENSION, sep=""), plot = mp, dpi=DPI, width=WIDTH, height=HEIGHT)
  }
    
  return(mp)
  
}
#-----------------------------------------------------------------

#TODO - Qualitative map (palette)
#       Colored choropleth
#Colored by region
qualMap<-function(map, save=0, mapname='map1', title=NULL){

  
  #Expand rcolorbrewer map palette
  #TODO Color palette needs work
  colorcount= length(unique(map$id))
  getpalette= colorRampPalette(brewer.pal(12, 'Set3'))
  
  #Layer qualitative map over basemap
  mp<-basemap(map)+
    geom_polygon(data=map, aes(x=long,y=lat,group=group, factor(id), fill=factor(id)), color=LINE_COLOR)+
    theme(legend.position='none')+
    scale_fill_manual(values=getpalette(colorcount))
    
  #Save to desired file format
  if(save==1){
    ggsave(mp, filename=paste(mapname, EXTENSION), width=WIDTH, height=HEIGHT, 
           type=TYPE)
  }
  
  return(mp)
}

#-----------------------------------------------------------------
# choropleth - color map for data
#   Features - diverging, 


choropleth<-function(datatable, colname, save=0, mapname = 'map1', title='Map1', legendtitle = 'Population'){
  
  
  
  
  
  
  
  #Basic function to create chloropleth map
  #TODO: correct color scheme, change axes labels
  #Add graphics and projection definitions to be utilized. 
  datatable[, colname]=as.numeric(datatable[,colname])
  
  #Zoom to manually selected coordinates
  x1<-X.MAX
  x2<-X.MIN
  y1<-Y.MAX
  y2<-Y.MIN
  
  
  mp<- ggplot()+
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=colname), data=datatable,
                 color = LINE_COLOR,environment=environment())+
    coord_equal(xlim=c(x1,x2), ylim=c(y1,y2))
  
  mp<-mp+
    theme(panel.border=PANEL_BORDER, panel.background=PANEL_BACKGROUND,
          panel.grid = PANEL_GRID,
          axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT,
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size=14, face="bold"),
          legend.position = LEGEND_POSITION, 
          legend.key=element_rect(color='black')
          )+
    labs(title=title, x=XLAB, y=YLAB)+
    scale_fill_gradient(name=legendtitle, low=LOW_COLOR, high=HIGH_COLOR, guide = 'colourbar', 
                        na.value = "white")

    
  if (save==1){
    ggsave(mp, filename=paste(mapname,EXTENSION), width=WIDTH, height=HEIGHT, type=TYPE)
  }
  else{
    return(mp)
  }
}

#--------------
#Map #4: Display points

# displayPoints<-function(datatable, map, colname, mapname='map', save=1){
#   #Basic function to overlay points on a map
#   #Assumes data table is already formatted --i.e. if geoJSON point data, converted to data frame
#   mp<-basemap(map=map)
#   mp<-mp+
#     geom_point(aes_string(x='coords.x1', y='coords.x2', size=colname, 
#                           color=colname), data=datatable)+
#     scale_size_discrete(range=c(4,7))
#   return(mp)
# }



