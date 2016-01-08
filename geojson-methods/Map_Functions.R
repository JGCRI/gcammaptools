##MAP FUNCTIONS 

setwd('C:/Users/ledn787/Desktop/Visualization_Work/gcam-viz/')

#Required libraries
#------------------
library("rgdal")
library("ggplot2")
library("ggalt")
library("grid")
library("RColorBrewer")
library("rgeos")
library("maptools")

#-----------------
#Graphics Specifications (for exporting finished maps)
WIDTH = 6
HEIGHT= 4
TYPE= 'cairo-png'
EXTENSION= '.png'


#-----------
#Basic Map Specifications: 

#Default Colors
LINE_COLOR<-"black"
RGN_FILL<-"white"

#Choropleth Colors
LOW_COLOR = 'light green'
HIGH_COLOR = 'dark green'
TRANSFORMATION = 'none' #Mathematical transformation (i.e. 'log', 'sqrt')

#Extent of map (lat-long coordinates)
X.MAX<-180
X.MIN<--180
Y.MAX<-90
Y.MIN<--90

#Background
PANEL_BORDER<-element_blank()
PANEL_BACKGROUND<-element_blank()
PANEL_GRID<-element_line(colour = 'black')
AXIS_TICKS<-element_blank()
AXIS_TEXT<-element_blank()
XLAB<-NULL
YLAB<-NULL

#Legend
LEGEND_POSITION='bottom'


#---------------
#DATA PROCESSING FUNCTIONS

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

#------------------------------
#Map 1 & 2: Basic Maps

basemap<-function(map, save=0, mapname='map1', title=NULL){
  #Displays basic map
  #Zoom to manually selected coordinates if desired
  x1<-X.MAX
  x2<-X.MIN
  y1<-Y.MAX
  y2<-Y.MIN
  
  
  #Plot basic map features
  mp<-ggplot()+
    geom_polygon(data=map, aes(x=long, y=lat, group=group), fill=RGN_FILL, color=LINE_COLOR)+
    coord_equal(xlim=c(x1,x2), ylim=c(y1,y2))

  #Add thematic specifications
  mp<- mp+
    theme(legend.position=LEGEND_POSITION)+
    theme(panel.border=PANEL_BORDER, panel.background=PANEL_BACKGROUND,
          panel.grid=PANEL_GRID,
          axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
    labs(title=title, x=XLAB, y=YLAB)
    
  #Save to desired file format
  if(save==1){
    ggsave(mp, filename=paste(mapname, EXTENSION), width=WIDTH, height=HEIGHT, 
    type=TYPE)
  }
  
  return(mp) 

}

#Colored by region
qualMap<-function(map, save=0, mapname='map1', title=NULL){
  x1<-X.MAX
  x2<-X.MIN
  y1<-Y.MAX
  y2<-Y.MIN
  
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

#----------
#Map 3: Choropleth

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


#-----------------------
#Test Code
# #GCAM 32-region map w/o Taiwan - Choropleth 
# 
# #Read in map
# gcam_map1<-readOGR(dsn = 'input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson', layer="OGRGeoJSON")
# 
# gc1<-fortify(gcam_map1, region="GCAM_ID")
# 
# mp<-basemap(gc1)
# mp<-mp+coord_proj("+proj=eck3")
# 
# mp
# 
# #Transform to Eckert III Projection
# gcam_map1Eck3<-spTransform(gcam_map1,CRS("+proj=eck3 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# 
#                            
# #Convert to data frame
# gcam_map1_2<-fortify(gcam_map1, region = "GCAM_ID")
# 
# #Load data file and match region name to gcam id
# df1<-addRegionID(datafile = '#INSERT DATA FILE HERE', lookupfile = 'GCAM_32_wo_TaiwanLookupTable.csv')
# 
# #Merge the dataframes and plot as choropleth
# mp<-merge(x=gcam_map1_2, y=df1, by='id')
# mp1<-choropleth(mp, '#Name of data column in frame')
# 
