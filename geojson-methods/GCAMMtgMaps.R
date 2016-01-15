###Generate Maps for GCAM Map Document
###TODO: Update Map_Functions with graticule - fix gen_grat function to be consistent
    #Organize existing code here into functions
    #Add paths for saving files


setwd('C:/Users/ledn787/Desktop/Visualization_Work/gcam-viz/')

require(rgdal)
require(ggplot2)
require(ggalt)
require(graticule)

source("geojson-methods/Map_Functions.R")

#-----------------------------------------------------------------
#get_bbox_polys - Returns a list of polygons that intersect bounding box
#params: dataset - dataframe of map geometry created with fortify function
#        bbox - numeric vector (long_min, long_max, lat_min, lat_max)
#Issue: results change depending on bbox set. Need to fix. 

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

#------------------------------------------------------------------
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

#MAPSET 1: World MAP, Eckert III Projection
dat1<-readOGR("input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson", "OGRGeoJSON")
gc1<-fortify(dat1, region="GCAM_ID")
mp1<-basemap(gc1)

#Eckert 3 Projection with graticule
prj<-"+proj=eck3"

#Generate graticule
bbox1=c(-180,180,-90,90)

grat<-gen_grat(bbox1)

#Plot Graticule
plot_grat<-function(dat){
  mp<-ggplot()+
    geom_path(data=dat,aes(long,lat,group=group,fill=NULL),color="grey50")
  return(mp)
}

mp1<-ggplot()+
  geom_path(data=grat, aes(long,lat,group=group,fill=NULL), color="grey50")

mp1
#Add polygons OVER graticule
mp1<-mp1+
  geom_polygon(data=gc1, aes(long,lat,group=group),fill="grey", color="black")+
  coord_equal(xlim=c(X.MIN,X.MAX), ylim=c(Y.MIN,Y.MAX))+
  theme(legend.position=LEGEND_POSITION)+
  theme(panel.border=PANEL_BORDER, panel.background=PANEL_BACKGROUND,
        panel.grid=PANEL_GRID,
        axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
  labs(title='Eckert III World')+
  labs(x=NULL,y=NULL)

#Reproject Map
mp1<-mp1+
  coord_proj(proj=prj)
mp1

ggsave(filename = 'Eck3.png', plot = mp1, dpi=300/2, width=2560/300, height=1440/300)

#Winkel-Tripel
proj2="+proj=wintri"

mp2<-mp1+
  coord_proj(proj=proj2)+
  labs(title="Winkel-Tripel World")

mp2
ggsave(filename = 'WinTri.png', plot = mp2, dpi=300/2, width=2560/300, height=1440/300)

#Robinson
proj3="+proj=robin"

mp3<-mp1+
  coord_proj(proj=proj3)+
  labs(title="Robinson World")

mp3
ggsave(filename = 'Robin.png', plot = mp3, dpi=300/2, width=2560/300, height=1440/300)

#MAPSET 2: USA, India, China (Conic)

#Map 2A USA Albers Equal Area
prj4<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

bbox2<-c(-120,-70,0,60)
USA.grat<-gen_grat(bbox2)

usd2<-get_bbox_polys(gc1, bbox2)

m<-ggplot()+
  geom_path(data=USA.grat, aes(long,lat,group=group,fill=NULL), color="grey50")+
  geom_polygon(data=usd2, aes(x=long,y=lat,group=group), fill="grey", color="black")+
  theme(legend.position=LEGEND_POSITION)+
  theme(panel.border=element_rect(color="black", fill=NA), panel.background=PANEL_BACKGROUND,
        panel.grid=PANEL_GRID,
        axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
  coord_map("albers", lat0=30, lat1=40, xlim = c(-120,-70), ylim=c(20,60))+
  labs(title='USA, Albers Equal Area')+
  labs(x=NULL,y=NULL)

m

ggsave(filename = 'USA_AEA.png', plot = m, dpi=300/2, width=2560/300, height=1440/300)

#Map 2B China 

bbox3<-c(77,135,20,53)
China.grat<-gen_grat(bbox3,longint = 20, latint=15)
chndat<-get_bbox_polys(gc1,bbox3)

c<-ggplot()+
  geom_path(data=China.grat, aes(long,lat,group=group,fill=NULL), color="grey50")+
  geom_polygon(data=chndat, aes(x=long,y=lat,group=group), fill="grey", color="black")+
  theme(legend.position=LEGEND_POSITION)+
  theme(panel.border=element_rect(color="black", fill=NA), panel.background=PANEL_BACKGROUND,
        panel.grid=PANEL_GRID,
        axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
  coord_map("albers", lat0=30, lat1=40, xlim = c(bbox3[1],bbox3[2]), ylim=c(bbox3[3],bbox3[4]))+
  labs(title='China, Albers Equal Area')+
  labs(x=NULL,y=NULL)

c
ggsave(filename = 'China_AEA.png', plot = c, dpi=300/2, width=2560/300, height=1440/300)


#MAPSET 3: Latin America, Africa Superregions (Ortho)
#MAPSET 4: Polar (??)


