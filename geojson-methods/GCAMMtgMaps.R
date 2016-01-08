###Generate Maps for GCAM Map Document
###TODO: Update Map_Functions with graticule
    #Organize existing code here into functions
    #Add paths for saving files


setwd('C:/Users/ledn787/Desktop/Visualization_Work/gcam-viz/')

require(rgdal)
require(ggplot2)
require(ggalt)
require(graticule)

source("geojson-methods/Map_Functions.R")

#MAPSET 1: World MAP, Eckert III Projection
dat1<-readOGR("input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson", "OGRGeoJSON")
gc1<-fortify(gcam_map1, region="GCAM_ID")
mp1<-basemap(gc1)

#Eckert 3 Projection with graticule
prj<-"+proj=eck3"

#Define graticule
lons = seq(-180,180,by=20)
lats= seq(-90,90,by=30)
xl= range(lons)
yl= range(lats)

grat<-graticule(lons,lats,xlim=xl,ylim=yl)
grat<-fortify(grat,long=lons,lat=lats)

#Plot Graticule
plot_grat<-function(dat){
  mp<-ggplot()+
    geom_path(data=dat,aes(long,lat,group=group,fill=NULL),color="grey50")
  return(mp)
}

mp1<-ggplot()+
  geom_path(data=grat, aes(long,lat,group=group,fill=NULL), color="grey50")

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
###In progress

prj4<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

lons=seq(-180,-60,by=20)
lats=seq(0,60,by=30)


USA.grat<-graticule(lons,lats,proj=prj4,xlim=range(lons)+c(-10,10),ylim=range(lats)+c(-10,10))
USA.grat<-fortify(USA.grat)

mpUS<-mpUS+
  geom_path(data=USA.grat, aes(long,lat,group=group,fill=NULL), color="black")

mpUS<-ggplot()+
  geom_polygon(data=gc1, aes(long,lat,group=group), fill="grey", color="black")+
  coord_map("albers", lat0=30, lat1=40, xlim = c(-120,-60), ylim=c(20,60))+
  theme(legend.position=LEGEND_POSITION)+
  theme(panel.border=PANEL_BORDER, panel.background=PANEL_BACKGROUND,
        panel.grid=PANEL_GRID,
        axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
  labs(title='Albers Equal-Area, USA')+
  labs(x=NULL,y=NULL)

mpUS

mpUS
#MAPSET 3: Latin America, Africa Superregions (Ortho)
#MAPSET 4: Polar (??)


