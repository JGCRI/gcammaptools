###Generate Maps for GCAM Map Document
###TODO: 
    #Add paths for saving files


setwd('C:/Users/ledn787/Desktop/Visualization_Work/gcam-viz/')

require(rgdal)
require(ggplot2)
require(ggalt)
require(graticule)

#Core map functions
source("geojson-methods/Map_Functions.R")

#-----------------------------------------------------------------
#To geojson: writeOGR(d, layer="",dsn="China_map.geojson",driver="GeoJSON")

###Driver for Data-Processing Functions
#-----------------------------------------------------------------
#Prepare Data
#Scenario Data
tables<-parse_mi_output(fn = "input-data/sample_batch.csv")

#Map Data
dat1<-readOGR("input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson", "OGRGeoJSON")
gc1<-fortify(dat1, region="GCAM_ID")

#Break out sample scenario
prim_en<-process_batch_q(tables, "primary_energy", "Reference", c(fuel="Oil"))
prim_en<-addRegionID(prim_en, "input-data/rgn32/gcam_32_wo_TaiwanLookupTable_clean.csv", drops="input-data/rgn32/drop_regions.txt")

#Merge dataset with map data
map_primen<-merge(gc1, prim_en, by="id")

#-----------------------------------------------------------------

#MAPSET 1: Basemaps - World Maps, various projections, various types
#Map 1A: Eckert III World, Colored by Region
mp1<-map_category(gc1, prj = eck3, colorfcn=qualPalette)
mp1

#Map 1B: Robinson World, Colored by Oil Consumption
mp2<-map_query(map_primen, "X2050", c("white", "red"), prj=robin, title="Robinson World", qtitle="Oil Consumption, 2050")
mp2


#Map 1C: Winkel-Tripel
mp3<-basemap(gc1, prj=wintri, title="Winkel-Tripel World")
mp3


#MAPSET 2: USA, China (Conic)
#Map 2A USA Albers Equal Area
dat2<-readOGR("input-data/rgnusa/gcam_usa.geojson", layer="OGRGeoJSON")
d<-dat2@data
states<-d[d$GCAM_30_re==1,]
state_nm<-states$NAME
state_abbrev<-states$STUSPS
state_id<-states$GEOID

statesdf<-data.frame(state_id, state_abbrev, state_nm)
statesdf<-na.omit(statesdf)
statesdf<-statesdf[1:2]
statesdf<-statesdf[c(2,1)]
statesdf$state_abbrev<-as.character(statesdf$state_abbrev)
statesdf$state_id<-as.numeric(statesdf$state_id)
statesdf$state_id<-sapply(statesdf$state_id, FUN = function(x) x = x+100)


lookup<-read.csv("input-data/rgn32/gcam_32_wo_TaiwanLookupTable_clean.csv")
lookup<-rbind(lookup, data.frame("REGION_NAME"="Taiwan", "GCAM_ID"=30))

#Modify DF
dat2@data<-merge(x = dat2@data, y = statesdf, by.x='STUSPS', by.y='state_abbrev', all.x = T, all.y=T)
d<-dat2@data



mp4<-basemap_regional(gc1, extent=EXTENT_USA, save=1, title="USA Albers Equal-Area", fn="USA_AEA")
mp4

#Map 2B China 

mp5<-basemap_regional(gc1, extent=EXTENT_CHINA, save=1, title="China Albers Equal-Area", fn="China_AEA")
mp5


#MAPSET 3: Latin America, Africa Superregions (Ortho)
mp6<-basemap_regional(gc1, extent=EXTENT_AFRICA, lims=LIMS_AFRICA, orientation=ORIENTATION_AFRICA, prj="orthographic", save=1, title= "Africa Orthographic", fn="Africa_ortho")
mp6

mp7<-basemap_regional(gc1, extent=EXTENT_LA, orientation= ORIENTATION_LA, prj="orthographic", save=1, title="Latin America Orthographic", fn="LA_ortho2")
mp7
  


#MAPSET 4: Polar (??)

#Color map

nms<-read.csv(file = "input-data/rgn32/gcam_32_wo_TaiwanLookupTable_clean.csv")
names(nms)[3]<-"colors"

cols<-nms$colors
cols<-as.character(cols)
cols[32]<-""
cols<-replace(x = cols, cols=="", "white")

ids<-nms$GCAM_ID
ids[32]<-0

cols<-setNames(cols,nm = ids)

#Generate graticule
grat<-gen_grat(bbox=EXTENT_WORLD)

#Qualitative map
#Plot graticule
mp<-ggplot()+
  geom_path(data=grat,aes(long,lat,group=group,fill=NULL),color=LINE_GRAT)

#Plot polygons and aesthetics
mp<-mp+
  geom_polygon(data=gc1, aes(long,lat,group=group, fill=factor(id)), color=LINE_COLOR)+
  scale_fill_manual(values=cols)+
  #theme(legend.position=LEGEND_POSITION)+
  #theme(panel.border=PANEL_BORDER, panel.background=PANEL_BACKGROUND,
   #     panel.grid=PANEL_GRID,
    #    axis.ticks=AXIS_TICKS, axis.text=AXIS_TEXT)+
  labs(x=NULL,y=NULL)

#Reproject map
mp<-mp+
  coord_proj(proj=robin)
mp




pal<-colorRampPalette(brewer.pal(12,'Set3'))
