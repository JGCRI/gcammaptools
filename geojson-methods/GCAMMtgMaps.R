###Generate Maps for GCAM Map Document
###TODO: Update Map_Functions with graticule - fix gen_grat function to be consistent
    #Organize existing code here into functions
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

#Prepare Data
#Scenario Data
tables<-parse_mi_output(fn = "input-data/sample_batch.csv")

#Map Data
dat1<-readOGR("input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson", "OGRGeoJSON")
gc1<-fortify(dat1, region="GCAM_ID")

### TODO create function
#Sample scenario - Population
pop<-as.data.frame(tables$population)
pop<-pop[grepl("Reference", pop$scenario),]
pop<-dropRegions(pop, "input-data/rgn32/drop_regions.txt") #Remove data for dropped regions
pop<-addRegionID(pop, "input-data/rgn32/gcam_32_wo_TaiwanLookupTable_clean.csv")

#Prepare data for choropleth
map_pop<-merge(gc1, pop, by="id")


#Dataset2: Primary Energy
prim_en<-as.data.frame(tables$primary_energy)

#Filter by scenario and query
prim_en.oil<-prim_en[grepl("Reference", prim_en$scenario),] 
prim_en.oil<-prim_en.oil[(prim_en.oil$fuel=="Oil"),]

#Have to add rgn id last. 
prim_en.oil<-addRegionID(prim_en.oil, "input-data/rgn32/gcam_32_wo_TaiwanLookupTable_clean.csv", drops = "input-data/rgn32/drop_regions.txt")

map_primen<-merge(gc1, prim_en.oil, by="id")




#MAPSET 1: Basemaps - World Maps, various projections, basemap
#Map 1A: Eckert III World
mp1<-basemap_world(gc1, save = 1, prj = eck3, title="Eckert III World", fn="Eck3")
mp1


#Map 1B: Robinson World
mp2<-basemap_world(gc1, save=1, prj=robin, title="Robinson World", fn="Robin")
mp2

#Robin Qualitative
p<-colorRampPalette(brewer.pal(8,"Set2"))
q<-p(31)

mp2<-qualMap(gc1, "world", prj=robin, q, title= "Robinson World", fn="Robin_col")
mp2

#Map 1C: Winkel-Tripel
mp3<-basemap_world(gc1, save=1, prj=wintri, title="Winkel-Tripel World", fn="WinTri")
mp3


#MAPSET 2: USA, India, China (Conic)
#Map 2A USA Albers Equal Area
# TODO - Graticule issues

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
