###Generate Maps for GCAM Map Document
###TODO: 
    #Add paths for saving files


basedir.viz <- dirname(sys.frame(1)$ofile)

library(rgdal)
library(ggplot2)
library(ggalt)  # must install the github version:  install_github('hrbrmstr/ggalt')
library(graticule)
library(rgeos)
library(maptools)
library(gpclib)
library(sp)
library(mapproj)

#Core map functions
source(file.path(basedir.viz, "Map_Params.R"))
source(file.path(basedir.viz, "diag_header.R"))
source(file.path(basedir.viz, "Map_Parser.R"))
source(file.path(basedir.viz, "Map_Functions.R"))

gpclibPermit()

#-----------------------------------------------------------------
#To geojson: writeOGR(d, layer="",dsn="China_map.geojson",driver="GeoJSON")

###Driver for Data-Processing Functions
#-----------------------------------------------------------------
#Prepare Data
#Scenario Data
tables<-parse_mi_output(fn = file.path(basedir.viz, "../input-data/sample-batch.csv"))

#Map Data
map_32_wo_Taiwan<-readOGR(file.path(basedir.viz, "../input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson"), "OGRGeoJSON")
map_32_wo_Taiwan.fort<-fortify(map_32_wo_Taiwan, region="GCAM_ID")

#Break out sample scenario
prim_en<-process_batch_q(tables, "primary_energy", "Reference", c(fuel="a oil"))
prim_en<-addRegionID(prim_en, file.path(basedir.viz, "../input-data/rgn32/lookup.txt"), drops=file.path(basedir.viz, "../input-data/rgn32/drop-regions.txt"))

#Merge dataset with map data
map_primen<-merge(map_32_wo_Taiwan.fort, prim_en, by="id")

#-----------------------------------------------------------------

#MAPSET 1: Basemaps - World Maps, various projections, various types
#Map 1A: Eckert III World, Colored by Region
mp1<-map_category(map_32_wo_Taiwan.fort, prj = eck3, colorfcn=qualPalette)
mp1

#Map 1B: Robinson World, Colored by Oil Consumption
mp2<-map_query(map_primen, "X2050", c("white", "red"), prj=robin, title="Robinson World", qtitle="Oil Consumption, 2050")
mp2


#Map 1C: Winkel-Tripel
mp3<-basemap(map_32_wo_Taiwan.fort, prj=wintri, title="Winkel-Tripel World")
mp3


#Map 1D USA Albers Equal Area

mp4<-basemap(map_32_wo_Taiwan.fort, extent=EXTENT_USA, title="USA Albers Equal-Area")
mp4

#Map 1E China 

mp5<-basemap(map_32_wo_Taiwan.fort, extent=EXTENT_CHINA, title="China Albers Equal-Area")
mp5


# Map 1F Africa
mp6<-basemap(map_32_wo_Taiwan.fort, extent=EXTENT_AFRICA, orientation=ORIENTATION_AFRICA, prj="orthographic",title= "Africa Orthographic")
mp6

# Map 1G Latin America
mp7<-basemap(map_32_wo_Taiwan.fort, extent=EXTENT_LA, orientation= ORIENTATION_LA, prj="orthographic", title="Latin America Orthographic")
mp7
  
