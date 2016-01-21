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

#Helper functions
for (fn in list.files("scripts", full.names = T)){
  source(fn)
}

#-----------------------------------------------------------------
#To geojson: writeOGR(d, layer="",dsn="China_map.geojson",driver="GeoJSON")

#Parse batch output
tables<-parse_mi_output(fn = "input-data/sample_batch.csv")

#MAPSET 1: Basemaps - World Maps, various projections, basemap
dat1<-readOGR("input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson", "OGRGeoJSON")
gc1<-fortify(dat1, region="GCAM_ID")

#Map 1A: Eckert III World
mp1<-basemap_world(gc1, save = 1, prj = eck3, title="Eckert III World", fn="Eck3")
mp1


#Map 1B: Robinson World
mp2<-basemap_world(gc1, save=1, prj=robin, title="Robinson World", fn="Robin")
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
#MAPSET 4: Polar (??)


