###Generate Maps for GCAM Map Document
###TODO: 
    #Add paths for saving files

map_demos.R <- function(){} 
basedir.viz <- getSrcDirectory(map_demos.R) 

#Core map functions
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
# Sample Maps generated with plot_GCAM and GCAM32 shapefile

#Example 1: Eckert III World Projection, Colored by Region
mp1<-plot_GCAM(map_32_wo_Taiwan.fort, col = 'id', proj = eck3, colorfcn=qualPalette)
mp1

#Example 2: Robinson World Projection, Colored by Oil Consumption by Region
mp2<-plot_GCAM(map_primen, col = "X2050", colors = c("white", "red"), 
               title="Robinson World", qtitle="Oil Consumption, 2050", legend=T)
mp2

#Example 3: Winkel-Tripel Projection, Default Color and Style
mp3<-plot_GCAM(map_32_wo_Taiwan.fort, proj=wintri, title="Winkel-Tripel World")
mp3

#Example 4: U.S. Projection (Albers Equal-Area)
mp4<-plot_GCAM(map_32_wo_Taiwan.fort, proj=na_aea,  extent=EXTENT_USA, title="USA Albers Equal-Area")
mp4

#Example 5: China Projection (Albers Equal-Area)
mp5<-plot_GCAM(map_32_wo_Taiwan.fort, proj=ch_aea,extent=EXTENT_CHINA, title="China Albers Equal-Area")
mp5

#Example 6: Africa Projection (Orthographic)
mp6<-plot_GCAM(map_32_wo_Taiwan.fort, extent=EXTENT_AFRICA, 
               orientation=ORIENTATION_AFRICA, proj="orthographic",title= "Africa Orthographic")
mp6

#Example 7: Latin America Projection (Orthographic)
mp7<-plot_GCAM(map_32_wo_Taiwan.fort, extent=EXTENT_LA, 
               orientation= ORIENTATION_LA, proj="orthographic", title="Latin America Orthographic")
mp7


  
