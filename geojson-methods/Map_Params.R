#Parameters for mapping functions

require(rgdal)

#-----------------------------------------------
# Graphics Specifications (for exporting finished maps)
WIDTH = 2560/300
HEIGHT= 1440/300
DPI = 300/2
#TYPE= 'cairo-png'
EXTENSION= '.png'

#----------------------------------------------
#Default Projections (as PROJ4 strings)

eck3<-"+proj=eck3"  #Eckert III World projection
wintri<-"+proj=wintri" #Winkel-Tripel World projection
robin<-"+proj=robin"  #Robinson world projection

#AEA requires long/lat and datum selection. Placeholder
aea<-"+proj=aea"
aea.lat_0<-30
aea.lat_1<-40

#---------------------------------------------
# AESTHETICS 

#Default Colors
LINE_COLOR<-"black"
RGN_FILL<-"grey"
LINE_GRAT<-"grey50"

#TODO - ALTER to inc. colorschemes
#Choropleth Colors - 
LOW_COLOR = 'light green'
HIGH_COLOR = 'dark green'
TRANSFORMATION = 'none' #Mathematical transformation (i.e. 'log', 'sqrt')

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

#---------------------------------------------
# EXTENT

EXTENT_WORLD <- c(-180,180,-90,90)
EXTENT_USA <- c(-120,-70,20,60)
EXTENT_CHINA <- c(77,135,20,53)
