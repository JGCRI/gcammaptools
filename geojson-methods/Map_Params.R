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
na_aea<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"
ch_aea<-"+proj=aea +lat_1=27 +lat_2=45 +x_0=0 +y_0=0 +lat_0=35 +lon_0=105 +ellps=WGS84 +datum=WGS84"

#Special cases
ortho_africa<-"orthographic" 
ortho_la<-"orthographic"
ortho_polar<-"orthographic"

#For orthographic projections
ORIENTATION_AFRICA<-c(0,15,0)
ORIENTATION_LA<-c(-10,-70,0)
ORIENTATION_SPOLE<-c(-90,0,0)

#Coord_map default projections
coord_map_projs<-c("mercator", "sinusoidal", "cylequalarea",
                   "cylindrical", "rectangular", "gall", 
                   "mollweide", "gilbert", "azequidistant",
                   "azequalarea", "gnomonic", "perspective",
                   ortho="orthographic", "stereographic", "laue",
                   "fisheye", "newyorker", "conic", "simpleconic",
                   "lambert", aea="albers", "bonne", "polyconic", 
                   "aitoff", "lagrange", "bicentric", "elliptic",
                   "globular", "vandergrinten", "eisenlohr", "guyou",
                   "square", "tetra", "hex", "harrison", "trapezoidal",
                   "lune", "mecca", "homing", "sp\\_mercator", "sp\\_albers")

#---------------------------------------------
# EXTENT
# (lon0,lon1,lat0,lat1)

EXTENT_WORLD <- c(-180,180,-90,90)
EXTENT_USA <- c(-120,-70,20,60)
EXTENT_CHINA <- c(77,130,15,53)
EXTENT_AFRICA<-c(-20,60,-40,40)
EXTENT_LA<-c(-120,-30,-60,40)


#---------------------------------------------
# AESTHETICS 

#Default Colors
LINE_COLOR<-"black"
RGN_FILL<-"grey"
LINE_GRAT<-"grey50"
GUIDE="colourbar"
SPACE="Lab"

#TODO - ALTER to inc. colorschemes
#Choropleth Colors - 
LOW_COLOR = 'light green'
HIGH_COLOR = 'dark green'
TRANSFORMATION = 'none' #Mathematical transformation (i.e. 'log', 'sqrt')
NA_VAL<-"grey50"

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


#------------------------------------------
# Color Palettes
# World Palettes
  # 1. GCAM 32
#pal_gcam32<-

gcam14_colors<- c("Africa" = "navajowhite3",
                   "Australia_NZ" = "lightpink",
                   "India" = "lightslateblue",
                   "USA" = "sandybrown",
                   "Japan" = "rosybrown1",
                   "Korea" = "brown",
                   "Eastern Europe" = "orange" ,
                   "Western Europe" = "greenyellow",
                   "Canada" = "saddlebrown",
                   "China" = "lightblue",
                   "Southeast Asia" = "gold",
                   "Latin America" = "seagreen2",
                   "Middle East" = "indianred",
                   "Former Soviet Union" = "plum2")


gcam32_colors<-c("Africa_Eastern" = "navajowhite3",
                 "Australia_NZ" = "lightpink",
                 "India" = "lightslateblue",
                 "USA" = "sandybrown",
                 "Japan" = "rosybrown1",
                 "South Korea" = "brown",
                 "Europe_Eastern" = "orange" ,
                 "EU-12" = "greenyellow",
                 "Canada" = "saddlebrown",
                 "China" = "lightblue",
                 "Southeast Asia" = "gold",
                 "South America_Northern" = "seagreen2",
                 "Middle East" = "indianred",
                 "Russia" = "plum2")

