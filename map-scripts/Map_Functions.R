##MAP FUNCTIONS 
#---------------------------------------------------------------------------
# DATA PROCESSING FUNCTIONS
#---------------------------------------------------------------------------

process_batch_q<-function(batchq, query, scen, filters, func=sum){
  ### Extract a query from a list of queries and filter to desired output.
  ### Allows for simple transformations on data (sum, mean, stdev, etc)
  ### Inputs: 
  ###   batchq - the output from parse_mi_output; a list of tables of queries.
  ###   query - the name of the table you would like to select from batchq
  ###   scen - the name of the scenario; can be partial
  ###   filters - a named vector of filtering criteria, in the form: 
  ###           "c(header1 = value1, header2 = value2, ...), where headers
  ###           are the names of columns in the data frame. If aggregating data,
  ###           use the value "Aggregate". 
  ###   func - optional; specify the operation you would like to perform on the
  ###           aggregated data (e.g. mean, sum, etc.)
  ### Outputs: 
  ###   qdata - a data frame of filtered query data. 
  
  qdata<-as.data.frame(batchq[[query]])
  
  #Filter for scenario; allow partial lookup
  #Bug: if partial has multiple matches, will return multiple scenarios
  qdata<-qdata[grepl(scen, qdata$scenario),]
  
  #Get years and aggregate value if applicable
  years<-grep("(X1)|(X2)", names(qdata), value=T)
  ag<-names(filters[filters[names(filters)]=="Aggregate"]) #Super clunky
  
  nms<-!(names(qdata) %in% years| names(qdata) %in% ag) 
  
  #Filter to query of interest using filters 
  for (name in names(filters)){
    if (filters[[name]]=="Aggregate"){
      qdata<-aggregate(qdata[years], by=qdata[nms], FUN=func)
      qdata[[ag]]<-"All"
    }
    else{
      qdata<-qdata[qdata[[name]]==filters[[name]],]
    }
  }
  
  return(qdata)
  
}

#TODO - modify to search for appropriate lookup, province, drop files in directory. 
addRegionID<-function(datatable, lookupfile, provincefile='none', drops='none') {
  ### Match GCAM ID to region using lookup file. Last data processing step 
  ###   before joining GCAM scenario data to geoJSON map.
  ###   Could just join by region, but prefer to avoid possible spelling 
  ###   issues. 
  ### Inputs: 
  ###   datatable - a query data frame processed with process_batch_q
  ###   lookupfile - string; path to the lookup file for the geoJSON map
  ###   provincefile - string; path to the province translation file, if applicable
  ###   drops - string; path to the drop regions file, if applicable
  ### Outputs: 
  ###   finaltable - a data frame with GCAM ID attached to each region. 
  
  if (provincefile != 'none'){
    datatable<-translateProvince(datatable, provincefile)
  }
  
  if (drops != 'none'){
    datatable<-dropRegions(datatable, drops)
  }
  
  lookuptable<-read.csv(lookupfile, strip.white=T, stringsAsFactors = F)
  
  #Differentiate region-Region issue
  if ("Region" %in% names(datatable)){
    rgn<-"Region"
  } else{
    rgn<-"region"
  }
  
  finaltable<-merge(datatable, lookuptable, by.x=rgn, by.y=colnames(lookuptable)[1], all.y=TRUE )
  
  ## Regions that weren't in the original table will show as NA.  Zero
  ## them out and give them a sensible unit.
  finaltable$Units[is.na(finaltable$Units)] <- finaltable$Units[1] # units will usually be all the same 
  finaltable[is.na(finaltable)] <- 0                               # set all remaining NA values to zero.
  colnames(finaltable)[ncol(finaltable)]<-'id'
  finaltable$id<-as.character(finaltable$id)
  
  
  #Add null vector row to end to account for GCAM region 0 
  nullvec<-c('0', 2:ncol(finaltable))
  nullvec[2:ncol(finaltable)]<-NA       # This one *should* be NA.
  
  finaltable<-rbind(finaltable, nullvec)
  finaltable$id[nrow(finaltable)]<-'0'
  
  return(finaltable)
}

translateProvince<-function(datatable, provincefile){
  ### Replace province abbreviations with full province names
  ### to ensure matching with GCAM map names. 
  ### Inputs: 
  ###   datatable - data frame of query from batch query CSV. 
  ###   provincefile - string; path to file with abbreviations and full names of regions. 
  ### Outputs: 
  ###   datatable - datatable modified so that abbreviations are now full names. 

  provincetable<-read.csv(provincefile, strip.white=T)
  
  #Differentiate region-Region issue
  if ("Region" %in% names(datatable)){
    rgn<-"Region"
  } else{
    rgn<-"region"
  }
  
  datatable$rgn<-as.character(datatable$rgn)
  provincetable$province<-as.character(provincetable$province)
  provincetable$province.name<-as.character(provincetable$province.name)
  
  datatable$rgn<-ifelse(is.na(provincetable$province.name[match(datatable$rgn, provincetable$province)]), 
                           datatable$rgn, 
                           provincetable$province.name[match(datatable$rgn, provincetable$province)])
  
  return(datatable)
} 

dropRegions<-function(datatable, drops){
  ### Drop regions listed in drops file from data frame. 
  ### Inputs: 
  ###   datatable - a data frame of query from batch query CSV
  ###   drops - string; path to file containing regions to be dropped
  ### Outputs:
  ###   datatable - updated data frame with regions dropped. 
  
  dr<-read.csv(drops, strip.white=T, header=F)
  dr<-as.character(dr$V1)

  regcols<-grepl("egion", names(datatable)) #Find instances of "region" or "Region" columns
  
  datatable[regcols]<-lapply(datatable[regcols], function(x) replace(x, x %in% dr, NA)) #Replace drop col values with NA
  
  datatable<-na.omit(datatable) #Remove rows containing NA
  
  return(datatable)
}

#---------------------------------------------------------------------------
# MAPPING UTILS
#---------------------------------------------------------------------------
get_bbox_polys<-function(dataset, bbox){
  ### Modifies map data to include only polygons that lie partially within 
  ### bounding box. 
  ### Inputs: 
  ###   dataset - data frame of map geometry
  ###   bbox - numeric vector (long_min, long_max, lat_min, lat_max)
  ### Outputs: 
  ###   newdata - data with only polygons that are at least partially in 
  ###           bounding box

    filter(dataset, in_range(long, bbox[1], bbox[2]), in_range(lat, bbox[3], bbox[4]))
}

## in_range - Test a vector to see which values are in the interval [a,b]
## params: x:  vector to test
##       a,b:  interval bounds. can be int or numeric; a<=b
in_range<-function(x, a, b){
    x>=a & x<=b
}

gen_grat<-function(bbox=EXTENT_WORLD,longint=20,latint=30){
  ### Generate graticule (long/lat lines) given bbox
  ### Inputs: 
  ###   bbox - numeric vector (long_min, long_max, lat_min, lat_max)
  ###   longint - interval between longitude lines
  ###   latint - interval between latitude lines
  ### Outputs: 
  ###   grat - dataframe describing of latitude and longitude lines 
  ###         spaced at specified intervals

### TODO: We could probably realize some savings here by
### precalculating and caching graticules for some commonly-used
### configurations.
    
  require(graticule)
  
  #Generate graticule as sp matrix object 
  lons=seq(bbox[1],bbox[2],by=longint)
  lats=seq(bbox[3],bbox[4],by=latint)
  
  grat<-graticule(lons,lats,xlim=range(lons),ylim=range(lats))
  
  #Convert to ggplot2-friendly format
  grat<-fortify(grat)
  
  return(grat)
  
}

calc.breaks <- function(maxval, minval=0, nbreak=5, nsig=3)
{
    step <- (maxval-minval)/(nbreak-1)
    seq(0,maxval, by=step) %>% signif(nsig) 
}
calc.breaks.map<-function(mapdata, colname, nbreaks=4, zero.min=TRUE){
  ### Calculate legend breaks (intervals to put labels on legend scale)
  ### Inputs: 
  ###   mapdata - data frame of geometry and scenario data
  ###   colname - column name of interest from which to calculate legend intervals
  ###   nbreaks - number of intervals
  ###   zero.min- force minimum value of scale to zero
  ### Outputs: 
  ###   breaks - vector of values at which to include legend label
  

    vals <- as.numeric(mapdata[[colname]])

    max_dat <- max(vals, na.rm = T)
    if(zero.min)
        min_dat = 0
    else
        min_dat <- min(vals, na.rm= T)
  
    calc.breaks(max_dat, min_dat, nbreaks) 
}

#-----------------------------------------------------------------
# COLOR PALETTE FUNCTIONS
#-----------------------------------------------------------------

qualPalette<- function(n = 31, pal = 'Set3', na.val = 'grey50'){
  colors<-colorRampPalette(brewer.pal(8, pal))(n)
  colors<-setNames(colors, as.character(1:n))
  colors["0"]<-na.val
  
  return(colors)
}


#-----------------------------------------------------------------
# MAPPING FUNCTIONS
#-----------------------------------------------------------------
# coord_GCAM: This function unifies the ggplot2 and ggalt coordinate systems to make use 
#   of both of them, depending on the projection needed. Can be used with any ggplot2 map object.
#   If projection is orthographic, will use ggplot2 coord_map functionality. If projection is not 
#   orthographic, will use ggalt coord_proj functionality. 
#
# Arguments
#   proj - the projection. proj4 string or pre-defined variable in diag_header.R
#   orientation - Use if using orthographic projection
#   extent - Vector of lat/lon limits c(lat0,lat1,lon0,lon1)
#   parameters - additional parameters corresponding to coord_map ggplot2 function
#   inverse ?
#   degrees - Units for lat/longitude ?
#   ellps.default - default ellipse to use with projection ?
#
# Usage: as add-on function to a ggplot2 object. Example: 
#  ggplot()+
#   geom_polygon(data, aes(x,y,group))+
#   coord_GCAM(proj)

coord_GCAM <- function(proj = NULL, orientation = NULL, extent = NULL, ..., parameters = NULL, inverse=FALSE,
                       degrees=TRUE, ellps.default="sphere"){
  
  if (is.null(proj)){
    # Default proj4 pstring for default GCAM projection (Robinson)
    proj <- paste0(c("+proj=robin +lon_0=0 +x_0=0 +y_0=0", 
                     "+ellps=WGS84 +datum=WGS84 +units=m +nodefs"), 
                   collapse = " ")
  }
  
  if (is.null(parameters)){
    params <- list(...)
  } else {
    params <- parameters
  }
  
  # Default extent is EXTENT_WORLD (-180,180,-90,90)
  if (is.null(extent)){
    xlim <- c(-180,180)
    ylim <- c(-90,90)
  } else{
    xlim <- c(extent[1], extent[2])
    ylim <- c(extent[3], extent[4])
  }
  
  # Use ggproto object defined in ggplot2 package if using orthographic map projection 
  if(grepl("ortho", proj)){
    ggproto(NULL, CoordMap,
            projection = proj,
            orientation = orientation,
            limits = list(x = xlim, y = ylim),
            params = params
    )
  } else{
    # Otherwise use ggproto object defined in ggalt package for default GCAM projections
    ggproto(NULL, CoordProj,
            proj = proj,
            inverse = inverse,
            ellps.default = ellps.default,
            degrees=degrees,
            limits = list(x = xlim, y = ylim),
            params = list()
    )
  }
  
}


# theme_GCAM: Default GCAM theme function. Can be used with any ggplot2 object. 
#   Derives from ggplot2 black and white theme function (theme_bw)
#
# Arguments: 
#   base_size: Base font size
#   base_family: Base font type
#   legend: T or F; whether to include a legend with default legend formatting. 
#
# Usage: As add-on function to any ggplot2 object. 
theme_GCAM <- function(base_size = 11, base_family="", legend=F){
  
  if (legend==F){
    theme_bw(base_size = base_size, base_family= base_family) %+replace%
      theme(
        panel.border = element_rect(color = LINE_COLOR, fill = NA),
        panel.background = PANEL_BACKGROUND,
        panel.grid = PANEL_GRID,
        axis.ticks = AXIS_TICKS,
        axis.text = AXIS_TEXT,
        legend.position='none'
      ) 
  }
  
  else if (legend==T){
    theme_bw(base_size = base_size, base_family= base_family) %+replace%
      theme(
        panel.border = element_rect(color = LINE_COLOR, fill = NA),
        panel.background = PANEL_BACKGROUND,
        panel.grid = PANEL_GRID,
        axis.ticks = AXIS_TICKS,
        axis.text = AXIS_TEXT,
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size=14, face="bold"),
        legend.position = LEGEND_POSITION, 
        legend.key=element_rect(color='black')
      )
  }
  
}




#-----------------------------------------------------------------
# MAPS
#-----------------------------------------------------------------
# plot_GCAM: Primary GCAM mapping function. Can handle categorical or continuous data. 
#   
# Arguments: 
#   mapdata - The data frame containing both geometric data (lat, long, id) and regional metadata.
#     This is the only mandatory variable. If used alone, will produce the default map. 
#   col - If plotting categorical/contiuous data, the name of the column to plot
#     !Will automatically determine type of style of plot based on type of data (numeric or character)!
#     !Don't plot numeric data stored as character or this will produce an error!
#   proj, extent, orientation: arguments passed to coord_GCAM function
#   title - Title of map
#   Legend - T/F value - use legend or not. Argument to theme_GCAM function
#   Colors - a vector of colors to use; otherwise will use defaults if necessary. 
#   qtitle - The title of the legend
#   limits - The range of the legend color bar; vector with 2 values: c(min, max)
#   colorfcn - If plotting categorical data, the function used to generate a colorscheme when colors
#         are not provided (usually qualPalette)

plot_GCAM <- function(mapdata, col = NULL, proj=robin, extent=EXTENT_WORLD, orientation = NULL, 
                      title = NULL, legend = F, colors = NULL, qtitle=NULL, limits=NULL, 
                      colorfcn=NULL, ...){
  
  # Generate graticule (latitude/longitude lines) and clip map to extent specified.
  grat<-gen_grat()
  mappolys <- get_bbox_polys(dataset = mapdata)
  
  
  #Plot graticule and polygons
  mp<-ggplot()+
    geom_path(data=grat,aes(long,lat,group=group,fill=NULL),color=LINE_GRAT)+
    geom_polygon(data=mappolys, aes_string("long","lat",group="group",fill=col), color=LINE_COLOR)
  
  # If a column name is specified, add a color gradient or categorical colors
  if (!is.null(col)){
    
    if(typeof(mappolys[[col]])=='double'|typeof(mappolys[[col]])=='integer'){
      # Instructions for color gradient
      # Calculate legend label increments ('breaks')
      if(is.null(limits)) 
        breaks <- calc.breaks.map(mappolys, col)
      else
        breaks <- calc.breaks(limits[2])
      
      # Use default colors if none specified
      if(is.null(colors))
        colors <- DEFAULT_CHOROPLETH
      
      # Add color scale to map
      mp <- mp+
        scale_fill_gradientn(name=qtitle, colors=colors, values=NULL, guide=GUIDE, space=SPACE,
                             na.value=NA_VAL, breaks=breaks,limits=limits, 
                             labels=breaks)
      
    } else if(typeof(mappolys[[col]])=='character'){
      # Instructions for categorical map
      # Use default color scheme and color function if none specified
      if (is.null(colors)){
        if(is.null(colorfcn)){
          colorfcn <- qualPalette
        }
        colors<-colorfcn(n = length(unique(mappolys[[col]])), ...)
      }
      
      # Add color scale to map
      mp <- mp+
        scale_fill_manual(values=colors,name=qtitle)
    }
  } else{
    # If no data is being plotted, use default color scale
    mp <- mp + 
      geom_polygon(data=mappolys, aes_string("long","lat",group="group",fill=col),fill=RGN_FILL, color=LINE_COLOR)
  }
  
  # Project map and add theme and labels
  mp <- mp + 
    coord_GCAM(proj=proj,orientation=orientation,extent=extent)+
    theme_GCAM(legend=legend)+
    labs(title=title, x=XLAB, y=YLAB)
  
  return(mp)
}




#-----------------------------------------------------------------
# MISC UTILS
#-----------------------------------------------------------------
# save_image - Wrapper for ggsave function with desired parameters as defaults. 
# Arguments - mp - map object that you want to save
#             fn - file name you want to save it as

save_image<-function(mp, fn){
  ggsave(filename = paste("output-files/",fn, EXTENSION, sep=""), plot = mp, dpi=DPI, width=WIDTH, height=HEIGHT)
}
    


