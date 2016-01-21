#!usr/bin/env python 
"""A class of functions for manipulating GeoJSON maps for use in GCAM
visualization tasks. Creates a Map instance and alters the map's geoJSON
structure, stored as a python dictonary object. Functionality includes
importing data as map attributes, modifying map attributes and exporting
maps for use in subsequent visualization tasks."""

import string
import json
import csv
from collections import defaultdict
import sys

class Map:

    def __init__(self, name, mapfile, idName = "GCAM_ID", regionName ="REGION_NAME"):
        """Initialize a geoJSON dictionary object of type 'Map' from geoJSON data.
            Inputs:
                name - String giving map name. For use in saving files.
                mapfile - file handle for geoJSON file containing map geometry and
                attributes. 
                rgnconfig - path to directory where map's regional attributes are located.
                idName - string denoting map's default region ID category.
                regionName - string denoting map's default region name category
        """
        #TODO file opening/closing, rgn config
        
        #Basic map features:
        self.name = name
        self.idName = idName
        self.regionName = regionName
    
        #Load map as dictionary
        self.mapdict = json.load(mapfile)

        #Create GeoJSON Schema
        #TODO: nlevels max level
        #TODO: move to function; recursive? Automatically create nlevels?
        #Do here or in init?
        #Avoid L1,L2,L3 and have one nested dict?
        self.level0_names = self.mapdict.keys()
        self.level1_names = {}
        for nm in self.level0_names:
            try:         
                self.level1_names[nm]=self.mapdict[nm].keys()
            except AttributeError:
                if type(self.mapdict[nm])==unicode:
                    pass
                elif type(self.mapdict[nm])==list:
                    self.level1_names[nm] = self.mapdict[nm][0].keys()
        self.level2_names = {}
        for l1 in self.level1_names:
            for l2 in self.level1_names[l1]:
                try:
                    self.level2_names[l2]= self.mapdict[l1][l2].keys()
                except TypeError:
                    if type(self.mapdict[l1])==list:
                        try: 
                            self.level2_names[l2]= self.mapdict[l1][0][l2].keys()
                        except AttributeError:
                            pass

                        
               #Region configuration file path - path to outputs
        ###To do - load up lookup and ordering info
        #sys.path.append(rgnconfig)

        mapfile.close()


    def listAttrs(self, level):
        """Queries the map's geoJSON dictionary and returns a list of attributes.
        Assumes geoJSON path of ["features"][i]["properties"]. 

        Return value: attrs - list of attributes
        """
        #Create GeoJSON Schema
        #TODO: nlevels max level
        #TODO: move to function; recursive? Automatically create nlevels?
        #Do here or in init?
        #Avoid L1,L2,L3 and have one nested dict?
        
        if level==0:
           self.level0_names = self.mapdict.keys()
           return self.level0_names

        elif level==1:
            self.level1_names = {}
            for nm in self.level0_names:
                try:         
                    self.level1_names[nm]=self.mapdict[nm].keys()
                except AttributeError:
                    if type(self.mapdict[nm])==unicode:
                        pass
                    elif type(self.mapdict[nm])==list:
                        self.level1_names[nm] = self.mapdict[nm][0].keys()
            return self.level1_names

        elif level==2:
            self.level2_names = {}
            for l1 in self.level1_names:
                for l2 in self.level1_names[l1]:
                    try:
                        self.level2_names[l2]= self.mapdict[l1][l2].keys()
                    except TypeError:
                        if type(self.mapdict[l1])==list:
                            try: 
                                self.level2_names[l2]= self.mapdict[l1][0][l2].keys()
                            except AttributeError:
                                pass
            return(self.level2_names)

        return(0)

    
    def selectAttr(self, attrname=None,level=2):
        """Returns an attribute key either through querying attribute list and
            prompting user input or by direct assignment.
            
            attrname - name of the attribute key to select. Should be a string.
            
            Return value - attr - the name of the selected attribute key, a string. If attrname is incorrect,
                value of attr will be None. 
            
        """
        #IN PROGRESS
        #Get list of available attributes (default - properties)
        attrlist = self.listAttrs(level)
        attr = None

        #Prompt for user input
        if attrname==None:
            while attr==None:
                try:
                    attr = input("Select an attribute:\n%s\n>:" % (attrlist))
                except NameError:
                    print "Please select a category from the list above. Remember to use ''!\n"
                    
        #For no user input, verify that attrname is valid selection and assign to self.attr
        else:
            if (attrname in attrlist):
                attr= attrname
                return attr
            else:
                print "Invalid attribute selection."
        return attr


    def queryAttr(self, attrname, l1name= 'properties', attrid=None):
        """Gets list of values associated with key attrname for each feature in map.
            Inputs:
                attrname - key you want to query; string
                attrid - id key to attach to attribute data; string
            Outputs:
                vals - list of values associated with key, ordered by region in map,
                or a tuple of value, id pairs. 
        """

        path = self.mapdict["features"]

        if attrid==None:
            vals = []
            for feature in path:
                vals.append(feature[l1name][attrname])

            return(vals)
        else:
            vals = []
            val_id = []
            for feature in path:
                vals.append(feature[l1name][attrname])
                val_id.append(feature[l1name][attrid])
            nvals = zip(val_id, vals)

            return(nvals)
            

    def getLookupTable(self, export=0):
        """For maps with defined "GCAM_ID" and "REGION_NAME" fields only.
            Returns tuple of region name, GCAM ID.
            Arguments
                export - boolean. If export =1, will write lookup table to CSV.
            Return value:
                lookup - list of tuples of (region name, gcam id)
        """

        #Get tuple of id, region name from map
        gcamid = self.queryAttr(self.idName, self.regionName)

        #Remove duplicates and sort by id
        lookup = []
        lookup.append(gcamid[0])
        for i in range(1, len(gcamid)):
            if gcamid[i] not in lookup:
                lookup.append(gcamid[i])
                
        lookup.sort(key= lambda tup: tup[1])
                
        if export==1:
            filename = self.name + 'LookupTable' + '.csv'
            with open(filename, 'wb') as outfile:
                csv_out = csv.writer(outfile)
                csv_out.writerow([self.regionName, self.idName])
                for row in lookup:
                    csv_out.writerow(row)

        return(lookup)

    def matchData(self, data):
        ndata = []
        for tup in data:
            i = 0
            for feature in self.mapdict["features"]:
                i +=1
                if feature["properties"][self.idName]==tup[0] or feature["properties"][self.regionName]==tup[0]:
                    tup = tup + (i,)
                    ndata.append(tup)
        return(ndata)



    def appendNewAttr(self, attrname, data=None, uniform=False, append=False, level=2, levelnames=['features','properties']):
        """Append new attribute(s) to the geoJSON dictionary along with associated data.
            Inputs:
                attrname - Name of the attribute you want to append to (key). Does not have to exist in map already.

                data - The values you want to include with your attribute key. If nonuniform, in form of tuple (id, data);
                    if uniform in form of string, int, bool, etc. 
                
                uniform - A boolean denoting whether data is constant across regions or varies by region.

                append - boolean denoting whether you are appending data to an existing attribute. If false, data attached
                    to existing attributes will be overwritten with new data.                
            Outputs:
                self.mapdict will be modified to incorporate the new attribute. 
        """
        dta = data

        if level==0:
            self.mapdict[attrname]=dta
            
        elif level==1:
            try:
                self.mapdict[levelnames[0]][attrname]==dta
            except TypeError:
                if type(self.mapdict[levelnames[0]])==list:
                    if uniform==True:
                        for feature in self.mapdict[levelnames[0]]:
                            feature[attrname]=dta
                    else:
                        ndta = self.matchData(dta)

                        for tup in ndta: 
                            if append==True:
                                try:
                                    self.mapdict[levelnames[0]][tup[-1]][attrname]= list(self.mapdict[levelnames[0]][tup[-1]][attrname])
                                except TypeError:
                                    if self.mapdict[levelnames[0]][tup[-1]][attrname]==None:
                                        self.mapdict[levelnames[0]][tup[-1]][attrname]=[]

                                for i in range(1,len(tup)-1):
                                    self.mapdict[levelnames[0]][tup[-1]][attrname].append(tup[i])

                            else: 
                                #Keep single data values as non-lists for now (?). 
                                if len(tup)==3: 
                                    self.mapdict[levelnames[0]][tup[-1]][attrname]==tup[1]

                                #Only multiple data values in list form
                                elif len(tup)>3:
                                    self.mapdict[levelnames[0]][tup[-1]][attrname]=[]
                                    for i in range(1,len(tup)-1):
                                        self.mapdict[levelnames[0]][tup[-1]][attrname].append(tup[i])
                            
        elif level==2:
            try:
                self.mapdict[levelnames[0]][levelnames[1]][attrname]==dta
            except TypeError:
                if type(self.mapdict[levelnames[0]])==list:
                    if uniform==True:
                        for feature in self.mapdict[levelnames[0]]:
                            feature[levelnames[1]][attrname]=dta
                    else:
                        j = 0
                        for feat in self.mapdict["features"]: 
                            if j>3:
                                break
                            
                            featname = feat["properties"][self.regionName]
                            print "Featname:",featname
                            ndata = dta #Make a copy of data
                            print len(ndata)
                            
                            #Remove all data not in region
                            i = 0
                            rgn = [x for x in ndata if x[0]==featname]
                            print len(rgn)
                        
                            
                            #Append data to feature
                            try:
                                feat[levelnames[1]][attrname]= list(feat[levelnames[1]][attrname])
                            except TypeError:
                                if feat[levelnames[1]][attrname]==None:
                                    feat[levelnames[1]][attrname]=[]
                            except KeyError:
                                feat[levelnames[1]][attrname] = []


                            feat[levelnames[1]][attrname].extend(rgn)

                            print len(feat[levelnames[1]][attrname])
                            j+=1                         
                            
             

    def renameAttr(self, oldname, newname):
        """Renames an existing attribute. Assumes attribute is appended to
            ["features"][i]["properties"].
            Inputs
                oldname - old name of the attribute; string
                newname - new name of the attribute; string
        """

        path = self.mapdict["features"]

        for feature in path:
            features["properties"][newname] = features["properties"].pop(newname)

        
    def exportMapAsJSON(self, filename = None):
        """Write map dictionary to geoJSON outfile.
            filename - string
        """
        if filename==None:
            filename = str(self.name) + '.geojson'
        with open(filename, 'w') as outfile:
            json.dump(self.mapdict, outfile)

        outfile.close()

    def getDataFromCSV(self, filename, skip=0):
        """Converts CSV data to list of (region/id, data) tuples. 
            filename - string
        """
        with open(filename, 'r') as infile:
            data = []
            reader = csv.reader(infile)
            for sk in range(skip):
                reader.next()

            for line in reader:
                #Format items in each line
                ln=[]
                for item in line:
                    item = item.strip()
                    try:
                        item=float(item)
                    except ValueError:
                        pass
                    ln.append(item)
                #Append to data list
                data.append(tuple(ln))

        infile.close()                                                                     
        return data

    def deleteAttr(self, level, attr):
        """Deletes an attribute and associated data from the geoJSON dictionary.
            attr - name of the attribute you want to delete (string)
        """
        #TODO - Streamline and test
        if level==0:
            del self.mapdict[attr]
        elif level==1:
            for l0 in self.level1_names:
                if attr in self.level1_names[l0]:
                    try:
                        del self.mapdict[l0][attr]
                    except AttributeError:
                        if type(self.mapdict[l0])==list:
                            for i in self.mapdict[l0]:
                                del i[attr]
        elif level==2:
            for l1 in self.level2_names:
                if attr in self.level2_names[l1]:
                    level1 = l1
            for l0 in self.level1_names:
                if level1 in self.level1_names[l0]:
                    level0 = l0

            try:
                del self.mapdict[level0][level1][attr]
            except TypeError:
                if type(self.mapdict[level0])==list:
                    for i in self.mapdict[level0]:
                        del i[level1][attr]
        else:
            return(0)
                

##        #Append non-uniform data to corresponding map region      
##        elif uniform==0:

##            #Match data to map. 
##            ndata = self.matchDataToRegion(data, rgnid=idname)
##
##
##            for feature in self.mapdict['features']:                   
##                #Get name of map region
##                idpath = feature['properties'][idname]
##
##                #Get data corresponding to map region
##                vals=None
##                for rgn in ndata:
##                    found =0
##                    if rgn==str(idpath):
##                        vals=ndata[rgn]
##                        found =1
##                    if found==1:    #Exit loop when correct region is found.
##                        break
##                    
##                #Build path to desired attribute(s); Haven't figured out how to dynamically create path unfortunately
##                #This is really ugly but doing a loop to create a path doesn't work.
##                #Ideas on how to condense this?
##                if pathlen ==1:
##                    if feature[path[0]] == {}:
##                        if type(vals)==list:
##                            feature[path[0]]= {attrname[0]:vals[0]}
##                        else:
##                            feature[path[0]]= {attrname[0]:vals}
##                    else:
##                        if type(vals)==list:
##                            feature[path[0]][attrname[0]]= vals[0]
##                        else:
##                            feature[path[0]][attrname[0]]= vals
##
##
##                elif pathlen==2:
##                    #Append data to each attribute
##                    #Get around bug by adding first data value to empty dictionary
##                    if feature[path[0]][path[1]] == {}:
##                        if type(vals)==list:
##                            feature[path[0]][path[1]]= {attrname[0]:vals[0]}
##                        else:
##                            feature[path[0]][path[1]]= {attrname[0]:vals}
##                    else:
##                        if type(vals)==list:
##                            feature[path[0]][path[1]][attrname[0]]= vals[0]
##                        else:
##                            feature[path[0]][path[1]][attrname[0]]= vals
##                    
##                    if len(attrname)>1:
##                        for i in range(1, len(attrname)):
##                            if type(vals)==list:
##                                feature[path[0]][path[1]][attrname[i]]= vals[i]
##                            else:
##                                feature[path[0]][path[1]][attrname[i]]= vals
##                        
##                 #Not functional yet   
##                elif pathlen==3:
##                    feature= self.mapdict['features'][i][path[0]][path[1]][path[2]]
##                elif pathlen==4:
##                    feature= self.mapdict['features'][i][path[0]][path[1]][path[2]][path[3]]
                
##                               
##    def matchDataToRegion(self, data, rgnid=None, lookup=None):
##        """Match data table with region names to corresponding map region id using lookup dictionary.
##                data - dictionary with key as region name and values as data values
##                rgnid - map ID attribute
##                lookup - path to lookup table file (string); if 'None', will default to 'RgnNames.txt'
##                    for map configuration
## 
##        """
##        ndata = data
##        
##        #Interactivley select map region ID if none specified
##        if rgnid ==None:
##            nrgnid = self.selectAttr()
##        else:
##            nrgnid = rgnid
##            
##        #Query map and get list of region IDs 
##        regionlist = []
##        for i in range(len(self.mapdict['features'])):
##            regionlist.append(str(self.mapdict['features'][i]["properties"][nrgnid]))
##
##        #Get dictonary of region names and IDs
##        lookupdict = self.csvToDict(lookupfile = lookup)
##
##        #If data's region names already match region ID, do nothing ---THIS NEEDS TO BE FIXED
##        datargns= [str(key) for key in ndata.keys()]
##        check = 0 #Counter of omitted region names
##        for r in datargns:
##            if r not in regionlist:
##                check+=1
##        if check==0:
##            return ndata
##
##        #Otherwise add region id value to each region in data
##        else:
##            check = 0
##            badrgns=[]
##            
##            for rgn in datargns:
##                if str(rgn) in lookupdict:
##                    newname= lookupdict[str(rgn)]
##                    ndata[newname]=ndata[rgn]
##                    del ndata[rgn]
##
##                else:
##                    if rgn in rgns:
##                        check +=1
##                        badrgns.append(rgn)
##                        print '%s not in lookup table.' %rgn
##                
## 
##        #Warn when some regions are omitted.
##        if check >0:
##            print 'Regions omitted from new dictionary: %s' %badrgns
##        return ndata
##        
##
##    def csvToDict(self,lookupfile=None):
##        #redundant--see waterdisag
##        """Transform two-column CSV file to dictionary.
##            lookupfile - path to two-column CSV lookup file
##
##            Return value - filedict - dictionary created from lookupfile
##        """
##
##        #Note: can potentially enhance this to allow for selection from multi-column lookup?
##        #Based on map region id?
##        
##        #Open lookup file
##        if lookupfile == None:
##            lookupfile = self.rgnconfig+'\RgnNames.txt'
##        infile = open(lookupfile, "r")
##        
##        reader = csv.reader(infile)
##        reader.next()
##
##        columns = defaultdict(list)
##        filedict = {}
##
##        #Transform 2-column csv to dictionary
##        #Move to util function eventually
##        for row in reader:
##            for (i,v) in enumerate(row):
##                columns[i].append(v)
##
##        #Strip extra characters from each column
##        for i in range(len(columns)):
##            columns[i] = [j.strip() for j in columns[i]]
##
##        #Add column 0 keys with column 1 values
##        for i in range(len(columns[0])):
##            filedict[columns[0][i]] = columns[1][i]
##       
##        infile.close()
##        return filedict
##

##
##    def getMapPath(self, query, fpath=None, pathlist = []):
##        #STill not working...
##        found = 0
##        if fpath==None:
##            path = self.mapdict["features"][0]
##        else:
##            path = fpath
##        
##        keys = [str(key) for key in path.keys()]
##                
##        if query not in keys:
##            try:
##                for key in keys:
##                    self.getMapPath(query, fpath= path[key])
##            except AttributeError:
##                pass
##        else:
##            found = 1
##            
##        if found == 1:
##            return path
##
##

## Test Code

#Test maps
##gcam_15_rgn = "GCAM_region.geojson"
##gcam_32_rgn_no_Taiwan = "GCAM_32_wo_Taiwan.geojson"
##gcam_china_rgn = "GCAM_CH_admin.geojson"
##gcam_water_basins="Global235_CLM_05_dissolve.geojson"
##
###Set up maps
##file1=open(gcam_15_rgn, "r")
##file2=open(gcam_32_rgn_no_Taiwan, "r")
##file3=open(gcam_china_rgn, "r")
##file4=open(gcam_water_basins,"r")
##
###Initialize Map
##gcam_map = MapType("gcam_map", file1, "../gcam-driver/input-data/rgn14")
##
###Add attributes
##waterdisag.init_rgn_tables(gcam_map.rgnconfig)
##gcam_map.appendNewAttr(["pop"])
##gcam_map.appendNewAttr(["2000"], data=waterdisag._gis2000, path=['properties','pop'],uniform=0, idname='GCAM_regio')
##
###Check results
##for feature in gcam_map.mapdict['features']:
##    print feature['properties']
##
###Display Results
####
##
##
###Export Results
###gcam_map.exportMapAsJSON()
##
##
##
###gcam_map.getProperties(printprop=1)
###gcam_map.selectProperty()
###print gcam_map.property
##
###print waterdisag._gis2000
###gcam_map.appendNewAttr("2000", data = waterdisag._gis2000, distinct=1, geoJSONpath=['features', 'properties', 'pop'])
##
##
###Tear down maps
##file1.close()
##file2.close()
##file3.close()
##file4.close()
