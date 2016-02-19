#!usr/bin/env python
"""A class of functions for manipulating GeoJSON maps for use in GCAM
visualization tasks. Creates a Gcam_map instance and alters the map's geoJSON
structure, stored as a python dictonary object. Functionality includes
importing data as map attributes, modifying map attributes and exporting
maps for use in subsequent visualization tasks."""

#TO-DOs:
# Add functionality with lookup file, drop file, region translation file
# Streamline/group helper functions -- some duplication
# Link up with scenario/batch csv processing functions (located in parsebatch.py)
# Need to consider ways of organizing more complex nested queries; may change current data
# organization


import json
import csv
from collections import defaultdict
import string
import sys, os

class Gcam_map:

    def __init__(self, mapfile, rgnconfig=None):
       
        #Allow for input of dictionary object or map file
        if isinstance(mapfile, dict):
            self.map = mapfile

        else:
        #Get files of interest from configuration for use in formatting
        #scenario data later
        #Assumes files located in same folder as mapfile.
        ##TODO - Not sure if this is right way to do this. 
            if rgnconfig == None:
                rgnconfig = os.path.dirname(mapfile)

            self.rgnconfig = rgnconfig

            rtfile = '%s/rgn-name-translation.csv' %rgnconfig
            dropfile = '%s/drop-regions.txt' %rgnconfig
            lookupfile = '%s/lookup.txt' %rgnconfig

            #If files above exist, store for use in later fcns
            if os.path.exists(rtfile):
                self.rtfile = rtfile
            if os.path.exists(dropfile):
                self.dropfile = dropfile
            if os.path.exists(lookupfile):
                self.lookupfile = lookupfile

            #Load geojson map   
            fn = open(mapfile, 'r')
            self.map = json.load(fn)
            fn.close()


    #-------------------Level 1: User Functions----------------------- 
    def appendNew(self, path, nkey=None, nvals=None):
        """Append new dictionary keys and/or values to map object.
            Inputs:
                path - path to location where you want to append of form ["key1",[],"key2"].
                    [] denotes a list and will append new key to every item in that list that
                    meets path criteria.
                nkey - name of new key you'd like to append
                nvals - values you want to append. Either single value or list of tuples with
                    regional identifier as first entry.
        """

        # Step 1: Append new key, if applicable
        if nkey != None:
            self.map = self.appendKey(self.map, path, nkey)
            path.append(nkey)
            
        #Step 2: Append new values if applicable
        if nvals != None:
            if isinstance(nvals, list) and len(path)>1:
                vals = self.groupData(nvals)
                vals = self.order(vals)
            else:
                vals=nvals
   
            self.map=self.mod_dict(path, vals, obj=self.map)

        #Self.map is modified, so no return value


    def delete(self, target_path, nkey, obj=None, path=None):
        """Delete key in dictionary by looping through recursively. 
            Inputs - 
                target_path - list of type ["key1", [], "key2"] where [] denotes a list.
                nkey - name of key you want to delete (str)
                obj - optional; specify if you're querying a dict other than self.map
                path - do not change. Will change with recursive iterations. 
            Modification of appendKey. Think about consolidating.
        """
        
        if obj is None:
            obj=self.map
        
        if path is None:
            path = []

        if isinstance(obj,dict) and path != target_path:
            value = {k: self.delete(v, target_path, nkey, path + [k])
                     for k,v in obj.items()}
        
        elif isinstance(obj,dict) and path==target_path:
            del obj[nkey]
            value = obj
            
        elif isinstance(obj, list) and path != target_path:
            value = [self.delete(elem, target_path, nkey, path + [[]])
                     for elem in obj]

        elif isinstance(obj, list) and path == target_path:
            for i in obj:
                del i[nkey]
            value = obj

        #Terminal value for recursion: if obj is not a dict or list
        else:
             value = obj

        return value


    def rename(self,target_path,oldname,newname,obj=None,path=None):
        """Rename key in dictionary by looping through recursively. 
            Inputs -
                target_path - list of type ["key1", [], "key2"] where [] denotes a list.
                oldname, newname - strings; names of old key and replacement key
                obj - optional; specify if you're querying a dict other than self.map
                path - do not change. Will change with recursive iterations. 
            Modification of appendKey. Think about consolidating.
        """
        if obj is None:
            obj=self.map
            
        if path is None:
            path = []

        if isinstance(obj,dict) and path != target_path:
            value = {k: self.rename(v, target_path, oldname, newname, path + [k])
                     for k,v in obj.items()}
        
        elif isinstance(obj,dict) and path==target_path:
            obj[newname] = obj.pop(oldname)
            value = obj
            
        elif isinstance(obj, list) and path != target_path:
            value = [self.rename(elem, target_path, oldname, newname, path + [[]])
                     for elem in obj]

        elif isinstance(obj, list) and path == target_path:
            for i in obj:
                i[newname]= i.pop(oldname)
            value = obj

        #Terminal value for recursion: if obj is not a dict or list
        else:
             value = obj

        return value


    def queryAttr(self, target_path, attr, obj=None, path=None):
        """Query a dictionary key (attr) and get a list of values. Utilizes traverse
            function to recursively loop through map, but does not modify map.
            Inputs:
                target_path - list of type ["key1", [], "key2"] where [] denotes a list.
                attr - attribute you want to query.
                obj - optional; specify if you're querying a dict other than self.map
                path - do not change. Will change with recursive iterations. 
        """
        if obj==None:
            obj=self.map
            
        self.value = []
        target_path.append(attr)

        def match_path(path, value):
            if path==target_path:
                self.value.append(value)
            return value
            
        a = self.traverse(self.map,func=match_path)
        return self.value
        

       
    def exportMapAsJSON(self, filename = None):
        """Write map dictionary to geoJSON outfile.
            filename - string
        """
        if filename==None:
            filename = 'map1' + '.geojson'

        filename = self.rgnconfig + filename
        
        with open(filename, 'w') as outfile:
            json.dump(self.mapdict, outfile)

        outfile.close()


     #-------------------Level 2: Helper functions-----------------------    
    def traverse(self, obj, path=None, func=None):
        """Generalized function to recursively loop through and replicate geoJSON dictionary.
            Helper function for mod_dict. Low-level function; not for user modification. 
                obj - geoJSON dictionary
                path - recursively builds a path through dictionary. Should initially be set to None.
                    Note that path changes throughout recursive function; this is intentional.
                func - A function to modify values in the dictionary. Can be targeted to path in
                    mod_dict.
                    
            Returns geoJSON dictionary (with modified values specified by func).

            Credit for code: http://nvie.com/posts/modifying-deeply-nested-structures/
                Note: do we need to ask for permission?
        """
        
        #Initialize path. Path should be set to None when first calling the function;
        #will change in recursive calls 
        if path is None:
            path = []
        
        #If obj is dictionary, recursively loop through k,v, adding to
        #path until terminal value is reached
        if isinstance(obj, dict):
            value = {k: self.traverse(v, path + [k], func)
                     for k,v in obj.items()}

        #If object is list, recursively loop through items in list, adding to path until
        #terminal value is reached
        elif isinstance(obj, list):
            value = [self.traverse(elem, path + [[]], func)
                     for elem in obj]


        #Terminal value for recursion: if obj is not a dict or list
        else:
             value = obj

        #Apply function to terminal value or return as-is
        if func == None:
            return value
        else:
            return func(path, value)


    def appendKey(self, obj, target_path, nkey, path=None):
        """Modification of traverse function. Recursively append a new dictionary key or list item to
            existing dictionary in all places where path = target_path.
            Inputs: 
                obj - dictionary object. Changes with recursive calls.
                target_path - target path of form ["key1",[],"key2"]. [] denotes a list
                    and will append new key to every item in that list that meets path criteria.
                nkey - name of dictionary key to append. Will be initialized with value of 1.
                path - value that changes with recursive calls. DO NOT INPUT THIS WHEN CALLING.

            Note that this function could be modified to append items other than a dictionary key.
            Derived from http://nvie.com/posts/modifying-deeply-nested-structures/
        """ 
        if path is None:
            path = []

        if isinstance(obj,dict) and path != target_path:
            value = {k: self.appendKey(v, target_path, nkey, path + [k])
                     for k,v in obj.items()}
        
        elif isinstance(obj,dict) and path==target_path:
            obj[nkey] = None
            value = obj
        elif isinstance(obj, list) and path != target_path:
            value = [self.appendKey(elem, target_path, nkey, path + [[]])
                     for elem in obj]

        elif isinstance(obj, list) and path == target_path:
            obj.append({nkey:None})
            value = obj
        #Terminal value for recursion: if obj is not a dict or list
        else:
             value = obj

        return value


    
    def mod_dict(self, target_path, data, obj=None):
        """Utilizes traverse function to build copy of obj (geoJSON dictionary) and add
            or modify data at specified path. For regional data, takes pre-ordered list with regions
            in same order as map features in dict. 
            Inputs - 
                target_path - path to items you want to modify. List form. If path leads to a list,
                    use [] to denote stepping into list elements.
                data - if regional, list of lists or list of lists of tuples.
                obj - optional;  
            Derived from http://nvie.com/posts/modifying-deeply-nested-structures/
        """
        if obj==None:
            obj = self.map
        
        #match_val function is called for every path/value in geoJSON object
        #Works only for terminal strings or ints/floats

        self.data= data
        self.nstrings = self.detect_fields(self.data)
        
        def match_val(path, value):
            
            if path == target_path:
                if value==None:
                    if (isinstance(self.data, list) or isinstance(self.data,tuple)) and len(self.data)>0:
                      #  try:
                        value = self.data[0] #Don't worry about clipping query fields for now
                     #   except IndexError:
                      #      value = [self.data[0]]
                    else:
                        value = [self.data]
                                    
                else:
                    if isinstance(self.data,list) or isinstance(self.data,tuple):
                       # try:
                        value.extend(self.data[0])
                      #  except IndexError:
                      #      value.extend(self.data[0])
                    else:
                        value.append(self.data)          
                if isinstance(self.data,list) or isinstance(self.data,tuple):
                    self.data = self.data[1:]

                return value
            
            else:
                return value

        return self.traverse(obj, func=match_val)


    def detect_fields(self, data):
        """Detect number of query fields  (strings) in data. Data can be list of tuples,
            tuple of tuples, etc.
            Haven't found a good use for this yet; tbd. 
        """
        
        #Assumption: all lists in data have same # of query fields
        nfields = 0
        ndata = 0

        if type(data)!= list and type(data)!=tuple:
            return(nfields)
        
        #Determine how nested the data is:
        while ndata == 0:
            try:
                if type(data[0])==str or type(data[0])==int:
                    ndata= data
                else:
                    data = data[0]
                    
            except IndexError:
                ndata = data

        for item in ndata:
            try:
                float(item)
                break
            except ValueError:
                nfields +=1
                #print item
            
        return(nfields)


    #-------------Data processing helper functions------------------------
    # These functions make some assumptions about how data is to be organized/appended to map.
    # This may change in the future if we find a better way. 
    def order(self, vals, refkey= None, id1=0, master = None):
        """Orders a list of tuples such that they match the ordering of regions in
            geoJSON map (or in master list). Preparation for appending tuple data to map.
            Inputs:
                vals - a list of lists of tuples; pre-grouped by region (see groupData)
                refkey - key of region identifier in map (ex: GCAM_ID or REGION_NAME).
                    Assumed to be located in "properties" tag of each feature.
                id1 - location of tuple identifier in tuple (e.g. first entry, second, etc)
                master - optional; master list if not using geoJSON region ordering. 
        """
        #Refkey necessary if not providing a master list
        if master==None:
            if refkey==None:
                refkey='REGION_NAME'
         
            master = [i["properties"][refkey] for i in self.map["features"]]
            #print len(master)
            
        norder = []
        val_ids = [tup[id1][id1] for tup in vals] 

        for item in master:
            #If region in master not in data, add NA tuple
            if item not in val_ids:
                norder.append(tuple(["#N/A"]))
            else:
                for tup in vals:
                    #Ignore values not in master data
                    if tup[id1][id1] not in master:
                        pass

                    #Append tuple to list if id = master id
                    elif tup[id1][id1]==item:
                        norder.append(tup)
                        #break


        #Returns list of tuples in same order as items in master. Will break if not same length. 
        assert(len(norder) == len(master))
        return norder
            

    def groupData(self, data, id1=0):
        """Groups data of same region into lists of tuples.
            Data - list of tuples.
            id1 - region identifier
        """
        ndata = []

        if type(data[0])==list or type(data[0])==tuple:
            #Append first data grouping
            i = 1
            intdata = [data[0]]
            
            for item in data[1:]:
                if item[id1] == intdata[0][id1]:
                    intdata.append(item)
                else:
                    ndata.append(intdata)
                    intdata = [item]
        else:
            ndata = data
        
        return ndata

            
    
