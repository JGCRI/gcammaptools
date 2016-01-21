import string
import json
import csv
from Map import Map
import numpy as np
from parsebatch import parseBatchFile #Todo: relocate this



#Task: Add GCAM scenario data table to geoJSON file
#Step 1: Format Map
file1 = open('../input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson', 'r')
map1 = Map('GCAM_32_wo_Taiwan', file1)

#Step 1a: Delete Coordinates
map1.deleteAttr(level=2, attr="coordinates")
#print map1.listAttrs(level=2)

#Step 1b: Add new level0 attributes
yrs = [1990]
yrs.extend(list(np.arange(2005,2105,5)))

map1.appendNewAttr("years", yrs, level=0, levelnames=[])
#print map1.listAttrs(level=0)
#print map1.mapdict["years"]

#Add scenario from data
file2 = open("../input-data/sample_batch.csv", 'r')

#New Function: Parse Batch output into tables.
#Aggregate Batch Dictionary
batch = parseBatchFile("../input-data/sample_batch_2.csv")

#Only want primary energy for reference scenario for now
c = batch["primary_energy"]
d = c.keys() #Scenario names list
e = batch["primary_energy"][d[1]]

#Add scenario name to map
map1.appendNewAttr("scenario", d[1], level=0, levelnames=[])
#print map1.listAttrs(level=0)
#print map1.mapdict["scenario"]

#Create "primary_energy" tag for each region
map1.appendNewAttr("primary_energy", {}, level=1, uniform=True,levelnames=["features"])
print map1.listAttrs(level=1)

#Clean primary energy function data
for key in e.keys():
    if key=="":
        del e[key]

regions = e["region"]

#Reformat output because parse function not right yet. 
ndict = {}
for region in regions:
    ndict[region]={}
    for key in e.keys():
        try: 
            if int(key) in yrs:
                dta = zip(e["region"], e["fuel"], e[key])
                for tup in dta:
                    if tup[0]==region and tup[1] not in ndict[region].keys():
                        ndict[region][tup[1]] = tup
                    elif tup[0]==region and tup[1] in ndict[region].keys():
                        ndict[region][tup[1]] = ndict[region][tup[1]] + (tup[2],)
        except ValueError:
            pass

#Now extract all the tuples
ndata = []
for region in ndict.keys():
    for fuel in ndict[region].keys(): 
        ndata.append(ndict[region][fuel])
  
#map1.appendNewAttr("fuel", [], level=2, uniform=True, levelnames=["features", "primary_energy"])

map1.appendNewAttr("fuel", ndata, level=2, uniform=False, levelnames=["features", "primary_energy"])




#print x1990
#print e.keys()






















##map2.appendNewAttr(map2.idName, m, uniform=0)
##
##with open('./input-data/rgn14/RgnNamesGCAM14.txt', 'r') as file3:
##    csv_reader = csv.DictReader(file3)
##    data = {}
##    for row in csv_reader:
##        for header, value in row.items():
##            try:
##                data[header].append(value)
##            except KeyError:
##                data[header] = [value]
##
##rgnname = data['region']
##idnm = data[' region_id']
##
##idnm = [d.strip() for d in idnm]
##print idnm
##o = zip(idnm, rgnname)
##print o
##
##z = map2.matchData(o, m)
##a = [None]
##a.extend(z)
##print a
##
##map2.appendNewAttr(map2.regionName, z, uniform=0)
##
##
##
##

###Map 2: GCAM_32_wo_Taiwan.geojson
##file2 = open('./GCAM_32_wo_Taiwan.geojson', 'r')
##map2 = MapType('GCAM_32_no_Taiwan', file2, './input-data/rgn32/', "GCAM_ID", "REGION_NAME")
##
##l = map2.getAttrs()
##
##m = map2.queryAttr('GCAM_30_re')
##
##map2.appendNewAttr(map2.idName, m, uniform=0)
##n = map2.queryAttr(map2.idName)
##
##
###Extract data from csv with headers
##with open('./gcam_32_wo_taiwan.csv', 'r') as file3:
##    csv_reader = csv.DictReader(file3)
##    data = {}
##    for row in csv_reader:
##        for header, value in row.items():
##            try:
##                data[header].append(value)
##            except KeyError:
##                data[header] = [value]
##
##gcam32name = data['GCAM_32_name']
##gcam32id = data['GCAM_32_re']
##gcam32data = zip(gcam32id, gcam32name)
##z = map2.matchData(gcam32data, n)
##
##map2.appendNewAttr(map2.regionName, z, uniform=0)
##
##l = map2.getLookupTable(export=1)
##
##map2.exportMapAsJSON()

##map2.appendNewAttr(map2.regionName, gcam32name, uniform=0)
##o = map2.queryAttr(map2.regionName)
##print o
                   






#Init a new map --may have the standardized map located in the region ID folder
##file1=open('.\GCAM_China.geojson', 'r')
##
##map2=MapType('GCAM_China', file1, '../gcam-driver/input-data/rgnchn', "GCAM_ID", "REGION_NAME")
##
##l = map2.getLookupTable(export=1)
##
##
###Create a standard map ID for china map
###Get list of names of map attributes under 'properties'
##gcam = map1.queryAttr('GCAM_30__1')
##provid = map1.queryAttr('Prov_ID')
##gcamname = map1.queryAttr('Region')
##provname = map1.queryAttr('AD2_NAME')
##
##while 11 in gcam:
##    gcam.remove(11)
##
##while None in provid:
##    provid.remove(None)
##
##while 'China' in gcamname:
##    gcamname.remove('China')
##
##while None in provname:
##    provname.remove(None)
##        
###Add prov id to GCAM32 id
##newid = gcam
##newid.extend(provid)
##
##newname = gcamname
##newname.extend(provname)
##
###Add full id list to map
##map1.appendNewAttr('GCAM_ID', newid, uniform=0)
##map1.appendNewAttr('REGION_NAME', newname, uniform=0)
##
###map1.exportMapAsJSON()
##
###Check to see if formatted correctly
##check = map1.queryAttr('GCAM_ID')
##check2 = map1.queryAttr('REGION_NAME')
##print check
##print check2




file1.close()
#file3.close()

