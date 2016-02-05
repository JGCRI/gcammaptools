import string
import json
import csv
from Map import Map
from parsebatch import * #Todo: relocate this


###Task: Add GCAM scenario data table to geoJSON file
##Step 1: Format Map
file1 = open('../input-data/rgn32/GCAM_32_wo_Taiwan_clean.geojson', 'r')
map1 = Map('GCAM_32_wo_Taiwan', file1)
file1.close()

##Step 1a: Delete Coordinates
    #map1.deleteAttr(level=2, attr="coordinates")
    #print map1.listAttrs(level=2)

##Step 2: Add scenario from data
batch = parseBatchFile("../input-data/sample_batch_2.csv")

#Pull out primary energy from default scenario
quer = "primary_energy"
data = extractQuery(batch, quer)
ndata = data[0]

#Add scenario name, years to map
map1.appendNewAttr("scenario", data[1], level=0, levelnames=[])
map1.appendNewAttr("years", data[2], level=0, levelnames=[])

#Create "primary_energy" tag for each region
map1.appendNewAttr(quer, {}, level=1,levelnames=["features"])

#Order and append ndata
order = [i["properties"]["REGION_NAME"] for i in map1.mapdict["features"]]

###Uncleaned work below
#Sort ndata by order of features in map
#Q: dictionaries don't preserve order, right?

def sort_tups(master, tuplist, id1):
    norder = []
    
    for m in master:
        j = 0
        for tup in tuplist:
            #print len(tuplist)
            if tup[id1] not in master:
                pass
            
            if tup[id1] == m:
                norder.append(tup)
                break

            else:
                j +=1
            if j==len(tuplist):
                norder.append(tuple(["#N/A"]))

    return norder

b = [d[1] for d in ndata]
b = list(set(b))

dat = [tup for tup in ndata if tup[1]=="Oil"]
a = sort_tups(order,dat,0)

##e = set(i[0] for i in a)
##f = set(order)
###print e.difference(f)
###print f.difference(e)
##
##for fuel in b:
##    map1.appendNewAttr(fuel, None, level=2, levelnames=["features", "primary_energy"])
##
##mp = map1.mod_dict(obj=map1.mapdict, target_path = ["features", [], "primary_energy", "Oil"], data=a)
##
##for fuel in b[1:]:
##    dat = [tup for tup in ndata if tup[1]==fuel]
##    a = sort_tups(order, dat, 0)
##    mp = map1.mod_dict(obj=mp, target_path = ["features", [], "primary_energy", fuel], data=a)
##
##map1.mapdict = mp
##map1.exportMapAsJSON("GCAM_32_primary_energy_2.geojson")

#TEST code:
##
##def compare_features(obj, l):
##    for feature in obj:
##        for b in obj:
##            if feature["properties"]["REGION_NAME"] == b["properties"]["REGION_NAME"]:
##                for fuel in l:
##                    try:
##                        assert(feature["primary_energy"][fuel]==b["primary_energy"][fuel])
##                    except AssertionError:
##                        print feature["properties"]["REGION_NAME"], b["properties"]["REGION_NAME"], feature["primary_energy"][fuel], b["primary_energy"][fuel]
##            else:
##                for fuel in l:
##                    try: 
##                        assert(feature["primary_energy"][fuel]!=b["primary_energy"][fuel])
##                    except AssertionError:
##                        if fuel=="Other":#some fuels have value 0; ignore
##                            pass
##                        else:
##                            print feature["properties"]["REGION_NAME"], b["properties"]["REGION_NAME"], feature["primary_energy"][fuel], b["primary_energy"][fuel]
##
##
##compare_features(mp["features"], b)
##
##def check_names(obj):
##    i = 0
##    for feature in obj:
##        for tup in feature["primary_energy"]["fuel"]:
##            if feature["properties"]["REGION_NAME"] != tup[0]:
##                print i, feature["properties"]["REGION_NAME"], tup[0]
##                
##        i +=1
##
##
###check_names(mp["features"])
##        
###print a                 
###b = [t[0] for t in a]
##
##
###b = map1.mod_dict(obj=map1.mapdict, target_path=["features", [], "primary_energy", "fuel"], data=a)



#-------------------------------------------------------------
###Old, extraneous code
###TODO : figure out where this should go
#map2.appendNewAttr(map2.idName, m, uniform=0)
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





#file3.close()

