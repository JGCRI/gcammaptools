import string
import json
import csv
from Map import Map


#Map 3: GCAM_14.geojson

file2 = open('../input-data/rgn32/GCAM_32_wo_Taiwan.geojson', 'r')
map2 = Map('GCAM_32_wo_Taiwan', file2, './GCAM_32_wo_Taiwan.geojson', "GCAM_ID", "REGION_NAME")

map2.appendNewAttr("dummy", uniform=True)
l = map2.listAttrs()
print l
l = map2.queryAttr("dummy")
print l
map2.deleteAttr("dummy")
l = map2.listAttrs()
print l



##print l
##m = map2.queryAttr('GCAM_regio')
##print m
##
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




file2.close()
#file3.close()

