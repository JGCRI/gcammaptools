import json
import csv
from Map import Map


file1 = open('../input-data/rgnusa/gcam_usa.geojson')
map1 = Map('GCAM_USA', file1)
file1.close()

l = map1.listAttrs(level=2)

m = map1.mapdict["features"][300]["properties"]

delAttrs = ["FUNCSTAT", "AWATER", "ISO_3DIGIT", "GUF", "MTFCC", "INTPTLAT",
            "FID_t1_201", "INTPTLON", "LSAD", "ISO_NUM", "STATENS", "DIVISION",
            "FIPS_CNTRY"]

for attr in delAttrs:
    map1.deleteAttr(level=2, attr=attr)

m = map1.mapdict["features"][300]["properties"]
