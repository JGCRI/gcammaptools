"""Scratch script to clean up GCAM_USA map; in progress. Looking to eventually
systematize this."""

import json
import csv
from Map_redo import Gcam_map 


map1 = Gcam_map('../input-data/rgnusa/gcam_usa.geojson')

map1.appendNew(["features", [], "properties"], nkey='foo', nvals='bar')

#l = map1.listAttrs(level=2)

#m = map1.mapdict["features"][300]["properties"]

#delAttrs = ["FUNCSTAT", "AWATER", "ISO_3DIGIT", "GUF", "MTFCC", "INTPTLAT",
 #           "FID_t1_201", "INTPTLON", "LSAD", "ISO_NUM", "STATENS", "DIVISION",
  #
#for attr in delAttrs:
   # map1.deleteAttr(level=2, attr=attr)
#
#m = map1.mapdict["features"][300]["properties"]
