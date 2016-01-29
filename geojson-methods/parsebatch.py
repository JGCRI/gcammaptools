import csv
import string
import json
#New Function: Parse Batch output into tables.
#New table for each query and scenario

def parseBatchFile(filename,skip=0):
    """Convert Model Interface batch CSV to dictionary. First three columns of CSV must be
    query name, scenario, region. Queries should be separated by header lines. Aim is to make
    this a storable, accessable format from which you can access data for individual scenarios
    and queries to add to geoJSON map structure.
    
        Dict structure: 
            Level 0 keys: Query names.
                Value: List of scenario dictionaries
            Level 1 keys: scenario, categories, years, regions.
                Values: scenario name, list of categories, list of years. Regions is
                    a dictionary of region-level data.
            Level 2 keys: region name. 
                Values: List of tuples containing headers and data. To be parsed when extracted.  
    """

    with open(filename, 'r') as infile:
        data = {}
        reader= csv.reader(infile)
        
        for sk in range(skip):
            reader.next()

        #Initialize batch dictionary we are building
        batchdict = {}
        
        for ln in reader:
            ln = [item.strip() for item in ln]

            #When you hit a header line, parse headers
            if ln[0] == "title":
                (headers, cats, yrs, ncats, nyrs, lcats) = parseHeaders(ln)
                #rcats = ncats - lcats #Number of right categories after data(should only be units, 
                                #but some have extra fields. Not sure what I want to do with these yet.
                rgn = headers[2]
                #cats = cats[3:] #Strip off "title", "scenario", "region" fields from cats
    
                # print headers, cats, yrs, ncats, nyrs, lcats
                # break
            
            else:
                quer = ln[0] #Query name
                scen = ln[1]    #Scenario name
                
                #Initialize New Query
                if quer not in batchdict.keys():
                    batchdict[quer] = []

                    #Add first scenario name and dictionary of regions; list of category headers
                    batchdict[quer].append({"scenario":scen, rgn: {}, "categories": headers[2:],
                                            "years": yrs})
                    
                    #For simplicity's sake, add the data as tuples by region
                    #Not going into dynamic multi-level dictionary creation just yet. 
                    batchdict[quer][0][rgn][ln[2]]=[tuple(ln[2:])]

                #For existing Queries, check to see if scenario/region has already been entered
                else:
                    existing_scens = [i["scenario"] for i in batchdict[quer]]
                    if ln[1] in existing_scens:
                        index = matchIndex(ln[1], existing_scens)   #Find correct scenario entry

                        if ln[2] in batchdict[quer][index[0]][rgn].keys(): 
                            batchdict[quer][index[0]][rgn][ln[2]].extend([tuple(ln[2:])])
                        else:
                            batchdict[quer][index[0]][rgn][ln[2]]=[tuple(ln[2:])]
                    else:
                        batchdict[quer].append({"scenario":scen, rgn:{}, "categories": headers[2:], "years": yrs})
                        batchdict[quer][-1][rgn][ln[2]]= [tuple(ln[2:])]

        infile.close()
        return batchdict
                    

def parseHeaders(line):
    headers = [str(item) for item in line]  #For ease of parsing, ensure all are strings

    ncats = 0
    nyears = 0
    lcats=0     # of cats to left of years
    
    cats = []
    years = []

    for header in headers:
        header = header.strip()
        try:
            header = int(header)
            nyears +=1
            years.append(header)
        except ValueError:
            #If batch file has been processed by R scripts, years may have "X1990" format
            if header[0:2]== "X1" or header[0:2] == "X2": 
                header = header[1:]
                nyears +=1
                years.append(header)
            #Otherwise add to ncats counter
            else:
                ncats +=1
                cats.append(header)

    #Get number of categories to left of data
    i = 0
    while headers[i][0] not in ("X", "1", "2"):
        lcats +=1
        i +=1
    

    return(headers, cats, years, ncats, nyears, lcats)


def matchIndex(val, l):
    """Utility function to return index of items in list that match value."""

    return [i for i, item in enumerate(l) if item==val]

def cleanTuple(tup):
    ntup = tuple([item for item in tup if item != ''])
    return ntup


#TODO: get single query from batch file, format, add to geojson dict 
def extractQuery(batchdict, qname, scenario=None): 
    """Function to extract query from batch dictionary as a list of tuples."""
    
    #TODO: User selected scenario function 
    if scenario == None:
        quer = batchdict[qname][0]

    else:
        scens = [i["scenario"] for i in quer]
        j = matchIndex(scenario, scens)
        quer = batchdict[qname][j[0]]

    #A few useful attributes to return
    years = quer["years"]
    scenario = quer["scenario"]
    cats = cleanTuple(quer["categories"])
    
    data = []
    for rgn in quer["region"]:
        for tup in quer["region"][rgn]: 
            data.append(cleanTuple(tup))
        
    
    return (data, scenario, years, cats)



#Test Code:
#file2 = parseBatchFile("../input-data/sample_batch_2.csv")
#json.dump(file2, open("t1.txt", "w"))

#a = (0,1,2,3,"","","",'')
#b = cleanTuple(a)

#Try converting to JSON--unsuccessful
##file1 = open("../input-data/sample_batch.csv", "r")
##file3 = open("../input-data/samp_json.geojson", "w")
##a = json.dump(file1, file3)
