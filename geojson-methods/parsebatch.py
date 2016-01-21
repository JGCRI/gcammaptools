import csv
import string
#New Function: Parse Batch output into tables.
#New table for each query and scenario

def parseBatchFile(filename,skip=0):
    """Convert Model Interface batch CSV to dictionary. First two columns of CSV must be
    query name, scenario. Queries should be separated by header lines.
        Dict structure: 
            Level 0 keys: Query names
            Level 1 keys: Scenarios
            Level 2 keys: region, sector(s), years
            Data in list form. 
    """

    with open(filename, 'r') as infile:
        data = {}
        reader= csv.reader(infile)
        
        for sk in range(skip):
            reader.next()

        #Get headers
        headers = reader.next()
        headers = [item.strip() for item in headers]
        #print headers

        #Initialize table
        row = reader.next()
        data[row[0]]={}
        data[row[0]][row[1]]={}

        for i in range(2,len(headers)):
            data[row[0]][row[1]][headers[i]]=[]
            data[row[0]][row[1]][headers[i]].append(row[i].strip())

     
        for row in reader:
            if (row[0].strip() != headers[0]):
                if row[1] in data[row[0]].keys():
                    for i in range(2,len(headers)):
                        data[row[0]][row[1]][headers[i]].append(row[i].strip())
                else:
                    data[row[0]][row[1]] = {}
                    for i in range(2,len(headers)):
                        data[row[0]][row[1]][headers[i]]=[]
                        data[row[0]][row[1]][headers[i]].append(row[i].strip())
            else:
                #Read new headers
                headers = row
                headers = [item.strip() for item in headers]
                #print "p",headers
                
                nrow = reader.next()
                #print nrow

                data[nrow[0]]={}
                data[nrow[0]][nrow[1]]={}

                for i in range(2,len(headers)):
                    data[nrow[0]][nrow[1]][headers[i]]=[]
                    data[nrow[0]][nrow[1]][headers[i]].append(nrow[i].strip())
                #print data[nrow[0]]

                
        #print data.keys()
        infile.close()
        #print data
        return(data)

#TODO: get single query from batch file, format, add to geojson dict 
#def getSingleQuery()
#a = parseBatchFile("../input-data/sample_batch_2.csv")
