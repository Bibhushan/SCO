
source('readTableFromSource.R')
source('writeToLog.R')
source('matchFieldNames.R')
source('getKeyFields.R')
<<<<<<< HEAD
source('checkPrimaryKeyFields.R')
source('runGenericTableChecks.R')
=======
<<<<<<< HEAD
source('checkPrimaryKeyFields.R')
source('runGenericTableChecks.R')
=======
>>>>>>> c5e5327653cc8d56148ff54d2f834fc741a437a0
>>>>>>> c9d3de9a7eee0b3bd8335064eb7357f25e77ad95

log <- 'log.txt'

errorLog <- 'ErrorLog.txt'

cat('\nReading data file sources...')

fileSources <- read.csv('fileSources.csv', stringsAsFactors = F)

cat('Completed.')

sites <- readTableFromSource('Site', fileSources)

products <- readTableFromSource('Product', fileSources)

periods <- readTableFromSource('Period', fileSources)

transModes <- readTableFromSource('TransMode', fileSources)

modelSettings <- readTableFromSource('ModelSettings', fileSources)

boms <- readTableFromSource('BOM', fileSources)

processes <- readTableFromSource('Process', fileSources)

prodFac <- readTableFromSource('ProductAtFacility', fileSources)

prodFacPer <- readTableFromSource('ProductAtFacilityInPeriod', fileSources)

transLanes <- readTableFromSource('TransportationLane', fileSources)

transLinks <- readTableFromSource('TransportationLink', fileSources)

demands <- readTableFromSource('Demand', fileSources)

dataDefinition <- read.csv('dataDefinition.csv', stringsAsFactors = F)

# close(log)
# close(errorLog)