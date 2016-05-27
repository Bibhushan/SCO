
source('readTableFromSource.R')
source('writeToLog.R')
source('matchFieldNames.R')
source('getKeyFields.R')
source('checkPrimaryKeyFields.R')
source('runGenericTableChecks.R')

log <- 'log.txt'

errorLog <- 'ErrorLog.txt'

cat(paste(Sys.time(), 'Started loading data', sep = ': '), 
    file = 'ErrorLog.txt', append = F)

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