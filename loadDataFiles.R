
log <- file('log.txt')

errorLog <- file('ErrorLog.txt')

cat('\nReading data file sources...')

fileSources <- read.csv('fileSources.csv', stringsAsFactors = F)

cat('Completed.')

sites <- readTableFromSource('Site', fileSources)

products <- readTableFromSource('Product', fileSources)

periods <- readTableFromSource('Period', fileSources)

transModes <- readTableFromSource('TransMode', fileSources)

modelSettings <- readTableFromSource('ModelSettings', fileSources)

processes <- readTableFromSource('Process', fileSources)

prodFac <- readTableFromSource('ProductAtFacility', fileSources)

prodFacPer <- readTableFromSource('ProductAtFacilityInPeriod', fileSources)

transLanes <- readTableFromSource('TransportationLane', fileSources)

transLinks <- readTableFromSource('TransportationLink', fileSources)

demands <- readTableFromSource('Demand', fileSources)

dataDefinition <- read.csv('dataDefinition.csv', stringsAsFactors = F)

close(log)
close(errorLog)