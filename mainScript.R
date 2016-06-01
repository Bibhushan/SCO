
source('readTableFromSource.R')
source('writeToLog.R')
source('matchFieldNames.R')
source('getKeyFields.R')
source('checkPrimaryKeyFields.R')
source('runGenericTableChecks.R')
source('dataConversionWarningHandler.R')

file.create('ErrorLog.txt')

log <- 'log.txt'

errorLog <- 'ErrorLog.txt'

refCheckFile <- 'refCheckDef.csv'

logDepth = 0

depthPlusOne <- logDepth + 1

# load data tables from csv files in R environment.

source('loadDataFiles.R')

# run generic data checks on tables.
# fileSources has been loaded by the script ran before

source('runDataChecks.R')

# above script will check the data for valid field names and field types

