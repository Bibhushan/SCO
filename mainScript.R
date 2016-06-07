
source('readTableFromSource.R')
source('writeToLog.R')
source('matchFieldNames.R')
source('getKeyFields.R')
source('checkPrimaryKeyFields.R')
source('runGenericTableChecks.R')
source('dataConversionWarningHandler.R')
source('referentialIntegrityCheck.R')

library(lpSolveAPI)

file.create('ErrorLog.txt')

log <- 'log.txt'

errorLog <- 'ErrorLog.txt'

refCheckFile <- 'refCheckDef.txt'

logDepth = 0

depthPlusOne <- logDepth + 1

# load data tables from csv files in R environment.

source('loadDataFiles.R')

# run generic data checks on tables.
# fileSources has been loaded by the script ran before

# now check the data for valid field names and field types

source('runDataChecks.R')

# now check the referential checks.

source('runRefChecks.R')



