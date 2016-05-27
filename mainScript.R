
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

logDepth = 0

depthPlusOne <- logDepth + 1

# load data tables from csv files in R environment.

source('loadDataFiles.R')

# run generic data checks on tables.
# fileSources has been loaded by the script ran before

writeToLog('Running generic data checks...', fileConxn = errorLog, 
           printToConsole = T, addTimeStamp = T)

if (exists('FileSources')) {
  
  for (tbl in seq(1, tableCount)) {
    
    tableName <- FileSources$TableName[tbl]
    
    try(runGenericTableChecks(table = get(tableName), tableName = tableName, 
                              dataDefinition = DataDefinition, logFile = errorLog, 
                              logDepth = depthPlusOne))
    
  }
  
} else {
  
  cat('\n Please run loadDataFiles.R before running this')
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           printToConsole = T, depth = logDepth)
