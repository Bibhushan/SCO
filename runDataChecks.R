
# make sure that the data table, file sources and data definitions are loaded
# before this script is run

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
