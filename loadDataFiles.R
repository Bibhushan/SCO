
# this scripts needs following variables in environment: logDepth

depthPlusOne <- logDepth + 1

writeToLog('Reading file sources...', fileConxn = errorLog, depth = logDepth,
           printToConsole = T, addTimeStamp = T)

FileSources <- read.csv('fileSources.csv', stringsAsFactors = F)

tableCount <- nrow(FileSources)

writeToLog('Completed', fileConxn = errorLog, printToConsole = T, addNewLine = F)

# instead of reading table name from the file sources 
# we will add all tables to the environment using a for loop

# first check if all the fileNames as unique

writeToLog('Loading data files...', fileConxn = errorLog,
           printToConsole = T, addTimeStamp = T, depth = logDepth)

duplicateTables <- tableCount - length(unique(FileSources$TableName))

if(duplicateTables > 0){
 
  msg <- paste0(duplicateTables, 
                ' duplicate definitions found for tables in file sources.' ) 
  
  writeToLog(message = msg, type = 'Error', fileConxn = errorLog,
             printToConsole = T, depth = depthPlusOne)
  
  stop()
  
} else {
  
  # just for fun try if there are unique case insensititve names
  
  dupRecs <- tableCount - length(unique(tolower(FileSources$TableName)))
  
  if (dupRecs > 0) {
    
    msg <- paste0(dupRecs, ' tables does not seem to have unique names.' )
    
    writeToLog(message = msg, type = 'Warning', fileConxn = errorLog, 
               printToConsole = F, depth = depthPlusOne)
    
  }
  
  # just for fun, also check if the file sources are unique for not.
  
  dupRecs <- tableCount - length(unique(tolower(FileSources$TableName))) 
  
  if (dupRecs > 0){
    
    writeToLog(message = 'Same file source has been specifed for one or more tables.', 
               type = 'Warning', fileConxn = errorLog, depth = depthPlusOne)
    
  }
  
  # now just add all the tables to default environment

  for (table in seq(1, tableCount)){
    
      if (file.exists(FileSources$FileName[table])) {
      
      writeToLog(paste0('Reading ', FileSources$TableName[table], ' table...'), 
                 fileConxn = errorLog, printToConsole = T, depth = depthPlusOne)
      
      try(assign(FileSources$TableName[table], 
                 read.csv(FileSources$FileName[table], stringsAsFactors = F)))
      
      writeToLog('Completed', fileConxn = errorLog, printToConsole = T, 
                 addNewLine = F)
      
    } else {
      
      msg <- paste0('File source ', FileSources$FileName[table], 
                    ' defined for ', FileSources$TableName[table], 
                    ' does not exist.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = errorLog, printToConsole = T, depth = depthPlusOne)
      
    }
    
  }
    
}

writeToLog('Completed', fileConxn = errorLog, printToConsole = T)

