
# reads the data table specified by the file path specified in data definition

# fileSources is a data frame with two fields, tableName and fileName

readTableFromSource <- function(tableName, fileSources, logDepth = 0){
  
  writeToLog(paste('Reading data from', tableName, 'table...'),fileConxn = log,
             printToConsole = T, depth = logDepth)
  
  #cat(paste('\nReading data from', tableName, 'table...'))
  
  tempRows <- which(fileSources$TableName == tableName)
  
  rowCount <- length(tempRows)
 
  if (rowCount < 1) {
    
    writeToLog(paste('No file source defined for table', tableName, '.'), 
               type = 'Error', fileConxn = errorLog,printToConsole = T, 
              depth  = logDepth+1)                
    
    #cat(paste('\nNo file source defined for table', tableName, '.'))
    
  } else {
    
    if (rowCount > 1){
      
      msg <- paste('Warning: More than one file sources defined for table', tableName, 
                   '. Only the first source will be used.\n')
      
      writeToLog(message = msg, fileConxn = 'Warning', errorLog, T, depth = logDepth +1)
    
#       cat(paste('\n\tWarning: More than one file sources defined for table', tableName, 
#                 '. Only the first source will be used.\n'))
#     
    }
    
    fileName <- fileSources$FileName[tempRows[1]]
    
    cat('Completed.')
    
    return(read.csv(fileName, stringsAsFactors = F))
    
  } 
  
}