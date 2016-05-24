#run generic checks on the tables based on data definitions

#dataDefinitions is a data frame with following field: tableName, fielName,
#fieldType, dataType, defaultValue, minValue and maxValue.

# This function returns a list with two objects: hasErrors, which returns the number
# of errors found and table, which returns the table data modified based on the 
# data definitions.

runGenericTableChecks <- function(table, tableName, dataDefinition, logFile = NULL){
  
  msg <- paste('\nRunning generic checks on', tableName, 'table...')
  
  writeToLog(msg, type = 'Message', fileConxn = logFile, printToConsole = T)
  
  hasError <- 0
  
  result <- list(Errors = hasError, Table = table)
  
  tableDef <- dataDefinition[dataDefinition$TableName == tableName,]
  
  defCount <- nrow(tableDef)
  
  #exit if no field definitions are found 
  
  if (defCount == 0){
    
    cat(paste('No field definitions found for table', tableName))
    
    result$Errors = 1
    
  } else {
  
    # Make sure that the field names defined in the data definitions are unique.
    
    
    # Check if the fields defined in data definition are present in the data
    
    
    
    for (row in 1:defCount){
      
      fieldType <- tolower(tableDef$fieldType[row])
      
      if (fieldType == 'string'){
        
        if (!class(table[, row]) == 'character'){
          
          msg <- paste('Data type of', )
          
          writeToLog()
          
        }
        
      } else if (fieldType == 'numeric'){
        
        
      } else if (fieldType == 'date') {
        
        
      } else if (fieldType == 'boolean'){
        
        
      } else {
       
        msg <- paste('Invalid data type', tableDef$fieldType[row], 'for field', 
                     tableDef$fieldName[row], 'in table', tableName, '.')
        
        writeToLog(msg,type = 'Error', fileConxn = errorLog, printToConsole = T) 
        
      }
      
    }
    
    # Check all the primary keys
    
    tempRows <- which(tableDef$fieldType == 'PK')
    
    if (length(tempRows) > 0) {
      
      for (row in tempRows) {
      
        fieldName <- tableDef$fieldName[row]
        
        hasError = hasError + checkPrimaryKeyFields(table[,fieldName],tableName,fieldName,logFile)
      
      }
      
    }
  
  }
  
  if (result$Errors > 0) cat('\n')
  
  return(result)
  
}