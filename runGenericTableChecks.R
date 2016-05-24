#run generic checks on the tables based on data definitions

#dataDefinitions is a data frame with following field: tableName, fielName,
#fieldType, dataType, defaultValue, minValue and maxValue.

# This function returns a list with three objects: Errors, which returns the number
# of errors found, Warnings, which counts the number of warnings and table, 
# which returns the table data modified based on the data definitions.

runGenericTableChecks <- function(table, tableName, dataDefinition, 
                                  logFile = NULL, logDepth = 0){
  
  msg <- paste('\nRunning generic checks on', tableName, 'table...')
  
  writeToLog(msg, type = 'Message', fileConxn = logFile, printToConsole = T, logDepth)
  
  result <- list(Errors = 0, Warnings = 0, Table = table)
  
  tableDef <- dataDefinition[dataDefinition$TableName == tableName,]
  
  defCount <- nrow(tableDef)
  
  #exit if no field definitions are found 

  if (defCount == 0){
    
    msg <- paste('No field definitions found for table', tableName)
    
    writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)
    
    result$Errors = 1
    
  } else {
  
    # Perform basic field checks on the fields in data table and 
    
    keyFields <- getKeyFields(tableDef$FieldName, tableDef$FieldType) 
    
    res <- matchFieldNames(table, tableDef$FieldNames, keyFields, tableName, 
                           logFile, logDepth + 1)
    
    result$Errors = result$Errors + res$Errors
    result$Warnings = result$Warnings + res$Warnings
    
    if (res$Errors == 0) result$Table <- res$Table
        
    # check field type of data
    # convert to respective types if needed.
    
    fieldNames <- names(result$Table)
    
    for (field in fieldNames){
      
      fieldType <- tolower(tableDef$FieldType[tableDef$FieldName == field])
      
      fieldClass <- class(result$Table[, field])
      
      if (fieldType == 'string'){
        
        if (!fieldClass == 'character'){
          
          msg <- paste('Data type of', field, 'in', tableName, 'is', fieldClass,
                       '. Converting to expected field type String.')
          
          writeToLog(message = msg,type = 'Warning', fileConxn = logFile,
                     printToConsole = F,depth = logDepth + 1)
          
          result$Table[, field] <- as.character(result$Table[, field])
          
        }
        
      } else if (fieldType == 'numeric'){
        
        if (!fieldClass == 'numeric'){
          
          msg <- paste('Data type of', field, 'in', tableName, 'is', fieldClass,
                       '. Converting to expected field type Numeric.')
          
          writeToLog(message = msg,type = 'Warning', fileConxn = logFile,
                     printToConsole = F,depth = logDepth + 1)
          
          result$Table[, field] <- as.numeric(result$Table[, field])
          
        }
        
      } else if (fieldType == 'date') {
        
        if (!fieldClass %in% c('POSIXct', 'POSIXt')){
          
          msg <- paste('Data type of', field, 'in', tableName, 'is', fieldClass,
                       '. Converting to expected field type Date.')
          
          writeToLog(message = msg,type = 'Warning', fileConxn = logFile,
                     printToConsole = F,depth = logDepth + 1)
          
          result$Table[, field] <- as.numeric(result$Table[, field])
          
        }
        
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