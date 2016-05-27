#run generic checks on the tables based on data definitions

#dataDefinitions is a data frame with following field: tableName, fielName,
#fieldType, dataType, defaultValue, minValue and maxValue.

# This function returns a list with three objects: Errors, which returns the number
# of errors found, Warnings, which counts the number of warnings and table, 
# which returns the table data modified based on the data definitions.

runGenericTableChecks <- function(table, tableName, dataDefinition, 
                                  logFile = NULL, logDepth = 0){
  
  msg <- paste('Running generic checks on', tableName, 'table...')
  
  writeToLog(msg, type = 'Message', fileConxn = logFile, printToConsole = T, logDepth)
  
  result <- list(Errors = 0, Warnings = 0, Table = table)
  
  tableDef <- dataDefinition[dataDefinition$TableName == tableName,]
  
  defCount <- nrow(tableDef)
  
  #exit if no field definitions are found 

  if (defCount == 0){
    
    msg <- paste0('No field definitions found for table', tableName, '.')
    
    writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)

    result$Errors = 1
    
  } else {
  
    # Perform basic field checks on the fields in data table and 

    keyFields <- getKeyFields(tableDef$FieldName, tableDef$FieldType) 
    
    res <- matchFieldNames(table, tableDef$FieldName, keyFields, tableName, 
                           logFile, logDepth + 1)
    
    result$Errors = result$Errors + res$Errors
    result$Warnings = result$Warnings + res$Warnings
    
    if (res$Errors == 0) result$Table <- res$Table
        
    # check field type of data
    # convert to respective types if needed.

    fieldNames <- names(result$Table)
    
    for (field in fieldNames){
      
      fieldType <- tolower(tableDef$DataType[tableDef$FieldName == field])

      fieldClass <- class(result$Table[, field])
      
      if (fieldType == 'string'){
        
        if (!fieldClass == 'character'){

          msg <- paste0('Data type of ', field, ' field in ', tableName, 
                        ' table is ', fieldClass,
                        '. Converting to expected field type String.')
          
          writeToLog(message = msg,type = 'Warning', fileConxn = logFile,
                     printToConsole = F,depth = logDepth + 1)
          
          result$Table[, field] <- as.character(result$Table[, field])
          
        }
        
      } else if (fieldType == 'numeric'){
        
        if (!fieldClass %in% c('numeric', 'integer')){
          
          msg <- paste0('Data type of ', field, ' field in ', tableName, 
                        ' table is ', fieldClass, 
                        '. Converting to expected field type Numeric.')
          
          writeToLog(message = msg,type = 'Warning', fileConxn = logFile,
                     printToConsole = F,depth = logDepth + 1)
          
          result$Warnings <- result$Warnings + 1
          
          result$Table[, field] <- as.numeric(result$Table[, field])
          
        }
        
      } else if (fieldType == 'date') {
        
        if (!fieldClass %in% c('POSIXct', 'POSIXt')){
          
          msg <- paste0('Data type of ', field, ' field in ', tableName, 
                        ' table is ', fieldClass, 
                        '. Converting to expected field type Date.')
          
          writeToLog(message = msg,type = 'Warning', fileConxn = logFile,
                     printToConsole = F,depth = logDepth + 1)
          
          result$Warnings <- result$Warnings + 1
          
          # currently only dd/mm/yyyy format is supported
          
          result$Table[, field] <- as.POSIXct(as.Date(result$Table[, field],
                                                      '%d/%m/%Y'))
        }
        
      } else if (fieldType == 'boolean'){
        
        # we will implement a dumb way to coerce values to boolean
        # all values starting with 'T', 'Y', or numbers greater than 0 will be
        # converted to True (1), all others will be default value of False (0)
        
        if(!fieldClass %in% c('numeric', 'integer')){
          
          invalidRows <- which(!result$Table[, field] %in% c(0, 1))
          
          invalidRowCount <- length(invalidRows)
          
          if(invalidRowCount > 0){
           
            msg <- paste0(invalidRowCount, ' invalid rows found in ', field, 
                         ' field for table ', tableName, 
                         '. Replacing them with default values.')
            
            writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
                       printToConsole = F,depth = logDepth + 1)
            
            result$Warnings <- result$Warnings + invalidRowCount
            
            tempRows <- which(results$Table[, field] <= 0)
            
            result$Table[tempRows, field] <- 0
            result$Table[-tempRows, field] <- 1
            
          }
          
        } else {
          
          # if the boolean field happens to be non-numeric, we cast it to character
          # and convert all values which start from 'T' (True) or 'Yes' to 1 (True))
          # All other values are set to default value of 0 (False).
          
          fieldValues <- tolower(substr(as.character(result$Table[, field]),
                                start = 1, stop = 1))
          
          invalidRows <- which(!fieldValues %in% c('t', 'f', 'y', 'n'))
          
          invalidRowCount <- length(invalidRows)
          
          if (invalidRowCount > 0) {
            
            msg <- paste0(invalidRowCount, ' values in ', field, 
                          ' field of ', tableName, 
                          ' table could not be identified as valid boolean fields.',
                          ' They will be converted to default value of false.')
            
            writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
                       printToConsole = F, depth = logDepth + 1)
            
            result$Warnings <- result$Warnings + 1
            
            falseRows <- merge(invalidRows, which(fieldValues %in% c('f', 'n')))
            
            result$Table[, field] <- 1
            result$Table[falseRows, field] <- 0
            
          }
          
        }
        
        
      } else {
       
        msg <- paste0('Invalid data type ', tableDef$DataType[row], ' for field ', 
                     tableDef$FieldName[row], ' in table ', tableName, '.')
        
        writeToLog(message = msg,type = 'Error', 
                   fileConxn = logFile, printToConsole = T, logDepth + 1) 
        
        result$Errors <- result$Errors + 1
        
      }
      
    }
    
    # Check all the primary keys
    
    pkRows <- which(tableDef$fieldType == 'PK')
    
    pkCount <- length(pkRows)
    
    if (pkCount > 0) {
      
      for (row in pkRows) {
      
        fieldName <- tableDef$fieldName[row]
        
        result$Errors <- result$Errors + 
              checkPrimaryKeyFields(x = table[,fieldName], tableName = tableName, 
                                     fieldName = fieldName, logFile = logFile,
                                     logDepth = logDepth + 1)
        
        
      
      }
      
    }
  
  }
  
  msg <- ''
  
  if (result$Errors + result$Warnings > 0) msg <- (paste0('\n', 
                                   paste0(rep(' ', logDepth),   collapse = ' ')))
  
  cat(paste0(msg, 'Completed.', collapse = ''), file = logFile, append = T)
  
  return(result)
  
}