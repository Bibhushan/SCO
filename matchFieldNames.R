
# This function matches the field in a table with those defined in a list of fields
# handles duplicates in the fieldNames list

# Any errors are written to the log file specified here.

# table: a table with will be checked
# fieldNames: a list with field names
# tableName: name of the table for error reporting purpose
# logFile: file connection where errors will be appended

matchFieldNames <- function(table, fieldNames, keyFields = NULL, tableName, logFile, logDepth = 0){
  
  result <- list(Errors = 0, Warnings = 0, Table = table)
  
  tbl<- table(fieldNames)
  
  # first check if all the fieldNames are unique
  
  if (length(fieldNames) > length(tbl)){
    
    msg <- paste0('Duplicate field definitions found for ', 
                 paste0(names(tbl)[tbl>1]), ' in table ', tableName,
                 '. Only one of the field definitions will be checked.')
    
    writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)
    
    result$Warnings <- result$Warnings + 1
    
    fieldNames <- names(tbl)
    
  }
  
  tableFields <- names(table)
  
  # find fields provided in definition but not present in data table
  
  tempCols <- which(!fieldNames %in% tableFields)
  
  tempColCount <- length(tempCols)
  
  if (tempColCount > 0){
    
    # Check if the missing fields are key fields
    
    missingFields <- fieldNames[tempCols]
    
    missingKeyFields <- which(missingFields %in% keyFields)
    
    missingKeyFieldCount <- length(missingKeyFields)
    
    if (missingKeyFieldCount > 0){
      
      msg <- paste0(missingKeyFieldCount, 
                    ' key fields are missing in the data table: ', 
                    paste0(missingKeyFields, collapse = ', '), '.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      result$Errors <- result$Errors + missingKeyFieldCount
      
    }
    
    missingFields <- missingFields[!missingFields %in% missingKeyFields]
    
    missingFieldCount <- length(missingFields)
    
    if (missingFieldCount > 0){
    
      msg <- paste0('There are fields present in field definitions but not present in data table: ', 
                   paste0(fieldNames[tempCols], collapse = ', '), '.')
      
      writeToLog(message = msg,type = 'Warning', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      result$Warnings <- result$Warnings + missingFieldCount
      
    }
    
  }
  
  #find fields in data table but not in the field definitions.
  
  missingFields <- tableFields[!tableFields %in% fieldNames]
  
  tempCount <- length(missingFields)
  
  if(tempCount >0){
    
    missingKeyFields <- which(missingFields %in% keyFields)
    
    missingKeyFieldCount <- length(missingKeyFields)
    
    if (missingKeyFieldCount > 0){
      
      msg <- paste0(missingKeyFieldCount, 
                    ' key fields are missing in the data table:', 
                   paste0(missingKeyFields, collapse = ', '), '.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      result$Errors <- result$Errors + missingKeyFieldCount
      
    }
    
    missingFields <- missingFields[!missingFields %in% missingKeyFields]
    
    missingFieldCount <- length(missingFields)
    
    if (missingFieldCount > 0){
    
      msg <- paste0(missingFieldCount, 
                    ' fields present in field definitions but not present in data table:', 
                   paste0(fieldNames[tempCols], collapse = ', '), '.')
      
      writeToLog(message = msg,type = 'Warning', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      result$Warnings <- result$Warnings + missingFieldCount
      
    }
    
  }
  
  #find fields in data table but not in the field definitions.
  
  tempCols <- which(!tableFields %in% fieldNames)
  
  tempCount <- length(tempCols)
  
  if(tempCount >0){
    
    msg <- paste0(tempCount, ' fields are present in data table with no field definition:', 
              paste0(tableFields[tempCols], collapse=', '), 
              '. Only fields present in data field definitions will be used.')
    
    # Check if the missing fields are key fields
    
    missingFields <- fieldNames[tempCols]
    
    missingKeyFields <- which(missingFields %in% keyFields)
    
    missingKeyFieldCount <- length(missingKeyFields)
    
    if (missingKeyFieldCount > 0){
      
      msg <- paste0(missingKeyFieldCount, 
                    ' key fields are missing in the data table:', 
                   paste0(missingKeyFields, collapse = ', '), '.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      result$Errors <- result$Errors + missingKeyFieldCount
      
    }
    
    missingFields <- missingFields[!missingFields %in% missingKeyFields]
    
    missingFieldCount <- length(missingFields)
    
    if (missingFieldCount > 0){
    
      msg <- paste0(missingFieldCount, 
                    ' fields present in field definitions but not present in data table:', 
                   paste0(fieldNames[tempCols], collapse = ', '), '.')
      
      writeToLog(message = msg,type = 'Warning', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      result$Warnings <- result$Warnings + missingFieldCount
      
    }
    
  }
  
  #find fields in data table but not in the field definitions.
  
  tempCols <- which(!tableFields %in% fieldNames)
  
  tempCount <- length(tempCols)
  
  if(tempCount >0){
    
    msg <- paste0(tempCount, 
                  ' fields present in data table with no field definition:', 
                  paste0(tableFields[tempCols], collapse=', '), 
                  '. Only fields present in data field definitions will be used.')
    
    writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)
    
    tempCols <- which(tableFields %in% fieldNames)
    
    result$Table <- result$Table[, tempCols]
    
    result$Warnings = result$Warnings + tempCount
    
  }
  
  return(result)
  
}