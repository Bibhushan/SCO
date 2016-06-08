
# this function simply returns the values from a column in a data frame.
# after converting the invalid values to the default values specified

getSanitisedValues <- function(fieldName, data, defaultValue = 0, 
                               tableName = 'Data', logFile = 'ErrorLog.txt', 
                               logDepth = 0){
  
  depthPlusOne <- logDepth + 1
  
  if(is.na(fieldName) || fieldName == ''){
    
    msg <- paste0('Null or blank value specified for field name in ', 
                  tableName, ' table. Returning the default value')
    
    writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
               printToConsole = F, depthPlusOne)
    
    return(rep(defaultValue, nrow(data)))
    
  }
  
  if (!fieldName %in% names(data)) {
    
    msg <- paste0(fieldName, ' field does not exist in ', tableName, 
                  ' table. Returning default values.')
    
    writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
               printToConsole = T, depthPlusOne)
    
    return(rep(defaultValue, nrow(data)))
    
  }
  
  result <- data[, fieldName]
  
  result[is.na(result)] <- defaultValue
  
  return(result)
  
}

