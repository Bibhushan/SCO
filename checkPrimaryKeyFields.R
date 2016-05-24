
# Checks primary key data of a field. 
# Returns number of errors found

checkPrimaryKeyFields <- function(x, tableName, fieldName, 
                                  logFile = NULL, logDepth = 0){
  
  result <- 0
  
  # Check for null or blank values in primary key fields.
  
  nulls <- which(is.null(x) || is.na(x) || x=='')
  
  nullCount <- length(nulls)
  
  if (nullCount > 0) {
    
    errorMessage <- paste('Null values found in field', fieldName, 
                          'of table', tableName)
    
    writeToLog(errorMessage, type = 'Error', fileConxn = logFile,
               printToConsole = T)
    
    result <- nullCount

  }
  
  # check if all the records are unique or not.
  
  uniqueFieldsDiff <- length(x) - length(unique(x))
  
  if (uniqueFieldsDiff > 0) {
    
    errorMessage <- paste('Duplicate records found for primary key field', 
                          fieldName, 'in table', tableName )
    
    writeToLog(errorMessage, type = 'Error', fileConxn = logFile,
               printToConsole = T)
    
    result <- result + uniqueFieldsDiff
    
  }
  
  return(result)
  
}