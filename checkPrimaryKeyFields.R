
# Checks primary key data of a field. 
# Returns number of errors found

# Update: Implement capability to check multiple fields.
# x replaced by table, a data frame with the fields to be checked.
# pass only the key fields (primary key or foreign key) to this function
# parameter fieldName dropped as it can be determined from table.

checkPrimaryKeyFields <- function(table, tableName, 
                                  logFile = NULL, logDepth = 0){
  
  result <- 0
  
  fields <- names(table)
  
  # Check for null or blank values in primary key fields.
  
  for (x in fields){
  
    nulls <- which(is.null(table[, x]) | is.na(table[, x]) | table[, x] == '')
    
    nullCount <- length(nulls)
    
    if (nullCount > 0) {
      
      errorMessage <- paste0('Null values found in ', x, 
                            ' field of ', tableName, ' table.')
      
      writeToLog(errorMessage, type = 'Error', fileConxn = logFile,
                 printToConsole = T, depth = logDepth)
      
      result <- result + nullCount
  
    }
  
  }
  
  # check if all the records are unique or not.
  
  # earlier we used unique to find duplicates, 
  # with table data frame, we will use aggregate to find duplicates
  
  table$count <- 0
  
  uniqueFieldsDiff <- nrow(table) - nrow(aggregate(count~., data = table, FUN = length))
  
  if (uniqueFieldsDiff > 0) {
    
    errorMessage <- paste0('Duplicate records found for key fields in ', 
                           tableName, ' table.' )
    
    writeToLog(errorMessage, type = 'Error', fileConxn = logFile,
               printToConsole = T, depth = logDepth)
    
    result <- result + uniqueFieldsDiff
    
  }
  
  return(result)
  
}