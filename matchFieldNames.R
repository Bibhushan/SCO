
# This function matches the field in a table with those defined in a list of fields
# handles duplicates in the fieldNames list

# Any errors are written to the log file specified here.

# table: a table with will be checked
# fieldNames: a list with field names
# tableName: name of the table for error reporting purpose
# logFile: file connection where errors will be appended

matchFieldNames <- function(table, fieldNames, tableName, logFile, logDepth = 0){
  
  tbl<- table(fieldNames)
  
  # first check if all the fieldNames are unique
  
  if (length(fieldNames) > length(tbl)){
    
    msg <- paste('Duplicate field definitions found for ', 
                 paste0(names(tbl)[tbl>1]), 'in table', tableName,
                 '. Only one of the field definitions will be checked.')
    
    writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)
    
    fieldNames <- names(tbl)
    
  }
  
  tableFields <- names(table)
  
  # find fields provided in definition but not present in data table
  
  tempCols <- which(!fieldNames %in% tableFields)
  
  tempColCount <- length(tempCol)
  
  if (length(tempCols) > 0){
    
    msg <- paste('Fields present in file definitions but not present in data table:', 
                 paste0(fieldNames[tempCols]))
    
    writeToLog(message = msg,type = 'Warning', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)
    
  }
  
}