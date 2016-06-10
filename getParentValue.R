
# this function returns the value from a specified field in the parent table.

getParentValue <- function(childRow, childName, parentName, fieldName, 
                            defaultValue = 0, set = 1, 
                            RefCheckDefinition, logFile = 'ErrorLog.txt', 
                            logDepth = 0){
  
  depthPlusOne <- logDepth + 1
  
  rows <- getParentRows(childRow = childRow, childName = childName, 
                        parentName = parentName, set = set, 
                        RefCheckDefinition = RefCheckDefinition, 
                        logFile = logFile, logDepth = depthPlusOne)
  
  if (!is.null(rows) && length(rows) > 0){
    
    if (length(rows) > 1){
      
      msg <- paste0('Multiple records were not found in parent table',
             parentName, ' for a row in ', childName, 
             ' table. Only first row will be used.')
      
      writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
     
      rows <- rows[1] 
      
    } 
    
    tbl <- get(parentName)
    
    if (fieldName %in% names(tbl)){
      
      return(tbl[rows, fieldName])
      
    } else {
      
      msg <- paste0(fieldName, ' field not found in the parent table ',
                    parentName, ' for ', childName,
                    ' table. Returning the default value.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
      return(defaultValue)
      
    }
    
  } else {
    
    msg <- paste0('Matching records were not found in parent table',
           parentName, ' for a row in ', childName, 
           ' table. Returning the default value.')
    
    writeToLog(message = msg, type = 'Warning', fileConxn = logFile, 
               printToConsole = F, depth = logDepth)
    
    return(defaultValue)
    
  }
  
}