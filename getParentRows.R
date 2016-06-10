
getParentRows <- function(childRow, childName, parentName, set = 1, RefCheckDefinition, 
                          logFile = 'ErrorLog.txt', logDepth = 0){
  
  if(nrow(childRow) == 1){
    
    tableDef <- RefCheckDefinition[(RefCheckDefinition$ChildTable == childName &
                                     RefCheckDefinition$ParentTable == parentName &
                                      RefCheckDefinition$Set == set), ]
    
    if (nrow(tableDef) > 0){
      
      if (!exists(childName)) {
        
        writeToLog(message = paste0(childTable, ' table does not exist.'), 
                   type = 'Error', fileConxn = logFile, printToConsole = T, depth = logDepth)
        
        return(NULL)
        
      }
      
      if (!exists(parentName)){
        
        msg <- paste0('Parent table ', parentName, ' specified for ', childName, 
                      ' does not exist.')
        
        writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
                   printToConsole = T, depth = logDepth)
        
        return(NULL)
        
      }
      
      childTable <- childRow[, tableDef$ChildField]
      parentTable <- get(parentName)[, tableDef$ParentField]
      
      parentTable$rowIndex <- seq(1, nrow(parentTable))
      
      temp <- merge(parentTable, childTable, by.x = tableDef$ParentField, 
                    by.y = tableDef$ChildField, all.x = F, all.y = F)
      
      return(temp$rowIndex)
      
    } else {
      
      msg <- paste0('Parent table ', parentName, ' not defined for ', childName, 
                    '. No parent rows could be fetched.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
                 printToConsole = T, depth = logDepth)
      
    }
    
  } else {
    
    writeToLog('Please specify one and only one row to use getParentRows function', 
               type = 'Error', fileConxn = logFile, printToConsole = T,depth = logDepth)
    
  }
  
}