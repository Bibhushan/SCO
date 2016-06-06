
# This script will assume that all the data tables whose referential integrity
# needs to be checks are already loaded in the environment.

writeToLog('Running referential data checks...', fileConxn = errorLog, 
           printToConsole = T, addTimeStamp = T)

# First, read the field definition file

refCheckDefs <- read.csv('RefCheckDefinition.txt', stringsAsFactors = F)

childTables <- unique(refCheckDefs$ChildTable)

childTableCount <- length(childTables)

if(childTableCount > 0){

  for (child in childTables){
    
    msg <- paste0('Performing ref check for fields in ', 
                  child, ' table...')
    
    writeToLog(message = msg, fileConxn = errorLog, printToConsole = T, 
               depth = depthPlusOne)
    
      
    if (exists(child)) {
      
      tblDef <- refCheckDefs[refCheckDefs$ChildTable == child, ]
      
      parentTables <- unique(tblDef$ParentTable)
      
      for (parent in parentTables){
        
        if (exists(parent)){
          
          rows <- which(tblDef$ParentTable == parent)
          
          # check if the fields defined for parent and child tables 
          # are present in the respective tables.
          
          childTable <- get(child)
          
          invalidChildFields <- tblDef$ChildField[rows][!tblDef$ChildField[rows] 
                                                   %in% names(childTable)]
          
          invalidChildFieldCount <- length(invalidChildFields)
          
          if (invalidChildFieldCount > 0) {
            
            msg <- paste0(invalidChildFieldCount, ' fields defined for reference check of ', 
                          child, ' table does not exist: ', 
                          paste0(invalidChildFields, collapse = ', '), '.')
            
            writeToLog(message = msg, type = 'Error', fileConxn = errorLog, 
                       printToConsole = T, depth = depthPlusOne)
            
            
          } else {
            
            childTable <- childTable[, tblDef$ChildField[rows], drop = F]
            
          }
          
          # check if the fields defined for parent tables exist
          
          parentTable <- get(parent)
          
          invalidParentFields <-  tblDef$ParentField[rows][!tblDef$ParentField[rows] 
                                                           %in% names(parentTable)]
          
          invalidParentFieldCount <- length(invalidParentFields)
          
          if (invalidParentFieldCount > 0) {
            
            msg <- paste0(invalidParentFieldCount, ' fields defined as parent fields for reference check of ', 
                          child, ' table does not exist in corresponding parent table ', 
                          parent, ': ', paste0(invalidParentFields, collapse = ', '), '.')
            
            writeToLog(message = msg, type = 'Error', fileConxn = errorLog, 
                       printToConsole = T, depth = depthPlusOne)
            
            
          } else {
            
            parentTable <- parentTable[, tblDef$ParentField[rows], drop  = F]
            
          }
            
          if(invalidChildFieldCount + invalidParentFieldCount == 0) {
            
            res <- referentialIntegrityCheck(x = childTable, y = parentTable, 
                                                errorLog, logDepth + 2, child, parent)
          } 
          
        } else {
          
          msg <- paste0('The parent table ', parent, 
                        ' defined for reference checks of ',
                        child, ' table does not exist.')
          
          writeToLog(message = msg, type = 'Error', fileConxn = errorLog, 
                     printToConsole = T, depth = depthPlusOne)
          
        }
        
      }
      
    } else {
      
      msg <- paste0('The child table ', child, 
                    ' defined for reference checks does not exist.')
      
      writeToLog(message = msg, type = 'Error', fileConxn = errorLog, 
                 printToConsole = T, depth = depthPlusOne)
      
    }
    
    writeToLog('Completed.', fileConxn = errorLog, printToConsole = T, 
               depth = logDepth + 2)
  }

} else {
  
  writeToLog(message = 'No tables defined in reference check definitions.', 
             type = 'Error', fileConxn = errorLog, printToConsole = T, 
             depth = logDepth)
  
}

writeToLog('Completed.', fileConxn = errorLog, printToConsole = T, 
           depth = logDepth)