
# this function checks whether foreign key fields are defined in respective tables.

# process is to left join the two tables (x and y) and find unmapped records.

referentialIntegrityCheck <- function(x, y, logFile, logDepth = 0, 
                                      childTable = 'Child', parentTable = 'Parent',
                                      maxErrorRows = 100){
  
  # initialize the result variable
  # we will return the error and error data rows if applicable
  
  result <- list(Errors = 0, Data = NULL)
  
  # first extract the unique values from y itself 
  
  x <- unique(x)
  
  y <- unique(y)
  
  childFields <- names(x)
  
  # we add an extra column to identify if the values present in y
  
  x$match <- 1
  
  # using all.x in merge will populate all records in x. The records which
  # are not available in y will have the match value null
  
  temp <- merge(x, y, all.x = T, by.x = childFields, by.y = names(y))
  
  invalidData <- temp[is.na(temp$match), ]
  
  invalidRowCount <- nrow(invalidData)
  
  if (invalidRowCount > 0){
    
    msg <- paste0(invalidRowCount, ' rows in ', childTable, 
                  ' table does not have corresponding records in its referenced parent table ', 
                  parentTable, '.')
    
    writeToLog(message = msg, type = 'Error', fileConxn = logFile, 
               printToConsole = T, depth = logDepth)
    
    result$Errors <- invalidRowCount
  
    if(invalidRowCount > maxErrorRows) {
      
      writeToLog(paste0('Printing only top ', maxErrorRows, ' error rows.'),
                 type = '', logFile, depth = logDepth) 
      
      invalidData <- invalidData[1:maxErrorRows, ]
      
    }
    
    result$Data <- invalidData
    
    cat('\n', file = logFile, append = T)
    
    # disable warnings because R throws a warning when appending column names to data
    
    options(warn = -1)
    
    write.table(invalidData, file = logFile, append = T)
    
    options(warn = 0)
    
  }
  
  return (result)
  
}