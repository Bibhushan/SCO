
dataConversionWarningHandler <- function(fieldName, tableName, dataType, 
                                         convertedType, logFile, logDepth = 0){
  
  
  msg <- paste0('Data type conversion from ', dataType, ' to ', convertedType,  
                ' of ', fieldName, ' field in ', tableName, 
                ' table introduced warnings. Some values have been converted to NAs.')
  
  writeToLog(message = msg, type = 'Warning', fileConxn = logFile, depth = logDepth)
  
}