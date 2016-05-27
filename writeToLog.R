
# message: message to be printed or logged.
# type: indicates the type of message ('Error', 'Warning', or 'Message')
# fileConxn: file connection, where the log output is written
# printToConsole: used to additionally print the output to console.
# depth: depth of the log

# Edit: fileConxn was not working. Using file name instead
# Edit: added the parameter to add the timestamp

writeToLog <- function(message, type = 'Message', fileConxn, 
                       printToConsole = T, depth=0, addTimeStamp = F){
  
    prefix <- '\n'
    
    if (addTimeStamp) prefix <- paste0(prefix, Sys.time(), ': ')
  
    msg <- paste0(prefix, paste(rep(' ', depth), collapse = ' '), type, ': ', message)
  
  if (printToConsole) cat(msg)
  
  #writeLines(msg, fileConxn)
  
  cat(msg, file = fileConxn, append = T)
  
}