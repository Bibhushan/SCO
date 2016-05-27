
# message: message to be printed or logged.
# type: indicates the type of message ('Error', 'Warning', or 'Message')
# fileConxn: file connection, where the log output is written
# printToConsole: used to additionally print the output to console.
# depth: depth of the log

# Edit: fileConxn was not working. Using file name instead
# Edit: added the parameter to add the timestamp

writeToLog <- function(message, type = '', fileConxn, 
                       printToConsole = T, depth=0, addTimeStamp = F,
                       addNewLine = T){
    
    prefix <- ''
    
    if (addNewLine) prefix <- '\n'
  
    # add next line and the number of spaces specified by depth
    if (addNewLine) prefix <- paste0(prefix, paste(rep(' ', depth), collapse = ' '))
    
    # add time stamp if needed
    # we add time stamp only when the message is printed to new line.
    if (addTimeStamp && addNewLine) prefix <- paste0(prefix, Sys.time(), '| ')
  
    # append type of message if needed
    if (type != '') prefix <- paste0(prefix, type, ': ')
    
    msg <- paste0(prefix, message)

    if (printToConsole) cat(msg)
  
    cat(msg, file = fileConxn, append = T)
  
}