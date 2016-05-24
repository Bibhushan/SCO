# Just a simple function which returns the key fields from a list of fields 

getKeyFields <- function(fieldNames, fieldTypes){
  
  return(fieldNames[fieldTypes %in% c('PK', 'FK')])
  
}