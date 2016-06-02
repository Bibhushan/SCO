# Just a simple function which returns the key fields from a list of fields 

getKeyFields <- function(fieldNames, fieldTypes, onlyPK = F, onlyFK = F){
  
  # Try to parse the field type based on the first two characters of the fieldType
  
  fTypes <- tolower(substr(fieldTypes, 1, 2))
    
  PKs <- fieldNames[fTypes %in% c('pk', 'pr')]
    
  if (onlyPK) return(PKs) 
  
  FKs <- fieldNames[fTypes %in% c('fk', 'fo', 'fr')]
  
  if (onlyFK) return(FKs)
  
  return(c(PKs, FKs))
  
}