
getKeyFieldsFromTable <- function(tableName, onlyPK = F, onlyFK = F){
  
  tableDef <- DataDefinition[DataDefinition$TableName == tableName,]
  
  return(getKeyFields(fieldNames = tableDef$FieldName, 
                      fieldTypes = tableDef$FieldType, 
                      onlyPK = onlyPK, onlyFK = onlyFK))
  
}