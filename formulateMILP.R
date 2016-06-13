
variableIndex <- 1

variableTypeCount <- nrow(ModelVariables)

costCoeff <- NULL
lowerBound <- NULL
upperBound <- NULL
variableNames <- NULL

Period$Sequence <- order(Period$EndingDate)

for (varTypeIndex in seq(1, variableTypeCount)){
  
  startIndex <- variableIndex
  
  tbl <- get(ModelVariables$TableName[varTypeIndex])
  
  varCount <- nrow(tbl)
  
  endIndex <- startIndex + varCount - 1
  
  variableIndex <- startIndex + varCount
  
  assign(paste0(ModelVariables$VariableName[varTypeIndex], '_Increment'), 
         startIndex - 1)
  
  assign(paste0(ModelVariables$VariableName[varTypeIndex], '_StartIndex'), 
         startIndex)
  
  assign(paste0(ModelVariables$VariableName[varTypeIndex], '_EndIndex'), 
         endIndex)

  # assign cost coefficient
  
  costCoeff <- c(costCoeff, (ModelVariables$CostMultiplier[varTypeIndex]*
                 getSanitisedValues(fieldName = ModelVariables$CostField[varTypeIndex], 
                                    data = tbl, defaultValue = 0, 
                                    tableName = ModelVariables$TableName[varTypeIndex], 
                                    logFile = errorLog, logDepth = depthPlusOne)))
  
  lowerBound <- c(lowerBound, 
                 getSanitisedValues(fieldName = ModelVariables$LowerBound[varTypeIndex], 
                                    data = tbl, defaultValue = 0, 
                                    tableName = ModelVariables$TableName[varTypeIndex], 
                                    logFile = errorLog, logDepth = depthPlusOne))
  
  upperBound <- c(upperBound, 
                 getSanitisedValues(fieldName = ModelVariables$UpperBound[varTypeIndex], 
                                    data = tbl, defaultValue = Inf, 
                                    tableName = ModelVariables$TableName[varTypeIndex], 
                                    logFile = errorLog, logDepth = depthPlusOne))
  
  variableNames <- c(variableNames, paste(ModelVariables$VariableName[varTypeIndex], rownames(tbl), sep = '_'))
  
}

thisModel <- make.lp(nrow = 0, ncol = length(variableNames))

lp.control(thisModel,sense='max')

colnames(thisModel) <- variableNames

set.bounds(lprec = thisModel, lower = lowerBound, upper = upperBound)

set.objfn(thisModel, costCoeff)

# identify service link flow constraints

# get the demand rows and find corresponding service links

demandCount <- nrow(Demand)

for (dem in seq(1, demandCount)){
  
  flowRows <- Link_Flow_Increment + 
                    getParentRows(childRow = Demand[dem,], childName = 'Demand', 
                                  parentName = 'TransportationLink', 
                                  RefCheckDefinition = RefCheckDefinition, 
                                  logFile = errorLog, logDepth = depthPlusOne)
  flowRowCount <- length(flowRows) 
  
  if (flowRowCount > 0) {
    
    coeffs <- c(-1, rep(1, flowRowCount))
    
    varIndices <- c(DemandMet_Increment + dem, flowRows)
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
  } else {
   
    writeToLog(message = paste0('No transportation links defined for demand ', dem, '.'), 
               type = 'Error', fileConxn = errorLog, printToConsole = F, depth = logDepth) 
    
  }
  
}

sptCount <- nrow(ProductAtFacilityInPeriod)
sptRange <- seq(1, sptCount)


# set Starting Inventory of first period to opening inventory

ProductAtFacilityInPeriod <- merge(x = ProductAtFacilityInPeriod, 
                                   y = Period[, c('Period', 'Sequence')], 
                                  all.x = T, all.y = T)

tempRows <- which(ProductAtFacilityInPeriod$Sequence == 1)

for(spt in tempRows){
  
  value <- getParentValue(childRow = ProductAtFacilityInPeriod[spt, ], 
                          childName = 'ProductAtFacilityInPeriod', 
                          parentName = 'ProductAtFacility', 
                          fieldName = 'OpeningInventory', defaultValue = 0, 
                          RefCheckDefinition = RefCheckDefinition,
                          logFile = errorLog, logDepth = depthPlusOne)
  
  set.bounds(thisModel, lower= value, upper = value, 
             columns = StartInv_SPT_Increment + spt)
  
}

# Ending inventory of period 1 is equal to starting inventory of period 2

tempRows <- which(ProductAtFacilityInPeriod$Sequence != 1)

for(spt in tempRows){
  
  endInvRow <- which((ProductAtFacilityInPeriod$Sequence == 
                       (ProductAtFacilityInPeriod$Sequence[spt] - 1)) & 
                       (ProductAtFacilityInPeriod$Product == ProductAtFacilityInPeriod$Product[spt] &
                       ProductAtFacilityInPeriod$SiteName == ProductAtFacilityInPeriod$SiteName[spt]))
  
  if(length(endInvRow) > 0){
    
    varIndices <- c(StartInv_SPT_Increment + spt,
                    EndInv_SPT_Increment + endInvRow)
    
    add.constraint(lprec = thisModel, xt = c(-1, 1), type = '=', rhs = 0, 
                   indices = varIndices)
    
  } else {
    
    writeToLog(message = 'Starting inventory record not found in _
               ProductAtFacilityInPeriod table for one of the periods.', 
               type = 'Error', fileConxn = errorLog, printToConsole = T, depth = logDepth)
    
  }
  
}

# add shipping constraints

for (spt in sptRange){
  
  shippingRows <- Link_Flow_Increment  + 
                    getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
                                  childName = 'ProductAtFacilityInPeriod', 
                                  parentName = 'TransportationLink', 
                                  RefCheckDefinition = RefCheckDefinition, 
                                  logFile = errorLog, logDepth = depthPlusOne)

  shippingRowCount <- length(shippingRows)
  
  if (shippingRowCount > 0){
    
    coeffs <- c(-1, rep(1, shippingRowCount))
    
    varIndices <- c(Shipping_SPT_Increment + spt, shippingRows)
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  }
    
}

# add receiving constraints

for (spt in sptRange){
  
  receivingRows <- Link_Flow_Increment + 
                    getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
                      childName = 'ProductAtFacilityInPeriod', 
                      parentName = 'TransportationLink', set = 2,
                      RefCheckDefinition = RefCheckDefinition, 
                      logFile = errorLog, logDepth = depthPlusOne)
      
  receivingRowCount <- length(receivingRows)
  
  if (receivingRowCount > 0){
    
    coeffs <- c(-1, rep(1, receivingRowCount))
    
    varIndices <- c(Receiving_SPT_Increment + spt, receivingRows)
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  } else {
    
    set.bounds(thisModel, upper = 0, columns = Receiving_SPT_Increment + spt)
    
  }
  
}

# group BOMAtFacilityInPeriod by Product, Facility and Period

tempSPT <- ProductAtFacilityInPeriod[, getKeyFieldsFromTable('ProductAtFacilityInPeriod')]

tempSPT$RowID <- seq(1, nrow(tempSPT))

prodxnSPTRows <- NULL
consumptionSPTRows <- NULL

BOMOutRows <- which(BOMAtFacilityInPeriod$Type == 'Output')

if (length(BOMOutRows) > 0){
  
  tempBOM <- aggregate(formula = Type~SiteName+Product+Period, 
                       data = BOMAtFacilityInPeriod[BOMOutRows,], FUN = length)
  
  temp <- merge(tempSPT, tempBOM, all.x = T)
  
  prodxnSPTRows <- temp$RowID[!is.na(temp$Type)]
  
}

BOMInRows <- which(BOMAtFacilityInPeriod$Type == 'Input')

if (length(BOMInRows) > 0) {
  
  tempBOM <- aggregate(formula = Type~SiteName+Product+Period, 
                       data = BOMAtFacilityInPeriod[BOMInRows, ], FUN = length)
  
  temp <- merge(tempSPT, tempBOM, all.x = T)
  
  consumptionSPTRows <- temp$RowID[!is.na(temp$Type)]
  
}

# Balance of Flow Constraints

for (spt in sptRange){
  
    coeffs <- c(rep(-1, 2), rep(1, 2))
    
    varIndices <- c(Shipping_SPT_Increment, EndInv_SPT_Increment, 
                    Receiving_SPT_Increment, StartInv_SPT_Increment) + spt
    
    # add production to balance if BOM exists
    
#     tempRows <- which(BOMAtFacilityInPeriod$Period == ProductAtFacilityInPeriod$Period[spt] & 
#                         BOMAtFacilityInPeriod$Product == ProductAtFacilityInPeriod$Product[spt] &
#                         BOMAtFacilityInPeriod$SiteName == ProductAtFacilityInPeriod$SiteName[spt] & 
#                         BOMAtFacilityInPeriod$Type == 'Output')
#     
#     if (length(tempRows)>0){
      
    if (spt %in% prodxnSPTRows){
      
      coeffs <- c(coeffs, 1)
      
      varIndices <- c(varIndices, Prodxn_SPT_Increment + spt)
      
    }
    
    # add consumption to balance of flow if consumption is defined for spt
    
#     tempRows <- which(BOMAtFacilityInPeriod$Period == ProductAtFacilityInPeriod$Period[spt] & 
#                         BOMAtFacilityInPeriod$Product == ProductAtFacilityInPeriod$Product[spt] &
#                         BOMAtFacilityInPeriod$SiteName == ProductAtFacilityInPeriod$SiteName[spt] & 
#                         BOMAtFacilityInPeriod$Type != 'Output')
#     
#     if (length(tempRows)>0){
    
    if (spt %in% consumptionSPTRows){  
            
      coeffs <- c(coeffs, -1)
      
      varIndices <- c(varIndices, Consumption_SPT_Increment + spt)
      
    }
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
  
}

# Handling equation

for (spt in sptRange){
  
  coeffs <- c(-1, rep(1, 4))
  
  varIndices <- c(Handling_SPT_Increment, 
                  Shipping_SPT_Increment, Consumption_SPT_Increment,
                  Receiving_SPT_Increment, Prodxn_SPT_Increment) + spt
  
  add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                 indices = varIndices)
  
}

# BOM Constraints 

# First add the production to BOM Usage constraints

for (spt in prodxnSPTRows){
  
  tempRows <- getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
                                 childName = 'ProductAtFacilityInPeriod', 
                                 parentName = 'BOMAtFacilityInPeriod', 
                                 RefCheckDefinition = RefCheckDefinition, 
                                 logFile = errorLog, logDepth = depthPlusOne)
  
  prodxnBOMRows <- intersect(tempRows, BOMOutRows)

  prodxnBOMRowCount <- length(prodxnBOMRows)
  
  if (prodxnBOMRowCount > 0) {
    
    coeffs <- c(-1, rep(1, prodxnBOMRowCount))
    
    varIndices <- c(Prodxn_SPT_Increment + spt, prodxnBOMRows + BOMUsage_SBPT_Increment) 
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
  }
  
}

# Now add the consumption to BOM Usage constraints

for (spt in consumptionSPTRows){
  
  tempRows <- getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
                                 childName = 'ProductAtFacilityInPeriod', 
                                 parentName = 'BOMAtFacilityInPeriod', 
                                 RefCheckDefinition = RefCheckDefinition, 
                                 logFile = errorLog, logDepth = depthPlusOne)
  
  consumptionBOMRows <- intersect(tempRows, BOMInRows)
  
  consumptionBOMRowCount <- length(consumptionBOMRows)
  
  if (consumptionBOMRowCount > 0) {
    
    coeffs <- c(-1, rep(1, consumptionBOMRowCount))
    
    varIndices <- c(Consumption_SPT_Increment + spt, consumptionBOMRows + BOMUsage_SBPT_Increment) 
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  }
  
}

# BOM Cycle constraints

sbptRange <- seq(1, nrow(BOMAtFacilityInPeriod))

for (sbpt in sbptRange){
  
  procRows <- getParentRows(childRow = BOMAtFacilityInPeriod[sbpt,], 
                            childName = 'BOMAtFacilityInPeriod', 
                            parentName = 'ProcessAtFacilityInPeriod', 
                            RefCheckDefinition = RefCheckDefinition, 
                            logFile = errorLog, logDepth = depthPlusOne)
  
  procRowCount <- length(procRows)
  
  if (procRowCount == 1){
    
    coeffs <- c(-1, BOMAtFacilityInPeriod$Quantity[sbpt])
    
    varIndices <- c(BOMUsage_SBPT_Increment + sbpt, Cycles_SBT_Increment + procRows) 
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  }
  
}

write.lp(thisModel, 'thisModel.txt', type = 'lp')

res <- solve(thisModel)

if (!res %in% c(0,9)){
  
  
  
} else {
  
  msg <- paste0(' Objective value is ', get.objective(thisModel))
  
  if (res == 0){
    
    writeToLog(message = paste0('Model solved to optimality.',msg), type = 'Message', 
               fileConxn = errorLog, printToConsole = T, depth = logDepth)
    
  } else {
   
    writeToLog(message = paste0('Model solved by presolve.',msg), type = 'Message', 
               fileConxn = errorLog, printToConsole = T, depth = logDepth)
    
    
  }
  
  optValues <- get.variables(thisModel)
  
  for (varTypeIndex in seq(1, variableTypeCount)){
   
    tbl <- get(ModelVariables$TableName[varTypeIndex]) 
    
    startIndex <- get(paste0(ModelVariables$VariableName[varTypeIndex], '_StartIndex'))
    
    endIndex <- get(paste0(ModelVariables$VariableName[varTypeIndex], '_EndIndex'))
    
    tbl[, ModelVariables$FieldName[varTypeIndex]] <- optValues[startIndex:endIndex]
    
    assign(ModelVariables$TableName[varTypeIndex], tbl)
    
    fileName <- paste0('Outputs/', ModelVariables$TableName[varTypeIndex], '.csv')
    
    write.csv(x = tbl, file = fileName, row.names = F)
    
  }
  
}
