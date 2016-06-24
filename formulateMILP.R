
writeToLog('Started building optimisation model...', fileConxn = errorLog, 
           depth = logDepth, addTimeStamp = T)

variableIndex <- 0

variableTypeCount <- nrow(ModelVariables)

costCoeff <- NULL
lowerBound <- NULL
upperBound <- NULL
variableNames <- NULL

writeToLog('Initialised variables.', fileConxn = errorLog, depth = depthPlusOne)

Period$Sequence <- order(Period$EndingDate)

writeToLog('Calculating Variable Indices...', fileConxn = errorLog, 
           depth = depthPlusOne)

for (varTypeIndex in seq(1, variableTypeCount)){
  
  startIndex <- variableIndex
  
  tbl <- get(ModelVariables$TableName[varTypeIndex])
  
  varCount <- nrow(tbl)
  
  endIndex <- startIndex + varCount - 1
  
  tbl[, paste0(ModelVariables$VariableName[varTypeIndex], '_Index')] <- variableIndex + seq(1, nrow(tbl))  
  
  assign(ModelVariables$TableName[varTypeIndex], tbl)
    
  assign(paste0(ModelVariables$VariableName[varTypeIndex], '_Increment'), 
         variableIndex)
  
  assign(paste0(ModelVariables$VariableName[varTypeIndex], '_StartIndex'), 
         startIndex)
  
  assign(paste0(ModelVariables$VariableName[varTypeIndex], '_EndIndex'), 
         endIndex)

  variableIndex <- startIndex + varCount

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

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addNewLine = F)

thisModel <- make.lp(nrow = 0, ncol = length(variableNames))

lp.control(thisModel,sense='max')

writeToLog('Assigning variable names...',fileConxn = errorLog, depth = depthPlusOne)

colnames(thisModel) <- variableNames

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addNewLine = F)

writeToLog('Setting variable bounds and defining objective function...', 
           fileConxn = errorLog, depth = depthPlusOne)

set.bounds(lprec = thisModel, lower = lowerBound, upper = upperBound)

set.objfn(thisModel, costCoeff)

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addNewLine = F)

# identify service link flow constraints

# get the demand rows and find corresponding service links

writeToLog('Adding demand constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

temp <- merge(Demand, TransportationLink, by.x = c('Customer', 'Product', 'Period'), 
              by.y = c('Destination', 'Product', 'Period'))

#demandCount <- nrow(Demand)

demRange <- unique(temp$DemandMet_Index)

prb <- txtProgressBar(max = length(demRange), style = 3)
prbValue <- 0

for (dem in demRange){
    
    tempRows <- which(temp$DemandMet_Index == dem)  
  
    coeffs <- c(-1, rep(1, length(tempRows)))
    
    varIndices <- c(dem, temp$Link_Flow_Index[tempRows])
    
#   flowRows <- Link_Flow_Increment + 
#                     getParentRows(childRow = Demand[dem,], childName = 'Demand', 
#                                   parentName = 'TransportationLink', 
#                                   RefCheckDefinition = RefCheckDefinition, 
#                                   logFile = errorLog, logDepth = depthPlusOne)
#   flowRowCount <- length(flowRows) 
#   
#   if (flowRowCount > 0) {
#     
#     coeffs <- c(-1, rep(1, flowRowCount))
#     
#     varIndices <- c(DemandMet_Increment + dem, flowRows)
#     
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
#     
#   } else {
#    
#     writeToLog(message = paste0('No transportation links defined for demand ', dem, '.'), 
#                type = 'Error', fileConxn = errorLog, printToConsole = F, depth = logDepth) 
#     
#   }
  
  prbValue <- prbValue + 1
  setTxtProgressBar(prb, prbValue)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)


sptCount <- nrow(ProductAtFacilityInPeriod)
sptRange <- seq(1, sptCount)

writeToLog('Adding Inventory Constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# set Starting Inventory of first period to opening inventory

tempSPT <- merge(x = ProductAtFacilityInPeriod, 
                                   y = Period[, c('Period', 'Sequence')], 
                                  all.x = T, all.y = T)

tempSPT$prevSequence <- tempSPT$Sequence - 1

temp <- merge(x = tempSPT, y = tempSPT[, c('SiteName', 'Product', 'Sequence', 'EndInv_SPT_Index')],
              by.x = c('SiteName', 'Product', 'prevSequence'), by.y = c('SiteName', 'Product', 'Sequence'), all.x = T)

prb <- txtProgressBar(max = sptCount, style = 3)

for (spt in sptRange){

# tempRows <- which(ProductAtFacilityInPeriod$Sequence == 1)
# 
# prb <- txtProgressBar(max = length(tempRows), style = 3)
# 
# prbValue <- 0
# 
# for(spt in tempRows){

  if (temp$Sequence[spt] == 1){
    
    value <- getParentValue(childRow = temp[spt, ], 
                            childName = 'ProductAtFacilityInPeriod', 
                            parentName = 'ProductAtFacility', 
                            fieldName = 'OpeningInventory', defaultValue = 0, 
                            RefCheckDefinition = RefCheckDefinition,
                            logFile = errorLog, logDepth = depthPlusOne)
    
#     set.bounds(thisModel, lower= value, upper = value, 
#                columns = temp$EndInv_SPT_Index[spt])
    
    add.constraint(lprec = thisModel, xt = 1, type = '=', rhs = value, 
                   indices = temp$StartInv_SPT_Index[spt])
    
    
  } else {
    
#     endInvRow <- which((temp$Sequence == 
#                           (temp$Sequence[spt] - 1)) & 
#                          (temp$Product == temp$Product[spt] &
#                             temp$SiteName == temp$SiteName[spt]))
#     
#     if(length(endInvRow) > 0){
      
      varIndices <- c(temp$StartInv_SPT_Index[spt],
                      temp$EndInv_SPT_Index.y[spt])
      
      add.constraint(lprec = thisModel, xt = c(-1, 1), type = '=', rhs = 0, 
                     indices = varIndices)
      
#     } else {
#       
#       writeToLog(message = 'Starting inventory record not found in ProductAtFacilityInPeriod table for one of the periods.', 
#                  type = 'Error', fileConxn = errorLog, printToConsole = T, depth = logDepth)
#       
#     }
    
    
  }
  
  setTxtProgressBar(prb, spt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# Ending inventory of period 1 is equal to starting inventory of period 2

# writeToLog('Adding ending inventory constraints...', fileConxn = errorLog, 
#            depth = depthPlusOne, addTimeStamp = T)
# 
# tempRows <- which(ProductAtFacilityInPeriod$Sequence != 1)
# 
# prb <- txtProgressBar(max = length(tempRows), style = 3)
# 
# prbValue <- 0
# 
# for(spt in tempRows){
#   
#   endInvRow <- which((ProductAtFacilityInPeriod$Sequence == 
#                        (ProductAtFacilityInPeriod$Sequence[spt] - 1)) & 
#                        (ProductAtFacilityInPeriod$Product == ProductAtFacilityInPeriod$Product[spt] &
#                        ProductAtFacilityInPeriod$SiteName == ProductAtFacilityInPeriod$SiteName[spt]))
#   
#   if(length(endInvRow) > 0){
#     
#     varIndices <- c(StartInv_SPT_Increment + spt,
#                     EndInv_SPT_Increment + endInvRow)
#     
#     add.constraint(lprec = thisModel, xt = c(-1, 1), type = '=', rhs = 0, 
#                    indices = varIndices)
#     
#   } else {
#     
#     writeToLog(message = 'Starting inventory record not found in _
#                ProductAtFacilityInPeriod table for one of the periods.', 
#                type = 'Error', fileConxn = errorLog, printToConsole = T, depth = logDepth)
#     
#   }
#   
#   prbValue <- prbValue + 1
#   
#   setTxtProgressBar(prb, prbValue)
#   
# }

# add shipping constraints

writeToLog('Adding Shipping constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

temp <- merge(ProductAtFacilityInPeriod, TransportationLink, 
              by.x = c('SiteName', 'Product', 'Period'), 
              by.y = c('Origin', 'Product', 'Period'))

prb <- txtProgressBar(max = sptCount, style = 3) 

for (spt in sptRange){

  tempRows <- which(temp$Shipping_SPT_Index == ProductAtFacilityInPeriod$Shipping_SPT_Index[spt])
  
#   shippingRows <- TransportationLink$Link_Flow_Index[getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
#                                   childName = 'ProductAtFacilityInPeriod', 
#                                   parentName = 'TransportationLink', 
#                                   RefCheckDefinition = RefCheckDefinition, 
#                                   logFile = errorLog, logDepth = depthPlusOne)]

  tempRowCount <- length(tempRows)
  
  if (tempRowCount > 0){
  
    coeffs <- c(-1, rep(1, tempRowCount))
    
    varIndices <- c(ProductAtFacilityInPeriod$Shipping_SPT_Index[spt], temp$Link_Flow_Index[tempRows])
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  } else {
    
    add.constraint(lprec = thisModel, xt = 1, type = '=', rhs = 0, 
                   indices = ProductAtFacilityInPeriod$Shipping_SPT_Index[spt])
    
  }
  
  setTxtProgressBar(prb, spt)
    
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# add receiving constraints

writeToLog('Adding Receiving constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

temp <- merge(ProductAtFacilityInPeriod, TransportationLink, 
              by.x = c('SiteName', 'Product', 'Period'), 
              by.y = c('Destination', 'Product', 'Period'))

prb <- txtProgressBar(max = sptCount, style = 3) 


for (spt in sptRange){
  
  tempRows <- which(temp$Receiving_SPT_Index == ProductAtFacilityInPeriod$Receiving_SPT_Index[spt])
  
  tempRowCount <- length(tempRows)
  
#   receivingRows <- Link_Flow_Increment + 
#                     getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
#                       childName = 'ProductAtFacilityInPeriod', 
#                       parentName = 'TransportationLink', set = 2,
#                       RefCheckDefinition = RefCheckDefinition, 
#                       logFile = errorLog, logDepth = depthPlusOne)
#       
#   receivingRowCount <- length(receivingRows)
  
# if (receivingRowCount > 0){
  
  if(tempRowCount > 0){
    
    coeffs <- c(-1, rep(1, tempRowCount))
    
    varIndices <- c(ProductAtFacilityInPeriod$Receiving_SPT_Index[spt], temp$Link_Flow_Index[tempRows])
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  } else {
    
    add.constraint(lprec = thisModel, xt = 1, type = '=', rhs = 0, 
                   indices = ProductAtFacilityInPeriod$Shipping_SPT_Index[spt])
    
    
  }
  
  setTxtProgressBar(prb, spt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# group BOMAtFacilityInPeriod by Product, Facility and Period
# 
# 
# tempSPT <- ProductAtFacilityInPeriod[, getKeyFieldsFromTable('ProductAtFacilityInPeriod')]
# 
# tempSPT$RowID <- seq(1, nrow(tempSPT))
# 
# prodxnSPTRows <- NULL
# consumptionSPTRows <- NULL
# 
# BOMOutRows <- which(BOMAtFacilityInPeriod$Type == 'Output')
# 
# if (length(BOMOutRows) > 0){
#   
#   tempBOM <- aggregate(formula = Type~SiteName+Product+Period, 
#                        data = BOMAtFacilityInPeriod[BOMOutRows,], FUN = length)
#   
#   temp <- merge(tempSPT, tempBOM, all.x = T)
#   
#   prodxnSPTRows <- temp$RowID[!is.na(temp$Type)]
#   
# }
# 
# BOMInRows <- which(BOMAtFacilityInPeriod$Type == 'Input')
# 
# if (length(BOMInRows) > 0) {
#   
#   tempBOM <- aggregate(formula = Type~SiteName+Product+Period, 
#                        data = BOMAtFacilityInPeriod[BOMInRows, ], FUN = length)
#   
#   temp <- merge(tempSPT, tempBOM, all.x = T)
#   
#   consumptionSPTRows <- temp$RowID[!is.na(temp$Type)]
#   
# }

# Balance of Flow Constraints

writeToLog('Adding Flow Balance constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

prb <- txtProgressBar(max = sptCount, style = 3) 

for (spt in sptRange){
  
    coeffs <- c(rep(-1, 3), rep(1, 3))
    
    varIndices <- c(ProductAtFacilityInPeriod$Shipping_SPT_Index[spt],
                    ProductAtFacilityInPeriod$EndInv_SPT_Index[spt],
                    ProductAtFacilityInPeriod$Consumption_SPT_Index[spt],
                    ProductAtFacilityInPeriod$Receiving_SPT_Index[spt],
                    ProductAtFacilityInPeriod$StartInv_SPT_Index[spt],
                    ProductAtFacilityInPeriod$Prodxn_SPT_Index[spt])
    
#     varIndices <- c(Shipping_SPT_Increment, EndInv_SPT_Increment, 
#                     Receiving_SPT_Increment, StartInv_SPT_Increment) + spt
    
    # add production to balance if BOM exists
    
#     tempRows <- which(BOMAtFacilityInPeriod$Period == ProductAtFacilityInPeriod$Period[spt] & 
#                         BOMAtFacilityInPeriod$Product == ProductAtFacilityInPeriod$Product[spt] &
#                         BOMAtFacilityInPeriod$SiteName == ProductAtFacilityInPeriod$SiteName[spt] & 
#                         BOMAtFacilityInPeriod$Type == 'Output')
#     
#     if (length(tempRows)>0){
      
#     if (spt %in% prodxnSPTRows){
#       
#       coeffs <- c(coeffs, 1)
#       
#       varIndices <- c(varIndices, Prodxn_SPT_Increment + spt)
#       
#     }
#     
    # add consumption to balance of flow if consumption is defined for spt
    
#     tempRows <- which(BOMAtFacilityInPeriod$Period == ProductAtFacilityInPeriod$Period[spt] & 
#                         BOMAtFacilityInPeriod$Product == ProductAtFacilityInPeriod$Product[spt] &
#                         BOMAtFacilityInPeriod$SiteName == ProductAtFacilityInPeriod$SiteName[spt] & 
#                         BOMAtFacilityInPeriod$Type != 'Output')
#     
#     if (length(tempRows)>0){
#     
#     if (spt %in% consumptionSPTRows){  
#             
#       coeffs <- c(coeffs, -1)
#       
#       varIndices <- c(varIndices, Consumption_SPT_Increment + spt)
#       
#     }
#     
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    setTxtProgressBar(prb, spt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# Handling equation

writeToLog('Adding Handling equations...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

prb <- txtProgressBar(max = sptCount, style = 3) 

for (spt in sptRange){
  
  coeffs <- c(-1, rep(1, 4))

  varIndices <- c(ProductAtFacilityInPeriod$Handling_SPT_Index[spt],
                  ProductAtFacilityInPeriod$Shipping_SPT_Index[spt],
                  ProductAtFacilityInPeriod$Consumption_SPT_Index[spt],
                  ProductAtFacilityInPeriod$Receiving_SPT_Index[spt],
                  ProductAtFacilityInPeriod$Prodxn_SPT_Index[spt])
    
#   varIndices <- c(Handling_SPT_Increment, 
#                   Shipping_SPT_Increment, Consumption_SPT_Increment,
#                   Receiving_SPT_Increment, Prodxn_SPT_Increment) + spt
  
  add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                 indices = varIndices)
  
  setTxtProgressBar(prb, spt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# BOM Constraints 

writeToLog('Preparing data for BOM constraints...', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

SPTCols <- getKeyFieldsFromTable('ProductAtFacilityInPeriod')

tempBOM <- unique(BOMAtFacilityInPeriod[BOMAtFacilityInPeriod$Type == 'Output', SPTCols])

tempBOM$match <- 1

temp <- merge(ProductAtFacilityInPeriod, tempBOM, all.x = T)

prodxnSPTRows <- which(!is.na(temp$match))

tempBOM <- unique(BOMAtFacilityInPeriod[BOMAtFacilityInPeriod$Type != 'Output', SPTCols])

tempBOM$match <- 1

temp <- merge(ProductAtFacilityInPeriod, tempBOM, all.x = T)

consumptionSPTRows <- which(!is.na(temp$match))

writeToLog('Completed.', fileConxn = errorLog, addNewLine = F)

# First add the production to BOM Usage constraints

writeToLog('Adding Production Constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

temp <- merge(BOMAtFacilityInPeriod[BOMAtFacilityInPeriod$Type == 'Output', ], ProductAtFacilityInPeriod)

prb <- txtProgressBar(max = sptCount, style = 3) 

for (spt in sptRange){

  tempRows <- which(temp$Prodxn_SPT_Index == ProductAtFacilityInPeriod$Prodxn_SPT_Index[spt])
    
#   tempRows <- getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
#                                  childName = 'ProductAtFacilityInPeriod', 
#                                  parentName = 'BOMAtFacizxlityInPeriod', 
#                                  RefCheckDefinition = RefCheckDefinition, 
#                                  logFile = errorLog, logDepth = depthPlusOne)
  
#   prodxnBOMRows <- intersect(tempRows, BOMOutRows)
# 
#   prodxnBOMRowCount <- length(prodxnBOMRows)
  
# if (prodxnBOMRowCount > 0) {

  tempRowCount <- length(tempRows)
  
  if (tempRowCount > 0){
      
    coeffs <- c(-1, rep(1, tempRowCount))
    
    varIndices <- c(ProductAtFacilityInPeriod$Prodxn_SPT_Index[spt], 
                    temp$BOMUsage_SBPT_Index[tempRows]) 
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
  } else {
    
    add.constraint(lprec = thisModel, xt = 1, type = '=', rhs = 0, 
                   indices = ProductAtFacilityInPeriod$Prodxn_SPT_Index[spt])
    
  }
  
  setTxtProgressBar(prb, spt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# Now add the consumption to BOM Usage constraints

writeToLog('Adding Consumption Constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

temp <- merge(BOMAtFacilityInPeriod[BOMAtFacilityInPeriod$Type != 'Output', ], ProductAtFacilityInPeriod)

prb <- txtProgressBar(max = sptCount, style = 3) 

for (spt in sptRange){
  
#   tempRows <- getParentRows(childRow = ProductAtFacilityInPeriod[spt,], 
#                                  childName = 'ProductAtFacilityInPeriod', 
#                                  parentName = 'BOMAtFacilityInPeriod', 
#                                  RefCheckDefinition = RefCheckDefinition, 
#                                  logFile = errorLog, logDepth = depthPlusOne)
#   
#   consumptionBOMRows <- intersect(tempRows, BOMInRows)
#   
#   consumptionBOMRowCount <- length(consumptionBOMRows)
#   
#   if (consumptionBOMRowCount > 0) {
# 
  
  tempRows <- which(temp$Consumption_SPT_Index == ProductAtFacilityInPeriod$Consumption_SPT_Index[spt])
  
  tempRowCount <- length(tempRows)
  
  if(tempRowCount > 0 ){
  
    coeffs <- c(-1, rep(1, tempRowCount))
    
    varIndices <- c(ProductAtFacilityInPeriod$Consumption_SPT_Index[spt], 
                    temp$BOMUsage_SBPT_Index[tempRows]) 
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
  
  } else {
    
    add.constraint(lprec = thisModel, xt = 1, type = '=', rhs = 0, 
                   indices = ProductAtFacilityInPeriod$Consumption_SPT_Index[spt])
    
  }
  
  setTxtProgressBar(prb, spt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

# BOM Cycle constraints

writeToLog('Adding BOM Cycle Constraints...\n', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

temp <- merge(BOMAtFacilityInPeriod, ProcessAtFacilityInPeriod, all.x = T)

sbptCount <- nrow(BOMAtFacilityInPeriod)

sbptRange <- seq(1, sbptCount)

prb <- txtProgressBar(max = sbptCount, style = 3) 

for (sbpt in sbptRange){
  
#   procRows <- getParentRows(childRow = BOMAtFacilityInPeriod[sbpt,], 
#                             childName = 'BOMAtFacilityInPeriod', 
#                             parentName = 'ProcessAtFacilityInPeriod', 
#                             RefCheckDefinition = RefCheckDefinition, 
#                             logFile = errorLog, logDepth = depthPlusOne)
#   
#   procRowCount <- length(procRows)
#   
#   if (procRowCount == 1){
  
  if (is.na(temp$Cycles_SBT_Index[sbpt])){
    
    add.constraint(lprec = thisModel, xt = 1, type = '=', rhs = 0, 
                   indices = temp$BOMUsage_SBPT_Index[sbpt])
    
  } else {
    
    coeffs <- c(-1, temp$Quantity[sbpt])
    
    varIndices <- c(temp$BOMUsage_SBPT_Index[sbpt], temp$Cycles_SBT_Index[sbpt]) 
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
    
    
  }
  
  setTxtProgressBar(prb, sbpt)
  
}

writeToLog('Completed.', fileConxn = errorLog, 
           depth = depthPlusOne, addTimeStamp = T)

write.lp(thisModel, 'thisModel.txt', type = 'lp')

break

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
