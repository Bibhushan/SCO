
#modelVariables <- read.csv('ModelVariables.txt', stringsAsFactors = F)

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

# set Starting Inventory of first period to opening inventory

# add shipping constraints

for (spt in seq(1, sptCount)){
  
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

for (spt in seq(1, sptCount)){
  
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
    
    
  }
  
}

# Balance of Flow Constraints

for (spt in seq(1, sptCount)){
  
    coeffs <- c(rep(-1, 3), rep(1, 3))
    
    varIndices <- c(Shipping_SPT_Increment, EndInv_SPT_Increment, Consumption_SPT_Increment,
                    Receiving_SPT_Increment, StartInv_SPT_Increment, Prodxn_SPT_Increment) + spt
    
    add.constraint(lprec = thisModel, xt = coeffs, type = '=', rhs = 0, 
                   indices = varIndices)
  
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
   
    tbl <- get(modelVariables$TableName[varTypeIndex]) 
    
    startIndex <- get(paste0(modelVariables$VariableName[varTypeIndex], '_StartIndex'))
    
    endIndex <- get(paste0(modelVariables$VariableName[varTypeIndex], '_EndIndex'))
    
    tbl[, modelVariables$FieldName[varTypeIndex]] <- optValues[startIndex:endIndex]
    
    assign(modelVariables$TableName[varTypeIndex], tbl)
    
    fileName <- paste0('Outputs/', modelVariables$TableName[varTypeIndex], '.csv')
    
    write.csv(x = tbl, file = fileName, row.names = F)
    
  }
  
}
