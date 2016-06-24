
source('readTableFromSource.R')
source('writeToLog.R')
source('matchFieldNames.R')
source('getKeyFields.R')
source('checkPrimaryKeyFields.R')
source('runGenericTableChecks.R')
source('dataConversionWarningHandler.R')
source('referentialIntegrityCheck.R')
source('getSanitisedValues.R')
source('getParentRows.R')
source('getParentValue.R')
source('getKeyFieldsFromTable.R')

library(lpSolveAPI)

file.create('ErrorLog.txt')

log <- 'log.txt'

errorLog <- 'ErrorLog.txt'

logDepth = 0

depthPlusOne <- logDepth + 1

# load data tables from csv files in R environment.

source('loadDataFiles.R')

# run generic data checks on tables.
# fileSources has been loaded by the script ran before


# populate missing links in TransportationLinks
# 
# custData <- Demand[, c('Customer', 'Product', 'Period')]
# 
# names(custData) <- c('SiteName', 'Product', 'Period')
# 
# origSPT <- ProductAtFacilityInPeriod[, c('SiteName', 'Product', 'Period')]
# 
# destSPT <- rbind(custData, origSPT)
# 
# blankProdRows <- which(TransportationLink$Product == '')
# 
# tempTransLinks <- TransportationLink[0, ]
# 
# prb <- txtProgressBar(max = length(blankProdRows), style = 3)
# 
# prbValue <- 0
# 
# for (row in blankProdRows){
#   
#   destRows <- which(destSPT$SiteName == TransportationLink$Destination[row] &
#                       destSPT$Period == TransportationLink$Period[row])
#   
#   destProds <- unique(destSPT$Product[destRows])
#   
#   origRows <- which(origSPT$SiteName == TransportationLink$Destination[row] &
#                       origSPT$Period == TransportationLink$Period[row])
#   
#   origProds <- unique(origSPT$Product[origRows])
#   
#   prods <- intersect(destProds, origProds)
#   
#   tempRows <- TransportationLink[rep(row, length(prods)), ]
#   
#   tempRows$Product <- prods
#   
#   tempTransLinks <- rbind(tempTransLinks, tempRows)
#   
#   prbValue <- prbValue + 1
#   
#   setTxtProgressBar(prb, prbValue)
#   
# }
# 
# TransportationLink <- TransportationLink[-blankProdRows,]
# 
# TransportationLink <- rbind(TransportationLink, tempTransLinks)
# 
# write.csv(TransportationLink, 'data/TransportationLink.csv', row.names = F)

# now check the data for valid field names and field types
# 
# branchGroups <- read.csv('BranchGroups.csv', stringsAsFactors = F)
# 
# groups <- unique(branchGroups$SiteType)
# 
# tempTransLinks <- TransportationLink[0, ]
# 
# for(grp in groups){
#  
#   grpRows <- which(branchGroups$SiteType == grp)
#   
#   grpRowCount <- length(grpRows)
#     
#   tempRows <- which(TransportationLink$Destination == grp)
#   
#   tempRowCount <- length(tempRows)
#   
#   tempLinks <- TransportationLink[rep(tempRows, grpRowCount), ]
#   
#   dest <- NULL
#   
#   for (row in grpRows){
#     
#     dest <- c(dest, rep(branchGroups$SiteName[row], tempRowCount))
#     
#   }
#   
#   tempLinks$Destination <- dest
#   
#   TransportationLink <- TransportationLink[-tempRows,]
#   
#   TransportationLink <- rbind(TransportationLink, tempLinks)
#   
# }
# 
# write.csv(TransportationLink, 'data/TransportationLink.csv', row.names = F)

source('runDataChecks.R')

# now check the referential checks.

source('runRefChecks.R')


#source('formulateMILP.R')

