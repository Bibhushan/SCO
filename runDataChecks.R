
cat(paste(Sys.time(), 'Started loading data', sep = ': '), 
    file = 'ErrorLog.txt', append = F)

runGenericTableChecks(fileSources, 'FileSources', dataDefinition, errorLog,logDepth = 1)

runGenericTableChecks(modelSettings, 'ModelSettings', dataDefinition, errorLog,
                      logDepth = 1)

runGenericTableChecks(products, 'Product', dataDefinition, errorLog,logDepth = 1)

runGenericTableChecks(sites, 'Site', dataDefinition, errorLog, logDepth = 1)

runGenericTableChecks(periods, 'Period', dataDefinition, errorLog, logDepth = 1)

runGenericTableChecks(transModes, 'TransMode', dataDefinition, errorLog,logDepth = 1)

runGenericTableChecks(boms, 'BOM', dataDefinition, errorLog,logDepth = 1)

runGenericTableChecks(processes, 'Process', dataDefinition, errorLog,logDepth = 1)

runGenericTableChecks(prodFacPer, 'ProductAtFacilityInPeriod', dataDefinition, 
                      errorLog,logDepth = 1)

runGenericTableChecks(transLinks, 'TransportationLink', dataDefinition, errorLog, 
                      logDepth = 1)