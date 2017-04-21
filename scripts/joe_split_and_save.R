sub_file <- "path/to/file"

Sub_dt <- data.table::fread(sub_file)

regions <- unique(Sub_dt[,1]) # Returns the unique region values
states  <- unique(Sub_dt[,2]) # Returns the unique state values

# Split data by REGION and save to a unique CSV file  

lapply(1:nrow(regions), function(x) {
  
  obj <- Sub_dt[REGION==regions[x,1]]
  save.name <- paste0('Region',regions[x,1],'.csv')
  write.csv(obj, file = save.name, row.names = F)
  
})


# Split data by STATEFIP and save to a unique CSV file  

lapply(1:nrow(states), function(x) {
  
  obj <- Sub_dt[STATEFIP==states[x,1]]
  save.name <- paste0('StateFIP',states[x,1],'.csv')
  write.csv(obj, file = save.name, row.names = F)
  
})
