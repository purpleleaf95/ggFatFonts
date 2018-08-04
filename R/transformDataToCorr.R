transformDataToCorr <- function(data){

# Purpose:
#   Transform data into one data frame which can then be editted by this package
#
# Inputs:
#   data - a vector containing the data to be transformed
#
# Outputs:
#   A data frame that can then be editted by this package

  oldDataFrame <- as.data.frame(data.frame(data))

  # Transform the old data frame into a new one
  newDataFrame <- data.frame('var1' = c(0),
                             'var2' = c(0),
                             'corr' = c(0))
  uniqueNames <- colnames(oldDataFrame)
  tempTable <- cor(oldDataFrame)
  for (i in 1:nrow(tempTable)){
    for (j in 1:ncol(tempTable)) {
      newDataFrame <- rbind(newDataFrame,
                            c(rownames(tempTable)[i],
                              colnames(tempTable)[j],
                              tempTable[i, j]))
    }
  }
  newDataFrame <- newDataFrame[-c(1), ]
  colnames(newDataFrame) <- c('var1', 'var2', 'corr')
  newDataFrame[, 1] <- as.factor(newDataFrame[, 1])
  newDataFrame[, 2] <- as.factor(newDataFrame[, 2])
  newDataFrame[, 3] <- as.numeric(newDataFrame[, 3])

  return(newDataFrame)
}
