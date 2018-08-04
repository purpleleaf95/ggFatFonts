transformRawToGroupedSum <- function(x, y, z){

# Purpose:
#   Transform three vectors into one data frame which can then be editted by this package
#
# Inputs:
#   x - a vector containing the x values
#   y - a vector containing the y values
#   z - a vector containing the numeric z values
#
# Outputs:
#   A data frame that can then be editted by this package

  oldDataFrame <- data.frame('x' = x,
                             'y' = y,
                             'z' = z)
  oldDataFrame <- na.omit(oldDataFrame)
  oldDataFrame[, 1] <- as.factor(oldDataFrame[, 1])
  oldDataFrame[, 2] <- as.factor(oldDataFrame[, 2])

  # Create a table containing the combinations
  tempTable <- table(oldDataFrame[, 1:2])

  # Transform the table into data frame
  newDataFrame <- data.frame('x' = c(0),
                             'y' = c(0),
                             'z' = c(0))
  for (i in 1:nrow(tempTable)){
    for (j in 1:ncol(tempTable)) {
      tempDataFrame <- oldDataFrame[which(oldDataFrame[, 1] == rownames(tempTable)[i]), ]
      tempDataFrame <- tempDataFrame[which(tempDataFrame[, 2] == colnames(tempTable)[j]), ]
      tempSum <- sum(tempDataFrame[, 3])
      newDataFrame <- rbind(newDataFrame,
                            c(rownames(tempTable)[i],
                              colnames(tempTable)[j],
                              tempSum))
    }
  }
  newDataFrame <- newDataFrame[-c(1), ]

  return(newDataFrame)
}
