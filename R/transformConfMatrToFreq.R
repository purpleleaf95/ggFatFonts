transformConfMatrToFreq <- function(actuClass, predClass){

# Purpose:
#   Transform two vectors into one data frame which can then be editted by this package
#
# Inputs:
#   actuData - a vector containing the actual values
#   predData - a vector containing the predicted values
#
# Outputs:
#   A data frame that can then be editted by this package

  oldDataFrame <- data.frame('actualClass' = actuClass,
                             'predictedClass' = predClass)

  # Transform the old data frame into a new one
  newDataFrame <- data.frame('actualClass' = c(0),
                             'predictedClass' = c(0),
                             'frequency' = c(0))
  uniqueNames <- unique(c(unique(oldDataFrame[, 1]), unique(oldDataFrame[, 2])))
  for (i in 1:length(uniqueNames)){
    for (j in 1:length(uniqueNames)) {
      tempDataFrame <- oldDataFrame[which(oldDataFrame[, 1] == uniqueNames[i]), ]
      tempDataFrame <- tempDataFrame[which(tempDataFrame[, 2] == uniqueNames[j]), ]
      tempSum <- nrow(tempDataFrame)
      newDataFrame <- rbind(newDataFrame,
                            c(uniqueNames[i],
                              uniqueNames[j],
                              tempSum))
    }
  }
  newDataFrame <- newDataFrame[-c(1), ]
  newDataFrame <- cbind(newDataFrame, newDataFrame[, 3] / sum(newDataFrame[, 3]))
  colnames(newDataFrame) <- c('actualClass', 'predictedClass',
                              'freq', 'prob')
  newDataFrame[, 1] <- as.factor(newDataFrame[, 1])
  newDataFrame[, 2] <- as.factor(newDataFrame[, 2])
  newDataFrame[, 3] <- as.numeric(newDataFrame[, 3])
  newDataFrame[, 4] <- as.numeric(newDataFrame[, 4])

  return(newDataFrame)
}
