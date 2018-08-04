transformPointsToCluster <- function(dataFrameRaw,
                                     xBinLength = 10,
                                     yBinLength = 10){

# Purpose:
#   Change the data structure of the raw data frame
#
# Inputs:
#   dataFrame - a data frame containing the raw data
#
# Outputs:
#   A data frame that contains the cluster data

  # Initialize data frame
  dataFrameCluster <- data.frame(xMean = c(), yMean = c(), Freq = c())

  # Calculate min and max of x and y
  xMin <- min(dataFrameRaw[, 1])
  xMax <- max(dataFrameRaw[, 1])
  yMin <- min(dataFrameRaw[, 2])
  yMax <- max(dataFrameRaw[, 2])

  # Calculate number of bins
  if (((xMax - xMin) / xBinLength) %% xBinLength == 0){
    xBins <- (xMax - xMin) / xBinLength + 1
  } else {
    xBins <- ceiling((xMax - xMin) / xBinLength)
  }
  if (((yMax - yMin) / yBinLength) %% yBinLength == 0){
    yBins <- (yMax - yMin) / yBinLength + 1
  } else {
    yBins <- ceiling((yMax - yMin) / yBinLength)
  }

  # Fill the first two columns of data frame
  for (i in 1:xBins){
    for (j in 1:yBins) {
      dataFrameCluster <- rbind(dataFrameCluster,
                                c(xMin + xBinLength / 2 + (i - 1) * xBinLength,
                                  yMin + yBinLength / 2 + (j - 1) * yBinLength,
                                  0))
    }
  }

  # Calculate the frequencies
  k <- 1
  for (k in 1:nrow(dataFrameCluster)) {
    xLower <- dataFrameCluster[k, 1] - xBinLength / 2
    xUpper <- dataFrameCluster[k, 1] + xBinLength / 2
    yLower <- dataFrameCluster[k, 2] - yBinLength / 2
    yUpper <- dataFrameCluster[k, 2] + yBinLength / 2
    tempDataFrame <- dataFrameRaw[which(dataFrameRaw[, 1] >= xLower), ]
    tempDataFrame <- tempDataFrame[which(tempDataFrame[, 1] < xUpper), ]
    tempDataFrame <- tempDataFrame[which(tempDataFrame[, 2] >= yLower), ]
    tempDataFrame <- tempDataFrame[which(tempDataFrame[, 2] < yUpper),]
    dataFrameCluster[k, 3] <- nrow(tempDataFrame)
  }

  colnames(dataFrameCluster) <- c('xMean', 'yMean', 'Freq')
  dataFrameCluster[, 1] <- as.numeric(dataFrameCluster[, 1])
  dataFrameCluster[, 2] <- as.numeric(dataFrameCluster[, 2])
  dataFrameCluster[, 3] <- as.numeric(dataFrameCluster[, 3])

  return(dataFrameCluster)
}
