#' @import ggplot2

#' @title Correlation Matrix using GGPlot2 and FatFonts
#'
#' @description ggFFCorrMatr() produces Correlation Matrix using GGPlot2 and FatFonts.
#'
#' @usage
#' ggFFCorrMatr(data, tileBorderColor = '#C2C2C2', tileBorderSize = 0.1,
#' tileFill = '#FFFFFF', shrink = -1, font = 'Cubica', fontSize = -1,
#' fontColorPos = 'red', fontColorNeg = 'blue', isSquare = TRUE, adjustPos = 3,
#' changeFontColValue = NULL, changeFontColDigit = NULL, changeBackCol = NULL, ...)
#'
#' @param data A vector containing the data.
#' @param tileBorderColor The color of the border of the tiles. (default: '#C2C2C2')
#' @param tileVorderSize The size of the border of the tiles. (default: 0.1)
#' @param tileFill The fill color of the tiles. (default: '#FFFFFF')
#' @param shrink To what extent are the raw values reduced. For instance, if shrink = 1000 and the original data is D, then the values displayed by FatFonts will be 1000D. If shrink = -1, then shrink will be chosen automatically. (default: -1)
#' @param font The FatFonts family used in the plot. There are currently three FatFonts types available: "Cubica", "Miguta" and "Rotunda". (default: 'Cubica')
#' @param fontSize The font size of the smallest FatFonts digit. If fontSize = -1, then font size will be chosen automatically. (default: -1)
#' @param fontColorPos The color of positive FatFonts digits. (default:'red')
#' @param fontColorNeg The color of negative FatFonts digits. (default:'blue')
#' @param isSquare If each grid is square. It is better to display FatFonts numbers in square rather than rectangle grids. (default: TRUE)
#' @param adjustPos Parameter used to slightly adjust the location of FatFonts digits. (default: 3)
#' @param changeFontColValue A function used to change font color based on value. An example function can be defined as follow: changeFontColValueFunc <- function(oldPlot){newPlot <- oldPlot + scale_color_gradient2(); return(newPlot)}. (default: NULL)
#' @param changeFontColDigit A vector containing the 9 colors used to represent each digit respectively (in the order of 1 to 9). You can also use color combinations provided by the system (e.g. topo.colors(9)). (default: NULL)
#' @param changeBackCol A function used to change tile fill color based on value. An example function can be defined as follow: changeBackColFunc <- function(oldPlot){newPlot <- oldPlot + scale_fill_gradient2(); return(newPlot)}. (default: NULL)
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @examples
#' ggFFCorrMatr(mtcars[,c(1,3:7)])
#'
#' ggFFCorrMatr(mtcars[c(1, 3:7)], fontColorPos = 'blue', fontColorNeg = 'green',
#'  fontSize = 1)
#'
#' ggFFCorrMatr(mtcars[c(1, 3:7)], font = 'Miguta')
#'
#' ggFFCorrMatr(mtcars[c(1, 3:7)], shrink = 10, fontSize = 3)
#'
#' ggFFCorrMatr(mtcars[c(1, 3:7)], tileFill = 'light grey', fontSize = 1)
#'
#' ggFFCorrMatr(mtcars[c(1, 3:7)], isSquare = FALSE, fontSize = 1)
#'
#' changeBackColFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_fill_gradient2()
#'   return(newPlot)
#' }
#' ggFFCorrMatr(mtcars[c(1, 3:7)], changeBackCol = changeBackColFunc,
#'  fontColorPos = 'yellow', fontColorNeg = 'yellow', fontSize = 1)
#'
#' changeFontColValueFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_color_gradient2()
#'  return(newPlot)
#' }
#' ggFFCorrMatr(mtcars[c(1, 3:7)], changeFontColValue = changeFontColValueFunc,
#'  adjustPos = 1, fontSize = 1)
#'
#' cols <- topo.colors(9)
#' ggFFCorrMatr(mtcars[c(1, 3:7)], changeFontColDigit = cols,
#'  adjustPos = 1, fontSize = 1)
#'
#' @export
ggFFCorrMatr <- function(data,
                         tileBorderColor = '#C2C2C2',
                         tileBorderSize = 0.1,
                         tileFill = '#FFFFFF',
                         shrink = -1,
                         font = 'Cubica',
                         fontSize = -1,
                         fontColorPos = 'red',
                         fontColorNeg = 'blue',
                         isSquare = TRUE,
                         adjustPos = 3,
                         changeFontColValue = NULL,
                         changeFontColDigit = NULL,
                         changeBackCol = NULL,
                         ...){


  # Change data structure
  dataFrame <- transformDataToCorr(data)
  dataFrame <- na.omit(dataFrame)

  # Choose shrink automatically
  if (shrink == -1){
    # Max = 1 in correlation matrix
    shrink <- 100
  }

  # Choose font size automatically
  if (fontSize == -1){
    # Find the highest place of the largest number to decide the level of FatFonts
    place <- 1
    max <- max(abs(round(dataFrame$corr * shrink, 0)))
    while (floor(max / 10) > 0) {
      place <- place + 1
      max <- floor(max / 10)
    }
    # Find shrink1
    shrink1 <- findShrink1Value(font)
    # Find suitable font size
    fontSize = (80 / (max(c(dataFrame[, 1], dataFrame[, 2])) + 1)) / (shrink1 ^ (place - 1))
  }

  # Draw Correlation Matrix
  myPlot <- ggplot(data = dataFrame,
                   mapping = aes(x = var1,
                                 y = var2,
                                 fill = corr))
  # Add layer of tiles
  if (is.null(changeBackCol) == TRUE) {
    myPlot <- myPlot + geom_tile(color = tileBorderColor,
                                 size = tileBorderSize,
                                 fill = tileFill)
  } else {
    myPlot <- myPlot + geom_tile(aes(fill = dataFrame[, 3] * shrink),
                                 color = tileBorderColor,
                                 size = tileBorderSize)
    myPlot <- changeBackCol(myPlot)
  }
  # Add layer of titles
  myPlot <- myPlot + labs(title = 'Correlation Matrix',
                          x = 'var1',
                          y = 'var2',
                          fill = 'fill')
  if (shrink != 1){
    if ((is.null(changeFontColValue) == TRUE) &&
        (is.null(changeFontColDigit) == TRUE)) {
      myPlot <- myPlot + labs(caption = paste(fontColorNeg,
                                              ': negative correlation\n',
                                              fontColorPos,
                                              ': positive correlation\n',
                                              'unit: /', shrink, sep = ''))
    } else {
      myPlot <- myPlot + labs(caption = paste('unit: /', shrink, sep = ''))
    }
  } else {
    if ((is.null(changeFontColValue) == TRUE) &&
        (is.null(changeFontColDigit) == TRUE)) {
      myPlot <- myPlot + labs(caption = paste(fontColorNeg,
                                              ': negative correlation\n',
                                              fontColorPos,
                                              ': positive correlation',
                                              sep = ''))
    }
  }
  # Transform to square grids
  if (isSquare == TRUE){
    myPlot <- myPlot + coord_fixed(expand = FALSE)
  }
  if ((is.null(changeFontColValue) == TRUE) &&
      (is.null(changeFontColDigit) == TRUE)) {
    # Divide into positve and negative data
    positiveData <- c()
    negativeData <- c()
    for (i in 1:length(dataFrame[, 3])){
      if (dataFrame[i, 3] > 0){
        positiveData <- c(positiveData, dataFrame[i, 3] * shrink)
        negativeData <- c(negativeData, 0)
      } else if (dataFrame[i, 3] < 0){
        positiveData <- c(positiveData, 0)
        negativeData <- c(negativeData, -dataFrame[i, 3] * shrink)
      } else {
        positiveData <- c(positiveData, 0)
        negativeData <- c(negativeData, 0)
      }
    }
    positiveData <- as.numeric(positiveData)
    negativeData <- as.numeric(negativeData)
    # Add FatFonts layer
    myPlot <- addGGFFLayer(oldPlot = myPlot,
                           data = positiveData,
                           xBinLength = 1,
                           yBinLength = 1,
                           font = font,
                           fontSize = fontSize,
                           fontColor = fontColorPos,
                           adjustPos = adjustPos)
    myPlot <- addGGFFLayer(oldPlot = myPlot,
                           data = negativeData,
                           xBinLength = 1,
                           yBinLength = 1,
                           font = font,
                           fontSize = fontSize,
                           fontColor = fontColorNeg,
                           adjustPos = adjustPos)
  } else {
    # Add FatFonts layer
    myPlot <- addGGFFLayer(oldPlot = myPlot,
                           data = dataFrame[, 3] * shrink,
                           xBinLength = 1,
                           yBinLength = 1,
                           font = font,
                           fontSize = fontSize,
                           adjustPos = adjustPos,
                           changeFontColValue = changeFontColValue,
                           changeFontColDigit = changeFontColDigit)
  }

  return(myPlot)
}

