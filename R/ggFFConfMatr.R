#' @import ggplot2

#' @title Confusion Matrix using GGPlot2 and FatFonts
#'
#' @description ggFFConfMatr() produces Confusion Matrix using GGPlot2 and FatFonts.
#'
#' @usage
#' ggFFConfMatr(actuClass, predClass, tileBorderColor = '#C2C2C2', tileBorderSize = 0.1,
#' tileFill = '#FFFFFF', shrink = -1, font = 'Cubica', fontSize = -1,
#' fontColor = '#000000', isSquare = TRUE, adjustPos = 3, changeFontColValue = NULL,
#' changeFontColDigit = NULL, changeBackCol = NULL, ...)
#'
#' @param actuClass A vector containing the actual values. actuClass and predClass should be of the same length.
#' @param predClass A vector containing the predicted values. actuClass and predClass should be of the same length.
#' @param tileBorderColor The color of the border of the tiles. (default: '#C2C2C2')
#' @param tileBorderSize The size of the border of the tiles. (default: 0.1)
#' @param tileFill The fill color of the tiles. (default: '#FFFFFF')
#' @param shrink To what extent are the raw values reduced. For instance, if shrink = 1000 and the original data is D, then the values displayed by FatFonts will be 1000D. If shrink = -1, then shrink will be chosen automatically. (default: -1)
#' @param font The FatFonts family used in the plot. There are currently three FatFonts types available: "Cubica", "Miguta" and "Rotunda". (default: 'Cubica')
#' @param fontSize The font size of the smallest FatFonts digit. If fontSize = -1, then font size will be chosen automatically. (default: -1)
#' @param fontColor The font color of FatFonts digits. (default: '#000000')
#' @param isSquare If each grid is square. It is better to display FatFonts numbers in square rather than rectangle grids. (default: TRUE)
#' @param adjustPos Parameter used to slightly adjust the location of FatFonts digits. (default: 3)
#' @param changeFontColValue A function used to change font color based on value. An example function can be defined as follow: changeFontColValueFunc <- function(oldPlot){newPlot <- oldPlot + scale_color_gradient2(); return(newPlot)}. (default: NULL)
#' @param changeFontColDigit A vector containing the 9 colors used to represent each digit respectively (in the order of 1 to 9). You can also use color combinations provided by the system (e.g. topo.colors(9)). (default: NULL)
#' @param changeBackCol A function used to change tile fill color based on value. An example function can be defined as follow: changeBackColFunc <- function(oldPlot){newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple'); return(newPlot)}. (default: NULL)
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @examples
#' ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`)
#'
#' ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
#'  adjustPos = 12)
#'
#' ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
#'  adjustPos = 12, fontSize = 5)
#'
#' ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
#'  adjustPos = 13, font = 'Rotunda', fontSize = 5)
#'
#' ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
#'  adjustPos = 4, shrink = 1000)
#'
#' ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
#'  adjustPos = 16, isSquare = FALSE)
#'
#' ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
#'  fontColor = 'blue')
#'
#' changeBackColFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple')
#'   return(newPlot)
#' }
#' ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
#'  changeBackCol = changeBackColFunc)
#'
#' changeFontColValueFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_color_continuous(low = 'dark grey', high = 'purple')
#'  return(newPlot)
#' }
#' ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
#'  changeFontColValue = changeFontColValueFunc)
#'
#' cols <- topo.colors(9)
#' ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
#'  changeFontColDigit = cols)
#'
#' @export
ggFFConfMatr <- function(actuClass,
                         predClass,
                         tileBorderColor = '#C2C2C2',
                         tileBorderSize = 0.1,
                         tileFill = '#FFFFFF',
                         shrink = -1,
                         font = 'Cubica',
                         fontSize = -1,
                         fontColor = '#000000',
                         isSquare = TRUE,
                         adjustPos = 3,
                         changeFontColValue = NULL,
                         changeFontColDigit = NULL,
                         changeBackCol = NULL,
                         ...){

  # actuClass and predClass should be of the same length
  if (length(actuClass) != length(predClass)) {
    stop("actuClass and predClass should be of the same length")
  }

  # Change data structure
  dataFrame <- transformConfMatrToFreq(actuClass, predClass)
  dataFrame <- na.omit(dataFrame)

  # Choose shrink automatically
  if (shrink == -1){
    # Find the highest place of the largest number to decide the shrink value
    max <- max(abs((dataFrame[, 4])))
    if (max >= 1){
      place <- 0
      while (floor(max) > 0) {
        place <- place + 1
        max <- max / 10
      }
    } else {
      place <- 1
      while (floor(max) == 0) {
        place <- place - 1
        max <- max * 10
      }
    }
    shrink <- 0.1 ^ (place - 2)
  }

  # Choose font size automatically
  if (fontSize == -1){
    # Find the highest place of the largest number to decide the level of FatFonts
    place <- 1
    max <- max(abs(round(dataFrame[, 4] * shrink, 0)))
    while (floor(max / 10) > 0) {
      place <- place + 1
      max <- floor(max / 10)
    }
    # Find shrink1
    shrink1 <- findShrink1Value(font)
    # Find suitable font size
    fontSize = (60 / (max(c(dataFrame[, 1], dataFrame[, 2])) + 1)) / (shrink1 ^ (place - 1))
  }

  # Draw Confusion Matrix
  myPlot <- ggplot(data = dataFrame,
                   mapping = aes(x = actualClass,
                                 y = predictedClass,
                                 fill = prob))
  # Add layer of tiles
  if (is.null(changeBackCol) == TRUE) {
    myPlot <- myPlot + geom_tile(color = tileBorderColor,
                                 size = tileBorderSize,
                                 fill = tileFill)
  } else {
    myPlot <- myPlot + geom_tile(aes(fill = dataFrame[, 4] * shrink),
                                 color = tileBorderColor,
                                 size = tileBorderSize)
    myPlot <- changeBackCol(myPlot)
  }

  # Add layer of titles
  myPlot <- myPlot + labs(title = 'Confusion Matrix',
                          x = 'actual class',
                          y = 'predicted class',
                          fill = 'fill')
  if (shrink != 1){
    myPlot <- myPlot + labs(caption = paste('unit: /', shrink, sep = ''))
  }
  # Transform to square grids
  if (isSquare == TRUE){
    myPlot <- myPlot + coord_fixed(expand = FALSE)
  }
  # Add FatFonts layer
  myPlot <- addGGFFLayer(oldPlot = myPlot,
                         data = dataFrame[, 4] * shrink,
                         xBinLength = 1,
                         yBinLength = 1,
                         font = font,
                         fontSize = fontSize,
                         fontColor = fontColor,
                         adjustPos = adjustPos,
                         changeFontColValue = changeFontColValue,
                         changeFontColDigit = changeFontColDigit)

  return(myPlot)
}

