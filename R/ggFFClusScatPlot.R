#' @import ggplot2

#' @title Cluster Scatter Plot using GGPlot2 and FatFonts
#'
#' @description ggFFClusScatPlot() produces Cluster Scatter Plot using GGPlot2 and FatFonts. The FatFonts numbers in each grid represents the number of points within each grid.
#'
#' @usage
#' ggFFClusScatPlot(x, y, xAxisTitle = 'x', yAxisTitle = 'y', panelFill = '#FAFAFA',
#' pointColor = '#C2C2C2', xBinLength = 3, yBinLength = 3, tileBorderColor = '#C2C2C2',
#' tileBorderSize = 0.1, tileFill = 'transparent', shrink = -1, font = 'Cubica',
#' fontSize = -1, fontColor = '#000000', isSquare = TRUE, showPoints = FALSE,
#' adjustPos = 1, changeFontColValue = NULL, changeFontColDigit = NULL, changeBackCol = NULL, ...)
#'
#' @param x A vector containing the x values of each data point. x and y should be of the same length.
#' @param y A vector containing the y values of each data point. x and y should be of the same length.
#' @param xAxisTitle The title of x axis. (default:'x')
#' @param yAxisTitle The title of y axis. (default: 'y')
#' @param panelFill The color of the fill of the panel. (default: '#FAFAFA')
#' @param pointColor The color of the points. (default: '#C2C2C2')
#' @param xBinLength The difference between each two neighboring numbers on the x axis. (default: 3)
#' @param yBinLength The difference between each two neighboring numbers on the y axis. (default: 3)
#' @param tileBorderColor The color of the border of the tiles. (default: '#C2C2C2')
#' @param tileBorderSize The size of the border of the tiles. (default: 0.1)
#' @param tileFill The fill color of the tiles. (default: 'transparent')
#' @param shrink To what extent are the raw values reduced. For instance, if shrink = 1000 and the original data is D, then the values displayed by FatFonts will be 1000D. If shrink = -1, then shrink will be chosen automatically. (default: -1)
#' @param font The FatFonts family used in the plot. There are currently three FatFonts types available: "Cubica", "Miguta" and "Rotunda". (default: 'Cubica')
#' @param fontSize The font size of the smallest FatFonts digit. If fontSize = -1, then font size will be chosen automatically. (default: -1)
#' @param fontColor The font color of FatFonts digits. (default: '#000000')
#' @param isSquare If each grid is square. It is better to display FatFonts numbers in square rather than rectangle grids. (default: TRUE)
#' @param showPoints If normal scatter plot is shown at the lowest layer. (default: FALSE)
#' @param adjustPos Parameter used to slightly adjust the location of FatFonts digits. (default: 1)
#' @param changeFontColValue A function used to change font color based on value. An example function can be defined as follow: changeFontColValueFunc <- function(oldPlot){newPlot <- oldPlot + scale_color_gradient2(); return(newPlot)}. (default: NULL)
#' @param changeFontColDigit A vector containing the 9 colors used to represent each digit respectively (in the order of 1 to 9). You can also use color combinations provided by the system (e.g. topo.colors(9)). (default: NULL)
#' @param changeBackCol A function used to change tile fill color based on value. An example function can be defined as follow: changeBackColFunc <- function(oldPlot){newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple'); return(newPlot)}. (default: NULL)
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @examples
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy')
#'
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  xBinLength = 5, yBinLength = 10, adjustPos = 1.5)
#'
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  font = 'Rotunda', adjustPos = 1.1)
#'
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  tileFill = 'black', fontColor = 'white')
#'
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  isSquare = FALSE, adjustPos = 1.5)
#'
#' changeBackColFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple')
#'   return(newPlot)
#' }
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  changeBackCol = changeBackColFunc)
#'
#' changeFontColValueFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_color_continuous(low = 'dark grey', high = 'purple')
#'  return(newPlot)
#' }
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  changeFontColValue = changeFontColValueFunc)
#'
#' cols <- topo.colors(9)
#' ggFFClusScatPlot(mpg$cty, mpg$hwy, xAxisTitle = 'cty', yAxisTitle = 'hwy',
#'  changeFontColDigit = cols)
#'
#' @export
ggFFClusScatPlot <- function(x,
                             y,
                             xAxisTitle = 'x',
                             yAxisTitle = 'y',
                             panelFill = '#FAFAFA',
                             pointColor = '#C2C2C2',
                             xBinLength = 3,
                             yBinLength = 3,
                             tileBorderColor = '#C2C2C2',
                             tileBorderSize = 0.1,
                             tileFill = 'transparent',
                             shrink = -1,
                             font = 'Cubica',
                             fontSize = -1,
                             fontColor = '#000000',
                             isSquare = TRUE,
                             showPoints = FALSE,
                             adjustPos = 1,
                             changeFontColValue = NULL,
                             changeFontColDigit = NULL,
                             changeBackCol = NULL,
                             ...){

  # x and y should be of the same length

  if (length(x) != length(y)) {
    stop("x and y should be of the same length")
  }

  # Change data structure
  dataFrameRaw <- data.frame(x = x, y = y)
  dataFrameRaw <- na.omit(dataFrameRaw)
  colnames(dataFrameRaw) <- c(xAxisTitle, yAxisTitle)
  dataFrameCluster <- transformPointsToCluster(dataFrameRaw,
                                               xBinLength = xBinLength,
                                               yBinLength = yBinLength)
  colnames(dataFrameCluster) <- c(xAxisTitle, yAxisTitle, 'freq')

  # Choose shrink automatically
  if (shrink == -1){
    # Find the highest place of the largest number to decide the shrink value
    max <- max(abs(dataFrameCluster[, 3]))
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
    max <- max(abs(round(dataFrameCluster[, 3] * shrink, 0)))
    while (floor(max / 10) > 0) {
      place <- place + 1
      max <- floor(max / 10)
    }
    # Find shrink1
    shrink1 <- findShrink1Value(font)
    # Find suitable font size
    xBin <- ceiling(max(dataFrameCluster[, 1]) - min(dataFrameCluster[, 1])) / xBinLength
    yBin <- ceiling(max(dataFrameCluster[, 2]) - min(dataFrameCluster[, 2])) / yBinLength
    fontSize = ((60 / max(xBin, yBin))/ (place - 1)) / (shrink1 ^ (place - 1))
  }

  # Initialize ggplot
  myPlot <- ggplot(data = dataFrameCluster,
                   mapping = aes(x = get(xAxisTitle),
                                 y = get(yAxisTitle),
                                 fill = freq * shrink)) +
    theme(panel.background = element_rect(fill = panelFill))
  # Add layer of scatter plot
  if (showPoints == TRUE){
    myPlot <- myPlot + geom_point(data = dataFrameRaw,
                                  mapping = aes(x = get(xAxisTitle),
                                                y = get(yAxisTitle),
                                                fill = NULL),
                                  color = pointColor)
  }
  # Add layer of tiles
  if (is.null(changeBackCol) == TRUE) {
    myPlot <- myPlot + geom_tile(color = tileBorderColor,
                                 size = tileBorderSize,
                                 fill = tileFill)
    # Remove legend
    if (is.null(changeFontColValue) == TRUE) {
      myPlot <- myPlot + theme(legend.position = 'none')
    }
  } else {
    myPlot <- myPlot + geom_tile(aes(fill = dataFrameCluster[, 3] * shrink),
                                 color = tileBorderColor,
                                 size = tileBorderSize)
    myPlot <- changeBackCol(myPlot)
  }
  # Add titles
  myPlot <- myPlot + labs(title = 'Cluster Scatter Plot',
                          x = xAxisTitle,
                          y = yAxisTitle,
                          fill = 'fill')
  if (shrink != 1){
    myPlot <- myPlot + labs(caption = paste('unit: /', shrink, sep = ''))
  }
  # Transform to square grids
  if (isSquare == TRUE){
    myPlot <- myPlot + coord_fixed(ratio = xBinLength / yBinLength, expand = FALSE)
  }

  # Add FatFonts layer
  myPlot <- addGGFFLayer(oldPlot = myPlot,
                         data = dataFrameCluster[, 3] * shrink,
                         xBinLength = xBinLength,
                         yBinLength = yBinLength,
                         font = font,
                         fontSize = fontSize,
                         fontColor = fontColor,
                         adjustPos = adjustPos,
                         changeFontColValue = changeFontColValue,
                         changeFontColDigit = changeFontColDigit)

  return(myPlot)
}
