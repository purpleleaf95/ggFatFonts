#' @import ggplot2

#' @title Heatmap using GGPlot2 and FatFonts
#'
#' @description ggFFConfMatr() produces Heatmap using GGPlot2 and FatFonts.
#'
#' @usage
#' ggFFConfMatr(x, y, z, xAxisTitle = 'x', yAxisTitle = 'y', zAxisTitle = 'z',
#' tileBorderColor = '#C2C2C2', tileBorderSize = 0.1, tileFill = '#FFFFFF',
#' shrink = -1, font = 'Cubica', fontSize = -1, fontColor = '#000000',
#' isSquare = TRUE, adjustPos = 2, changeFontColValue = NULL,
#' changeFontColDigit = NULL, changeBackCol = NULL, ...)
#'
#' @param x A vector containing the x values of each data point. x, y and z should be of the same length.
#' @param y A vector containing the y values of each data point. x, y and z should be of the same length.
#' @param z A numeric vector containing the values to be displayed in FatFonts. x, y and z should be of the same length.
#' @param xAxisTitle The title of x axis. (default:'x')
#' @param yAxisTitle The title of y axis. (default: 'y')
#' @param zAxisTitle The title of the data to be displayed in FatFonts. (default: 'z')
#' @param tileBorderColor The color of the border of the tiles. (default: '#C2C2C2')
#' @param tileVorderSize The size of the border of the tiles. (default: 0.1)
#' @param tileFill The fill color of the tiles. (default: '#FFFFFF')
#' @param shrink To what extent are the raw values reduced. For instance, if shrink = 1000 and the original data is D, then the values displayed by FatFonts will be 1000D. If shrink = -1, then shrink will be chosen automatically. (default: -1)
#' @param font The FatFonts family used in the plot. There are currently three FatFonts types available: "Cubica", "Miguta" and "Rotunda". (default: 'Cubica')
#' @param fontSize The font size of the smallest FatFonts digit. If fontSize = -1, then font size will be chosen automatically. (default: -1)
#' @param fontColor The color of FatFonts digit. (default:'#000000')
#' @param isSquare If each grid is square. It is better to display FatFonts numbers in square rather than rectangle grids. (default: TRUE)
#' @param adjustPos Parameter used to slightly adjust the location of FatFonts digits. (default: 2)
#' @param changeFontColValue A function used to change font color based on value. An example function can be defined as follow: changeFontColValueFunc <- function(oldPlot){newPlot <- oldPlot + scale_color_gradient2(); return(newPlot)}. (default: NULL)
#' @param changeFontColDigit A vector containing the 9 colors used to represent each digit respectively (in the order of 1 to 9). You can also use color combinations provided by the system (e.g. topo.colors(9)). (default: NULL)
#' @param changeBackCol A function used to change tile fill color based on value. An example function can be defined as follow: changeBackColFunc <- function(oldPlot){newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple'); return(newPlot)}. (default: NULL)
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @examples
#' ggFFHeatMap(x = txhousing$year[1:3000], y = txhousing$city[1:3000],
#'  z = txhousing$sales[1:3000], xAxisTitle = 'year', yAxisTitle = 'city',
#'  zAxisTitle = 'sales')
#'
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'   yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2)
#'
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2,
#'  font = 'Miguta')
#'
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', shrink = 0.01,
#'  adjustPos = 1)
#'
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2,
#'  tileFill = 'black', fontColor = 'white')
#'
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2,
#'  isSquare = FALSE)
#'
#' changeBackColFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple')
#'   return(newPlot)
#' }
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2,
#'  changeBackCol = changeBackColFunc)
#'
#' changeFontColValueFunc <- function(oldPlot){
#'   newPlot <- oldPlot + scale_color_continuous(low = 'dark grey', high = 'purple')
#'  return(newPlot)
#' }
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2,
#'  changeFontColValue = changeFontColValueFunc)
#'
#' cols <- topo.colors(9)
#' ggFFHeatMap(x = txhousing$year[1:2000], y = txhousing$city[1:2000],
#'  z = txhousing$sales[1:2000], xAxisTitle = 'year',
#'  yAxisTitle = 'city', zAxisTitle = 'sales', fontSize = 2,
#'  changeFontColDigit = cols)
#'
#' @export
ggFFHeatMap <- function(x,
                        y,
                        z,
                        xAxisTitle = 'x',
                        yAxisTitle = 'y',
                        zAxisTitle = 'z',
                        tileBorderColor = '#C2C2C2',
                        tileBorderSize = 0.1,
                        tileFill = '#FFFFFF',
                        shrink = -1,
                        font = 'Cubica',
                        fontSize = -1,
                        fontColor = '#000000',
                        isSquare = TRUE,
                        adjustPos = 2,
                        changeFontColValue = NULL,
                        changeFontColDigit = NULL,
                        changeBackCol = NULL,
                        ...){

  # x, y and z should be of the same length
  if ((length(x) != length(y)) || (length(x) != length(z)) || (length(y) != length(z))) {
    stop("x, y and z should be of the same length")
  }

  # Change data structure
  dataFrame <- transformRawToGroupedSum(x, y, z)
  dataFrame <- na.omit(dataFrame)
  dataFrame[, 1] <- as.factor(dataFrame[, 1])
  dataFrame[, 2] <- as.factor(dataFrame[, 2])
  dataFrame[, 3] <- round(as.numeric(dataFrame[, 3]), 0)
  colnames(dataFrame) <- c(xAxisTitle, yAxisTitle, zAxisTitle)

  # Choose shrink automatically
  if (shrink == -1){
    # Find the highest place of the largest number to decide the shrink value
    max <- max(abs(dataFrame[, 3]))
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
    max <- max(abs(round(dataFrame[, 3] * shrink, 0)))
    while (floor(max / 10) > 0) {
      place <- place + 1
      max <- floor(max / 10)
    }
    # Find shrink1
    shrink1 <- findShrink1Value(font)
    # Find suitable font size
    fontSize = (60 / (max(length(unique(x)),length(unique(y))))) / (shrink1 ^ (place - 1))
  }

  # Draw heatmap
  myPlot <- ggplot(data = dataFrame,
                   mapping = aes(x = get(xAxisTitle),
                                 y = get(yAxisTitle),
                                 fill = get(zAxisTitle) * shrink))
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
  myPlot <- myPlot +
    labs(title = 'Heat Map',
         x = xAxisTitle,
         y = yAxisTitle,
         fill = 'fill') +
    theme(axis.text.x = element_text(angle = 90))
  if (shrink != 1){
    myPlot <- myPlot + labs(caption = paste('unit: /', shrink, sep = ''))
  }
  # Transform to square grids
  if (isSquare == TRUE){
    myPlot <- myPlot + coord_fixed(expand = FALSE)
  }
  # Add FatFonts layer
  myPlot <- addGGFFLayer(oldPlot = myPlot,
                         data = dataFrame[, 3] * shrink,
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
