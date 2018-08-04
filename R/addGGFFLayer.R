#' @import ggplot2
#' @import grDevices

#' @title Add FatFonts layers to a ggplot object
#'
#' @description addGGFFLayer() adds FatFonts layers to an existing ggplot object. It can be used to build FatFonts plots other than confusion matrix, heatmap, correlation matrix and clustered scatter plot.
#'
#' @usage
#' addGGFFLayer(oldPlot, data, xBinLength = 1, yBinLength = 1, font = 'Cubica',
#' fontSize = 3, fontColor = '#000000', adjustPos = 2, changeFontColValue = NULL,
#' changeFontColDigit = NULL, ...)
#'
#' @param oldPlot A ggplot object to which the FatFonts layers will be added
#' @param data A vector containing the data that needs to be represented by FatFonts. The vector must has the same length as the data in the original ggplot object does.
#' @param xBinLength The difference between each two neighboring numbers on the x axis. (default: 1)
#' @param yBinLength The difference between each two neighboring numbers on the y axis. (default: 1)
#' @param font The FatFonts family used in the plot. There are currently three FatFonts types available: "Cubica", "Miguta" and "Rotunda". (default: 'Cubica')
#' @param fontSize The font size of the smallest FatFonts digit. (default: 3)
#' @param fontColor The font color of FatFonts digits. (default: '#000000')
#' @param adjustPos Parameter used to slightly adjust the location of FatFonts digits. (default: 2)
#' @param changeFontColValue A function used to change font color based on value. An example function can be defined as follow: changeFontColValueFunc <- function(oldPlot){newPlot <- oldPlot + scale_color_gradient2(); return(newPlot)}. (default: NULL)
#' @param changeFontColDigit A vector containing the 9 colors used to represent each digit respectively (in the order of 1 to 9). You can also use color combinations provided by the system (e.g. topo.colors(9)). (default: NULL)
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @examples
#' # Library
#' library(ggplot2)
#' library(ggFatFonts)
#'
#' # Creat test dataset
#' test <- data.frame(var1 = c('a', 'a', 'b', 'b'),
#'                    var2 = c('a', 'c', 'a', 'd'),
#'                    fill = c(12, 53, 87, 6))
#'
#' # Create ggplot without FatFonts layer
#' myPlot <- ggplot(data = test, mapping = aes(x = var1, y = var2, fill = fill)) +
#'   geom_tile() +
#'   coord_fixed(ratio = 1, expand = FALSE)
#'
#' # Add FatFonts layer
#' myPlot <- addGGFFLayer(oldPlot = myPlot,
#'                        data = test[, 3],
#'                        xBinLength = 1,
#'                        yBinLength = 1,
#'                        font = 'Cubica',
#'                        fontSize = 3,
#'                        fontColor = '#FFFFFF',
#'                        adjustPos = 7.5)
#'
#' print(myPlot)
#'
#' @export
addGGFFLayer <- function(oldPlot,
                         data,
                         xBinLength = 1,
                         yBinLength = 1,
                         font = 'Cubica',
                         fontSize = 3,
                         fontColor = '#000000',
                         adjustPos = 2,
                         changeFontColValue = NULL,
                         changeFontColDigit = NULL,
                         ...){

  # changeFontColValue and changeFontColDigit cannot be used at the same time
  if ((is.null(changeFontColValue) == FALSE) &&
      (is.null(changeFontColDigit) == FALSE)) {
    stop("changeFontColValue and changeFontColDigit cannot be used at the same time")
  }

  # Check length
  if ((length(xBinLength) != 1) || (length(yBinLength) != 1) ||
      (length(font) != 1) || (length(fontSize) != 1) ||
      (length(fontColor) != 1) || (length(adjustPos) != 1)) {
    stop("'xBinLength', 'yBinLength', 'font' ,'fontSize', 'fontColor' and 'adjustPos' must be a length 1 vector.")
  }

  # Check font type
  availableFonts <- c('Cubica', 'Miguta', 'Rotunda')
  if (font %in% availableFonts != TRUE) {
    stop("The font type is currently not available. Please choose from 'Cubica', 'Miguta' and 'Rotunda'.")
  }

  # Import FatFonts
  windowsFonts(myFont = windowsFont(font))

  if (font == 'VizFonts-150917 even'){
    # Replace 0 with NaN
    for (i in 1:length(data)) {
      if (round(abs(data[i]), 0) == 0) {
        data[i] = NaN
      }
    }
    # When normal font is used
    newPlot <- oldPlot
    newPlot <- newPlot + geom_text(label = round(abs(data), 0),
                                   family = 'myFont',
                                   size = fontSize,
                                   position = position_nudge(x = 0, y = 0),
                                   color = fontColor)
  } else {
    # Transform data to FatFonts matrix
    newData <- transformNumToFFMatr(data = abs(data),
                                    font = font,
                                    fontSize = fontSize,
                                    xBinLength = xBinLength,
                                    yBinLength = yBinLength,
                                    adjustPos = adjustPos)

    # Find the highest place of the largest number to decide the level of FatFonts
    place <- (ncol(newData) - 1) / 3
    # Add the first layer of FatFonts
    shrink1 <- findShrink1Value(font)
    newPlot <- oldPlot
    if ((is.null(changeFontColValue) == TRUE) &&
        (is.null(changeFontColDigit) == TRUE)) {
      newPlot <- newPlot + geom_text(label = newData[, 2],
                                     family = 'myFont',
                                     size = fontSize * (shrink1 ^ (place - 1)),
                                     position = position_nudge(x = 0, y = 0.05),
                                     color = fontColor)
    } else if (is.null(changeFontColValue) == FALSE) {
      newPlot <- newPlot + geom_text(aes(color = data),
                                     label = newData[, 2],
                                     family = 'myFont',
                                     size = fontSize * (shrink1 ^ (place - 1)),
                                     position = position_nudge(x = 0, y = 0.05))
      newPlot <- changeFontColValue(newPlot)
    } else {
      newPlot <- newPlot + geom_text(aes(color = factor(newData[, 2])),
                                     label = newData[, 2],
                                     family = 'myFont',
                                     size = fontSize * (shrink1 ^ (place - 1)),
                                     position = position_nudge(x = 0, y = 0.05))
      cols <- c('1' = changeFontColDigit[1],
                '2' = changeFontColDigit[2],
                '3' = changeFontColDigit[3],
                '4' = changeFontColDigit[4],
                '5' = changeFontColDigit[5],
                '6' = changeFontColDigit[6],
                '7' = changeFontColDigit[7],
                '8' = changeFontColDigit[8],
                '9' = changeFontColDigit[9],
                'NaN' = 'transparent')
      newPlot <- newPlot + scale_color_manual(values = cols,
                                              limits = c('1', '2', '3',
                                                         '4', '5', '6',
                                                         '7', '8', '9'),
                                              name = 'font')
      newPlot <- newPlot + theme(legend.position = 'none')
    }
    # Add the other layers of FatFonts
    if (place >= 2){
      for (i in (place - 1):1) {
        for (j in 1:nrow(newData)) {
          tempData <- newData[, (place - i) * 3 + 2]
          tempData[-c(j)] <- rep(NaN, nrow(newData) - 1)
          if (is.nan(newData[j, (place - i) * 3 + 2]) == FALSE) {
            if ((is.null(changeFontColValue) == TRUE) &&
                (is.null(changeFontColDigit) == TRUE)){
              newPlot <- newPlot + geom_text(label = tempData,
                                             family = 'myFont',
                                             size = fontSize * (shrink1 ^ (i - 1)),
                                             position = position_nudge(x = newData[j, (place - i) * 3 + 3] * fontSize * fontSize,
                                                                       y = newData[j, (place - i) * 3 + 4] * fontSize * fontSize),
                                             color = fontColor)
            } else if (is.null(changeFontColValue) == FALSE) {
              newPlot <- newPlot + geom_text(aes(color = data),
                                             label = tempData,
                                             family = 'myFont',
                                             size = fontSize * (shrink1 ^ (i - 1)),
                                             position = position_nudge(x = newData[j, (place - i) * 3 + 3] * fontSize * fontSize,
                                                                       y = newData[j, (place - i) * 3 + 4] * fontSize * fontSize))
            } else {
              newPlot <- newPlot + geom_text(label = tempData,
                                             family = 'myFont',
                                             size = fontSize * (shrink1 ^ (i - 1)),
                                             color = switch (as.character(tempData[j]),
                                                             '1' = changeFontColDigit[1],
                                                             '2' = changeFontColDigit[2],
                                                             '3' = changeFontColDigit[3],
                                                             '4' = changeFontColDigit[4],
                                                             '5' = changeFontColDigit[5],
                                                             '6' = changeFontColDigit[6],
                                                             '7' = changeFontColDigit[7],
                                                             '8' = changeFontColDigit[8],
                                                             '9' = changeFontColDigit[9]
                                             ),
                                             position = position_nudge(x = newData[j, (place - i) * 3 + 3] * fontSize * fontSize,
                                                                       y = newData[j, (place - i) * 3 + 4] * fontSize * fontSize))
            }
          }
        }
      }
    }
  }

  return(newPlot)
}
