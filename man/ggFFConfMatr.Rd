% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggFFConfMatr.R
\name{ggFFConfMatr}
\alias{ggFFConfMatr}
\title{Confusion Matrix using GGPlot2 and FatFonts}
\usage{
ggFFConfMatr(actuClass, predClass, tileBorderColor = '#C2C2C2', tileBorderSize = 0.1,
tileFill = '#FFFFFF', shrink = -1, font = 'Cubica', fontSize = -1,
fontColor = '#000000', isSquare = TRUE, adjustPos = 3, changeFontColValue = NULL,
changeFontColDigit = NULL, changeBackCol = NULL, ...)
}
\arguments{
\item{actuClass}{A vector containing the actual values. actuClass and predClass should be of the same length.}

\item{predClass}{A vector containing the predicted values. actuClass and predClass should be of the same length.}

\item{tileBorderColor}{The color of the border of the tiles. (default: '#C2C2C2')}

\item{tileBorderSize}{The size of the border of the tiles. (default: 0.1)}

\item{tileFill}{The fill color of the tiles. (default: '#FFFFFF')}

\item{shrink}{To what extent are the raw values reduced. For instance, if shrink = 1000 and the original data is D, then the values displayed by FatFonts will be 1000D. If shrink = -1, then shrink will be chosen automatically. (default: -1)}

\item{font}{The FatFonts family used in the plot. There are currently three FatFonts types available: "Cubica", "Miguta" and "Rotunda". (default: 'Cubica')}

\item{fontSize}{The font size of the smallest FatFonts digit. If fontSize = -1, then font size will be chosen automatically. (default: -1)}

\item{fontColor}{The font color of FatFonts digits. (default: '#000000')}

\item{isSquare}{If each grid is square. It is better to display FatFonts numbers in square rather than rectangle grids. (default: TRUE)}

\item{adjustPos}{Parameter used to slightly adjust the location of FatFonts digits. (default: 3)}

\item{changeFontColValue}{A function used to change font color based on value. An example function can be defined as follow: changeFontColValueFunc <- function(oldPlot){newPlot <- oldPlot + scale_color_gradient2(); return(newPlot)}. (default: NULL)}

\item{changeFontColDigit}{A vector containing the 9 colors used to represent each digit respectively (in the order of 1 to 9). You can also use color combinations provided by the system (e.g. topo.colors(9)). (default: NULL)}

\item{changeBackCol}{A function used to change tile fill color based on value. An example function can be defined as follow: changeBackColFunc <- function(oldPlot){newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple'); return(newPlot)}. (default: NULL)}

\item{...}{Other arguments passed on to methods. Not currently used.}
}
\description{
ggFFConfMatr() produces Confusion Matrix using GGPlot2 and FatFonts.
}
\examples{
ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`)

ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
 adjustPos = 12)

ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
 adjustPos = 12, fontSize = 5)

ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
 adjustPos = 13, font = 'Rotunda', fontSize = 5)

ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
 adjustPos = 4, shrink = 1000)

ggFFConfMatr(confMatrTestData2$TRUE., confMatrTestData2$predicted,
 adjustPos = 16, isSquare = FALSE)

ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
 fontColor = 'blue')

changeBackColFunc <- function(oldPlot){
  newPlot <- oldPlot + scale_fill_gradient(low = 'white', high = 'purple')
  return(newPlot)
}
ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
 changeBackCol = changeBackColFunc)

changeFontColValueFunc <- function(oldPlot){
  newPlot <- oldPlot + scale_color_continuous(low = 'dark grey', high = 'purple')
 return(newPlot)
}
ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
 changeFontColValue = changeFontColValueFunc)

cols <- topo.colors(9)
ggFFConfMatr(confMatrTestData$`actual value`, confMatrTestData$`predicted value`,
 changeFontColDigit = cols)

}
