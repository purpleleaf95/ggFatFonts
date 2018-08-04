transformNumToFFMatr <- function(data,
                                 font,
                                 fontSize,
                                 xBinLength,
                                 yBinLength,
                                 adjustPos){

# Purpose:
#   Transform raw data into one data frame which can then be editted by this package
#
# Inputs:
#   data - the raw numbers
#   font - the FatFonts family used in the plot
#   fontSize - the font size of the smallest FatFonts digit
#   xBinLength - the bin length of the first group of data
#   yBinLength - the bin length of the second group of data
#   adjustPos - parameter used to slightly adjust the location of FatFonts digits
#
# Outputs:
#   A data frame that can then be editted by this package

  # Find the highest place of the largest number to decide the level of FatFonts
  data <- as.numeric(data)
  place <- 1
  max <- max(round(data, 0))
  while (floor(max / 10) > 0) {
    place <- place + 1
    max <- floor(max / 10)
  }

  # Initialize FatFonts Matrix
  temp <- round(data, 0)
  FFMatr <- data.frame(data = data,
                       d = floor(temp / 10 ^ (place - 1)),
                       x = rep(0, length(data)),
                       y = rep(0, length(data)))
  colnames(FFMatr) <- c('data',
                        paste('d', place, sep = ''),
                        paste('x', place, sep = ''),
                        paste('y', place, sep = ''))

  # Set transformation rules for each font type
  switchMiguta <- function (upLevelNumber,
                            xOriginal,
                            yOriginal,
                            xBinLength,
                            yBinLength,
                            shrink1,
                            adjustPos){
    location <- c(NA, NA)
    location[1] <- switch (as.character(upLevelNumber),
      '0' = 0,
      '1' = 983 / 566928,
      '2' = 983 / 566928,
      '3' = 983 / 566928,
      '4' = 983 / 566928,
      '5' = 983 / 566928,
      '6' = 983 / 566928,
      '7' = 73 / 40495,
      '8' = 71 / 40495,
      '9' = 0.1854902722
    )
    location[2] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = 0.005141746395,
                           '2' = 0.005141746395,
                           '3' = 0.004192772274,
                           '4' = 583 / 566928,
                           '5' = 583 / 566928,
                           '6' = 583 / 566928,
                           '7' = 583 / 566928,
                           '8' = 983 / 566928,
                           '9' = 983 / 566928
    )
    location[1] <- location[1] * (sqrt(xBinLength) / adjustPos) / shrink1 + xOriginal
    location[2] <- location[2] * (sqrt(yBinLength) / adjustPos) / shrink1 + yOriginal

    return(location)
  }

  switchCubica <- function (upLevelNumber,
                            xOriginal,
                            yOriginal,
                            xBinLength,
                            yBinLength,
                            shrink1,
                            adjustPos){
    location <- c(NA, NA)
    location[1] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = 0.2587137515,
                           '2' = 1167 / 56692,
                           '3' = -0.1805016581,
                           '4' = -4945 / 56692,
                           '5' = -2783 / 56596,
                           '6' = 367 / 56692,
                           '7' = 0.3438404008,
                           '8' = 367 / 56692,
                           '9' = 0
    )
    location[2] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = -0.2364778099,
                           '2' = -0.2265046215,
                           '3' = -0.2317963734,
                           '4' = 1853 / 14173,
                           '5' = -0.1824066888,
                           '6' = -0.1886862344,
                           '7' = -0.3446870811,
                           '8' = -0.2256226628,
                           '9' = 7959/56692
    )
    location[1] <- location[1] * (sqrt(xBinLength) / adjustPos) / shrink1 + xOriginal
    location[2] <- location[2] * (sqrt(yBinLength) / adjustPos) / shrink1 + yOriginal

    return(location)
  }

  switchGracilia <- function (upLevelNumber,
                              xOriginal,
                              yOriginal,
                              xBinLength,
                              yBinLength,
                              shrink1,
                              adjustPos){
    location <- c(NA, NA)
    location[1] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = 9559 / 38066,
                           '2' = 393 / 1433,
                           '3' = -367 / 36740,
                           '4' = 2431 / 37228,
                           '5' = 347 / 19384,
                           '6' = 1005 / 36982,
                           '7' = 9213 / 38158,
                           '8' = 0,
                           '9' = -127 / 4747
    )
    location[2] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = -0.3217044091,
                           '2' = -4063 / 13350,
                           '3' = -1796 / 8889,
                           '4' = 183 / 52048,
                           '5' = -8019 / 53402,
                           '6' = -4546 / 26597,
                           '7' = -0.3244353568,
                           '8' = -3403 / 17742,
                           '9' = 8825 / 53328
    )
    location[1] <- location[1] * (sqrt(xBinLength) / adjustPos) / shrink1 + xOriginal
    location[2] <- location[2] * (sqrt(yBinLength) / adjustPos) / shrink1 + yOriginal

    return(location)
  }

  switchRutunda <- function (upLevelNumber,
                             xOriginal,
                             yOriginal,
                             xBinLength,
                             yBinLength,
                             shrink1,
                             adjustPos){
    location <- c(NA, NA)
    location[1] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = 1281 / 14174,
                           '2' = 448 / 14173,
                           '3' = 7 / 974,
                           '4' = -3883 / 28265,
                           '5' = 200 / 28347,
                           '6' = 2393 / 56692,
                           '7' = -8023 / 28346,
                           '8' = -3 / 2096,
                           '9' = -841 / 56792
    )
    location[2] <- switch (as.character(upLevelNumber),
                           '0' = 0,
                           '1' = -1815 / 7087,
                           '2' = -9677 / 56692,
                           '3' = -5973 / 28246,
                           '4' = 4026 / 28265,
                           '5' = -116 / 859,
                           '6' = -2003 / 14173,
                           '7' = -5081 / 56692,
                           '8' = -9071 / 56592,
                           '9' = 8573 / 56692
    )
    location[1] <- location[1] * (sqrt(xBinLength) / adjustPos) / shrink1 + xOriginal
    location[2] <- location[2] * (sqrt(yBinLength) / adjustPos) / shrink1 + yOriginal

    return(location)
  }

  switchGannet <- function (upLevelNumber,
                            xOriginal,
                            yOriginal,
                            xBinLength,
                            yBinLength,
                            shrink1,
                            adjustPos){
    location <- c(NA, NA)
    location[1] <- (sqrt(10) - 1) / (2 * sqrt(10))
    location[2] <- -(sqrt(10) - 1) / (2 * sqrt(10))
    location[1] <- location[1] * (sqrt(xBinLength) / adjustPos) / shrink1 + xOriginal
    location[2] <- location[2] * (sqrt(yBinLength) / adjustPos) / shrink1 + yOriginal

    return(location)
  }

  if (place >= 2){
    # Complete FatFonts Matrix
    for (i in (place - 1):1){
      # Create digit column
      temp <- round(temp - floor(temp / (10 ^ i)) * (10 ^ i), 0)
      FFMatr <- cbind(FFMatr, floor(temp / 10 ^ (i - 1)))
      colnames(FFMatr)[length(colnames(FFMatr))] <- paste('d', i, sep = '')
      # Create empty x and y position columns
      FFMatr <- cbind(FFMatr, rep(NA, nrow(FFMatr)))
      colnames(FFMatr)[length(colnames(FFMatr))] <- paste('x', i, sep = '')
      FFMatr <- cbind(FFMatr, rep(NA, nrow(FFMatr)))
      colnames(FFMatr)[length(colnames(FFMatr))] <- paste('y', i, sep = '')
      # Find shrink1
      shrink1 <- findShrink1Value(font)
      # Fill x and y position columns
      for (j in 1:nrow(FFMatr)) {
        FFMatr[j, c(ncol(FFMatr) - 1, ncol(FFMatr))] <-
          switch (font,
                  'Miguta' = switchMiguta(upLevelNumber = FFMatr[j, ncol(FFMatr) - 5],
                                          xOriginal = FFMatr[j, ncol(FFMatr) - 4],
                                          yOriginal = FFMatr[j, ncol(FFMatr) - 3],
                                          xBinLength = xBinLength,
                                          yBinLength = yBinLength,
                                          shrink1 = fontSize * (shrink1 ^ (place - i - 1)),
                                          adjustPos = adjustPos),
                  'Cubica' = switchCubica(upLevelNumber = FFMatr[j, ncol(FFMatr) - 5],
                                          xOriginal = FFMatr[j, ncol(FFMatr) - 4],
                                          yOriginal = FFMatr[j, ncol(FFMatr) - 3],
                                          xBinLength = xBinLength,
                                          yBinLength = yBinLength,
                                          shrink1 = fontSize * (shrink1 ^ (place - i - 1)),
                                          adjustPos = adjustPos),
                  'Gracilia' = switchCubica(upLevelNumber = FFMatr[j, ncol(FFMatr) - 5],
                                            xOriginal = FFMatr[j, ncol(FFMatr) - 4],
                                            yOriginal = FFMatr[j, ncol(FFMatr) - 3],
                                            xBinLength = xBinLength,
                                            yBinLength = yBinLength,
                                            shrink1 = fontSize * (shrink1 ^ (place - i - 1)),
                                            adjustPos = adjustPos),
                  'Rotunda' = switchCubica(upLevelNumber = FFMatr[j, ncol(FFMatr) - 5],
                                           xOriginal = FFMatr[j, ncol(FFMatr) - 4],
                                           yOriginal = FFMatr[j, ncol(FFMatr) - 3],
                                           xBinLength = xBinLength,
                                           yBinLength = yBinLength,
                                           shrink1 = fontSize * (shrink1 ^ (place - i - 1)),
                                           adjustPos = adjustPos),
                  'VizFont - Gannet' = switchGannet(upLevelNumber = FFMatr[j, ncol(FFMatr) - 5],
                                                    xOriginal = FFMatr[j, ncol(FFMatr) - 4],
                                                    yOriginal = FFMatr[j, ncol(FFMatr) - 3],
                                                    xBinLength = xBinLength,
                                                    yBinLength = yBinLength,
                                                    shrink1 = fontSize * (shrink1 ^ (place - i - 1)),
                                                    adjustPos = adjustPos)
          )
      }
    }
  }

  # Change 0 to NaN
  for (i in 1:nrow(FFMatr)) {
    for (j in seq(from = 2, to = (place * 3 - 1), by = 3)) {
      if (FFMatr[i, j] == 0) {
        FFMatr[i, j] = NaN
      }
    }
  }

  return(FFMatr)
}
