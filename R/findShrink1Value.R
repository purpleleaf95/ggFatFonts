findShrink1Value <- function(font){

# Purpose:
#   Set shrink1 value
#
# Inputs:
#   font - the FatFonts family used in the plot
#
# Outputs:
#   The shrink1 value

  # Check length
  if (length(font) != 1) {
    stop("'font' must be a length 1 vector.")
  }

  shrink1 <- switch (font,
                     'Miguta' = 2.857125578,
                     'Cubica' = 3.225901901,
                     'Gracilia' = 3.033545025,
                     'Rotunda' = 3.164545658,
                     'VizFont - Gannet' = sqrt(10),
                     'VizFonts-150917 even' = 1
  )

  return(shrink1)
}
