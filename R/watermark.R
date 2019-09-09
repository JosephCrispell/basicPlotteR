# Tutorials
#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#http://r-pkgs.had.co.nz/description.html
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html

## Packages to install
#install.packages("devtools")
#install.packages("digest")
#devtools::install_github("klutometis/roxygen")

## Packages to load
#library("devtools")
#library("roxygen2")

## Creating package
#packageDirectory <- "/home/josephcrispell/Desktop/Research/basicPlotteR/"
#usethis::create_package(packageDirectory)

## Documenting changes
#setwd(packageDirectory)
#document()

## Install
#setwd("..")
#install("basicPlotteR")

#' Add a watermark to an existing plot
#'
#' This function should be run following a plot. It will add a transparent text label to act as a watermark.
#' @param text a character vector specifying the text to be written
#' @param location an optional character vector specifying the location for the text to be plotted
#' @param x optional x co-ordinate to be used to position the text
#' @param y optional y co-ordinate to be used to position the text
#' @param col the colour of the text to be plotted. Defaults to black.
#' @param alpha the transparency (0=translucent and 1=opaque) of the text to be plotted. Defaults to 0.5.
#' @param srt the number of degrees from horizontal to set the angle of the text
#' @param cex numeric character expansion factor. By default, text will be stretched to fit plotting window.
#' @keywords watermark text plot
#' @export
#' @examples
#' # Generate some example data
#' x <- runif(10000)
#' y <- runif(10000)
#' 
#' # Plot the points
#' plot(x, y, las=1, bty="n", pch=19, col=rgb(0,0,0, 0.1))
#' 
#' # Add a watermark
#' watermark("CONFIDENTIAL", col="red")
watermark <- function(text, location=NULL, x=NULL, y=NULL, col="black", alpha=0.5, srt=45, cex=1,
                      ...){
  
  # Get the axis limits of the current plot
  axisLimits <- par("usr")
  
  # Get the label location
  coords <- c(x, y)
  
  # If no coordinates then calculate location based on a string (if no strin, defaults to centre)
  if(is.null(x)){
    coords <- calculateLocation(location, axisLimits)
  }
  
  # Calculate the cex of the watermark to fill the plot
  watermarkCex <- calculateWatermarkCex(text, axisLimits) * cex
  
  # Set the alpha of the watermark colour
  col <- basicPlotteR::setAlpha(col, alpha)
  
  # Plot the water mark
  text(x=coords[1], y=coords[2], labels=text, col=col, cex=watermarkCex, srt=srt, ...)
}

#' Calculate the factor to multiple text by so that it fits to plotting window
#'
#' Function used by \code{watermark()}
#' @param text a character vector specifying the text to be written
#' @param axisLimits a numeric vector storing the limits of the axes in the format \code{c(xMin, xMax, yMin, yMax)}.
#' @keywords internal
#' @return Returns a numeric value to multiple the size of the text to be plotted.
calculateWatermarkCex <- function(text, axisLimits){
  
  # Note the number of characters in the text
  nCharacters <- nchar(text)
  
  # Calculate the width of a character in the current plot
  characterWidth <- strwidth(text) / nCharacters
  
  # Calculate the text width
  textWidth <- characterWidth * nCharacters
  
  # Note the wdith of the X axis
  xAxisLength <- axisLimits[2] - axisLimits[1]
  
  # Calculate the factor to multiple the text to make it fit the plotting window
  return(xAxisLength / textWidth)
}

#' Get/calculate the location of the text to be plotted. Either as centre of plotting window or specified text location.
#'
#' Function used by \code{watermark()}
#' @param location a character vector specifying the location for the text to be plotted
#' @param axisLimits a numeric vector storing the limits of the axes in the format \code{c(xMin, xMax, yMin, yMax)}.
#' @keywords internal
#' @return Returns a numeric vector containing the X and Y coordinates to plot the text at
calculateLocation <- function(location=NULL, axisLimits){
  
  # Initialise a vector to store the coordinates of the location
  coords <- c(NA, NA)
  
  # Calculate the length of each axis
  xLength <- axisLimits[2] - axisLimits[1]
  yLength <- axisLimits[4] - axisLimits[3]
  
  # Calculate the centre if no location provided as a default
  coords[1] <- axisLimits[1] + (xLength*0.5)
  coords[2] <- axisLimits[3] + (yLength*0.5)
  
  # Check if location text given
  if(is.null(location) == FALSE){
    
    # Check if location is one of the options
    if(location %in% c("top", "bottom", "middle", "left", "right",
                       "topleft", "topright",
                       "bottomleft", "bottomright") == FALSE){
      location <- NULL
      stop("Location string not recognised. Please try one of: c(\"top\", \"bottom\", \"middle\",", 
           "\"left\", \"right\", \"topleft\", \"topright\", \"bottomleft\", \"bottomright\")")
    }
    
    # Define the location using the string code
    if(grepl(location, pattern="top")){
      coords[2] <- axisLimits[4]
    }
    if(grepl(location, pattern="left")){
      coords[1] <- axisLimits[1]
    }
    if(grepl(location, pattern="right")){
      coords[1] <- axisLimits[2]
    }
    if(grepl(location, pattern="bottom")){
      coords[2] <- axisLimits[3]
    }
  }
  
  return(coords)
}

