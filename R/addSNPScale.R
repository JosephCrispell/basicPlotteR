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
#' Add a SNP scale to phylogeny
#'
#' Function that adds a SNP scale onto a plotted phylogeny
#' @param x An optional numeric X coordinate for the scale
#' @param y An optional numeric Y coordinate for the scale
#' @param postion An optional character vector detailing the location of the plot (top, middle, left, right, etc.)
#' @param lineWidth A numeric scaling factor to change the width of the scale line. Defaults to 1. Equivalent to \code{lwd}
#' @param cex A numeric scaling factor to chancge the size of the scale label. Defautls to 1.
#' @keywords phylogeny scale nucleotide snp
#' @export
addSNPScale <- function(x=NULL, y=NULL, position=NULL, size=1, lineWidth=1, cex=1){
  
  # Get the axis limits of the current plot
  axisLimits <- par("usr")
  
  # Get the label location
  coords <- c(x, y)
  
  # If no coordinates then calculate location based on a string (if no strin, defaults to centre)
  if(is.null(x)){
    coords <- calculateLocation(position, axisLimits)
    x <- coords[1]
    y <- coords[2]
  }
  
  # Add a scale
  lines(x=c(x-(0.5*size), x+(0.5*size)), y=c(y, y), lwd=lineWidth, xpd=TRUE)
  text(x=x, y=y, labels=paste(size, ifelse(size > 1, "SNPs", "SNP")), pos=1, xpd=TRUE, cex=cex)
}

#' Get the X and Y coordinates of a string coded location on a plot
#'
#' Function used by \code{addSNPScale()}
#' @param location A character string indicating a location on a plot
#' @keywords internal
#' @return Returns a numeric vector storing the coordinates determined by string location
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
      coords[2] <- axisLimits[4] - 0.1*yLength
    }
    if(grepl(location, pattern="left")){
      coords[1] <- axisLimits[1] + 0.1*xLength
    }
    if(grepl(location, pattern="right")){
      coords[1] <- axisLimits[2] - 0.1*xLength
    }
    if(grepl(location, pattern="bottom")){
      coords[2] <- axisLimits[3] + 0.1*yLength
    }
  }
  
  return(coords)
}