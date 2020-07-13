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

#' A progress bar function to run within a \code{for} loop
#'
#' A function to change the alpha value (transparency) of colours that are defined as strings.
#' @param colours A vector of colours defined by strings of characters
#' @param alpha The transparency (0=transparent, 1=opaque)
#' @keywords colour alpha transparency
#' @export
#' @examples
#' # Define the coordinates of three points
#' x <- c(0.2, 0.5, 0.8)
#' y <- c(0.5, 0.5, 0.5)
#' 
#' # Define the colours for the points
#' colours <- c("green", "blue", "red")
#' 
#' # Plot the points - set the transparency
#' plot(x, y, bty="n", xlab="X", ylab="Y", las=1, pch=19, cex=30, xlim=c(0,1), ylim=c(0,1),
#'      col=setAlpha(colours, alpha=0.5))
setAlpha <- function(colours, alphas){
  
  # Check alpha value is from 0 to 1
  if(length(which(alphas < 0)) > 0 || ength(which(alphas < 0)) > 0){
    stop("The alpha value must be from 0 to 1.")
  }
  
  # Check enough alpha values were provided
  if(length(alphas) < length(colours)){
    alphas <- rep(alphas, ceiling(length(colours)/length(alphas)))
  }
  
  # Create a vector to store the output colours
  output <- c()
  
  # Examine each colour provided
  for(index in seq_along(colours)){
    
    # Convert the current colour into rgb values
    rgbValues <- col2rgb(colours[index])
    
    # Place rgb values within rgb function and insert alpha value
    # Note that col2rgb returns rgbvlues from 0 to 255
    output[index] <- rgb(rgbValues["red", 1], rgbValues["green", 1], rgbValues["blue", 1],
                         alpha=alphas[index]*255, maxColorValue=255)
  }

  return(output)
}
