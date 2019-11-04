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

#' Plot multiple histograms in the same plot
#'
#' This function uses the hist function to plot multiple distributions in the same plot
#' @param distributions A list containing multiple distributions - each represented as a numeric vector
#' @param nBins The number of bins to be used in the plot. Defaults to 10
#' @param colours A vector of length \code{length(distributions)} containing the colour of each distribution plotted
#' @param ... Arguments to be passed to \code{plot()} function
#' @keywords histogram multiple
#' @export
#' @examples
#' # Create two random samples from a normal distribution
#' distributions <- list(rnorm(500, mean=5, sd=0.5), 
#'                       rnorm(500, mean=8, sd=5), 
#'                       rnorm(500, mean=20, sd=2))
#' 
#' # Plot overlapping histograms
#' plotMultipleHistograms(distributions, nBins=20, colours=c(rgb(1,0,0, 0.5), rgb(0,0,1, 0.5), rgb(0,1,0, 0.5)), las=1)
plotMultipleHistograms <- function(distributions, nBins=10, colours, ...){

  # Check for NA values and remove
  distributions <- removeNAValues(distributions)
  
  # Calculate the limits of the X axis
  xLimits <- range(unlist(distributions))
  
  # Define the bins for the histogram
  breaks <- defineBreaks(xLimits, nBins)
  
  # Create a histogram object for each distribution
  histograms <- createHistogramObjects(distributions, breaks)
  
  # Calculate the y axis limits
  yLimits <- calculateYAxisLimits(histograms)
  
  # Plot the histograms
  plot(histograms[[1]], col=colours[1], ylim=yLimits, ...)
  for(i in 2:length(histograms)){
    plot(histograms[[i]], col=colours[i], add=TRUE)
  }
}

#' Remove \code{NA} values from each distribution
#'
#' Function used by \code{plotMultipleHistograms()}
#' @param distributions A list containing multiple distributions - each represented as a numeric vector
#' @keywords internal
#' @return Returns a list containing multiple distributions without any NA values
removeNAValues <- function(distributions){
  
  # Examine each of the distributions
  for(index in seq_along(distributions)){
    
    # Get the values from the current distribution
    values <- distributions[[index]]
    
    # Remove NA values
    values <- values[is.na(values) == FALSE]
    
    # Overwrite the distribution
    distributions[[index]] <- values
  }
  
  return(distributions)
}

#' Define the breaks to be used in histograms
#'
#' Function used by \code{plotMultipleHistograms()}
#' @param xLimits A vector of length 2 representing the range of the distributions
#' @param nBins A numeric value indicating the number of bins to define
#' @keywords internal
#' @return Returns a vector defining the start and end of bins
defineBreaks <- function(xLimits, nBins){
  
  # Define the bin size based upon the number of bins required
  binSize <- (xLimits[2] - xLimits[1])/nBins
  
  # Define the breaks using a sequence of numbers from the minimum x axis limit to the maximum
  breaks <- seq(from=xLimits[1], to=xLimits[2], by=binSize)
  
  # Check that breaks includes last
  if(xLimits[2] %in% breaks == FALSE){
    breaks[length(breaks) + 1] <- breaks[length(breaks)] + binSize
  }
  
  return(breaks)
}

#' Create, but don't plot, a histogram for each distribution
#'
#' Function used by \code{plotMultipleHistograms()}
#' @param distributions A list containing multiple distributions - each represented as a numeric vector
#' @param breaks A vector defining the start and end of bins
#' @keywords internal
#' @return Returns a list of histogram objects
createHistogramObjects <- function(distributions, breaks){
  
  # Create a list to store the histograms
  output <- list()
  
  # Create a histogram for each distribution
  for(distribution in distributions){

    output[[length(output) + 1]] <- hist(distribution, breaks=breaks, plot=FALSE)
  }
  
  return(output)
}

#' Calculate the limits of the Y axis
#'
#' Function used by \code{plotMultipleHistograms()}
#' @param histograms A list of histogram objects
#' @keywords internal
#' @return A vector of length 2 containing 0 and the maximum bin count observed across the histograms
calculateYAxisLimits <- function(histograms){
  
  # Initialise a vector to store the bin counts from each histogram
  counts <- c()
  
  # Find the maximum height of bats in the histogram
  for(histogram in histograms){
    
    counts <- c(counts, histogram$counts)
  }

  # Set the yLimits
  yLimits <- c(0, max(counts))
  
  return(yLimits)
}
