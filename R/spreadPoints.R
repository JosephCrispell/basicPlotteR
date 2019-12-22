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

#' Add spreaded, to avoid overlap, points to a boxplot
#'
#' This function is similar to \code{stripchart()} function except it spreads points along an axis in a deterministic rather than random manner
#' @param values A vector of numerical values
#' @param position A numerical value indicating the position on the x-axis where values are to be plotted. Boxplots use integers: 1,2,3,...
#' @param pointCex A numerical value giving the amount by which plotted symbols should be magnified relative to their default. Default value is 1
#' @param col The colour of the points to be plotted. Defaults to black
#' @param pch The shape of the points to be plotted
#' @param alpha The transparency (0=transparent, 1=opaque). Default value is 0.5
#' @param plotBins Boolean parameter indicating whether the bins used to spread to points should be plotted as horizontal lines
#' @param plotOutliers Boolean parameter indicating whether to plot outliers. Outliers are defined as those outside \code{range} * InterQuartileRange. Defaults to FALSE
#' @param range Numerical value used to determine outliers. Default value is 1.5 - same as used by boxplot function
#' @param horiz Boolean parameter indicating whether boxplot was plotted horizontally. Default value is FALSE
#' @param fitToBoxWidth Boolean parameter indicating whether the points are to spread only within the width of the box. Default value is TRUE
#' @param xpd A Boolean value or NA. If FALSE, all plotting is clipped to the plot region, if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region
#' @param widthCex A numerical value giving the amount by which amount the points are spread out should be magnified relative to their default. Default value is 1
#' @keywords points spread boxplot
#' @export
#' @examples
#' # Generate some example points - drawn from exponential distribution
#' values <- rexp(n=50, rate=2)
#'
#' # Plot a boxplot
#' boxplot(values, xlab="",  ylab="", frame=FALSE, las=1, pch=19, outcol=rgb(1,0,0, 0.5),
#'         horizontal=FALSE)
#'
#' # Plot the points spread along the X axis
#' spreadPoints(values, position=1)
spreadPoints <- function(values, position, pointCex=1, col="black", pch=19, alpha=0.5, plotBins=FALSE, plotOutliers=FALSE,
                         range=1.5, horiz=FALSE, fitToBoxWidth=TRUE, xpd=FALSE, widthCex=1){

  # Check if NA values are present
  indicesOfNAs <- which(is.na(values))
  if(length(indicesOfNAs) != 0){
    
    # Remove the NA values
    values <- values[-indicesOfNAs]
    
    # Print warning about the presence of NA values
    warning(paste0("NA values are present (n = ", length(indicesOfNAs), "). These are ignored for plotting."))
  }
  
  # Calculate the point size
  ptSize <- calculatePointShapeSize(cex=pointCex)

  # Get the whisker thresholds used for the boxplot
  whiskerInfo <- identifyWhiskersAndOutliers(values, range)

  # If not plotting outliers - remove them
  if(plotOutliers == FALSE){
    values <- values[values <= whiskerInfo$Upper[1] & values >= whiskerInfo$Lower[1]]
  }

  # Set the colour alpha
  colour <- basicPlotteR::setAlpha(col, alpha)

  # Get the aixs limits
  axisLimits <- par("usr")

  # Check if multiple values available to define bins
  bins <- as.factor(rep("SameBin", length(values)))
  if(length(unique(values)) > 1){
    
    # Assign each of the values into a bin - the number of bins is defined by the yPad
    bins <- cut(values, breaks = (axisLimits[4] - axisLimits[3])/ptSize[2])
    if(horiz){
      bins <- cut(values, breaks = (axisLimits[2] - axisLimits[1])/ptSize[2])
    }
  }
  
  # Identify the indices of the values that are present in each bin
  indicesOfValuesInBins <- identifyValuesInBins(bins)

  # Calculate number of boxplots in current plot
  nBoxes <- floor(axisLimits[2])
  if(horiz){
    nBoxes <- floor(axisLimits[4])
  }

  # Examine each bin
  for(key in names(indicesOfValuesInBins)){

    # Check if more than one point present
    if(length(indicesOfValuesInBins[[key]]) > 1){

      # Note the number of points
      nPoints <- length(indicesOfValuesInBins[[key]])

      # Calculate the space available for each point
      # 0.8 is for when multiple plots are available - boxplot width is 80% of space available
      # 0.4 seems to work better for a single box - I have no idea why??
      spaceAvailable <- min(ptSize[1], (widthCex*0.8)/nPoints)
      if(nBoxes == 1){
        spaceAvailable <- min(ptSize[1], (widthCex*0.4)/nPoints)
      }
      if(fitToBoxWidth == FALSE){
        spaceAvailable <- ptSize[1]
      }

      # Define new X positions of each point      <-.->
      xPositions <- seq(from=position - (0.5*nPoints*spaceAvailable),
                        to=position + (0.5*nPoints*spaceAvailable),
                        length.out=nPoints)

      # Plot the points - check if horizontal plotting
      if(horiz){
        points(x=values[indicesOfValuesInBins[[key]]], y=xPositions, col=colour, xpd=xpd, cex=pointCex, pch=pch)
      }else{
        points(x=xPositions, y=values[indicesOfValuesInBins[[key]]], col=colour, xpd=xpd, cex=pointCex, pch=pch)
      }

      # Check if single point present
    }else if(length(indicesOfValuesInBins[[key]]) == 1){

      # Plot the point - check if horizontal plotting
      if(horiz){
        points(x=values[indicesOfValuesInBins[[key]][1]], y=position, col=colour, xpd=xpd, cex=pointCex, pch=pch)
      }else{
        points(x=position, y=values[indicesOfValuesInBins[[key]][1]], col=colour, xpd=xpd, cex=pointCex, pch=pch)
      }
    }
  }

  # Plot the bins if requested - for testing
  if(plotBins){
    plotBinBoundaries(bins, col="green")
  }
}

#' Add spreaded, to avoid overlap, points to a boxplot containing multiple boxes drawn using data from dataframe
#'
#' This function is similar to \code{stripchart()} function except it spreads points along an axis in a deterministic rather than random manner
#' @param data A dataframe object containing the data to be plotted
#' @param responseColumn The name or index of the response data (Y)
#' @param categoriesColumn The name or index of the column that categorises the response data
#' @param pointCex A numerical value giving the amount by which plotted symbols should be magnified relative to their default. Default value is 1
#' @param col The colour of the points to be plotted. Defaults to black
#' @param pch The shape of the points to be plotted
#' @param alpha The transparency (0=transparent, 1=opaque). Default value is 0.5
#' @param plotBins Boolean parameter indicating whether the bins used to spread to points should be plotted as horizontal lines
#' @param plotOutliers Boolean parameter indicating whether to plot outliers. Outliers are defined as those outside \code{range} * InterQuartileRange. Defaults to FALSE
#' @param range Numerical value used to determine outliers. Default value is 1.5 - same as used by boxplot function
#' @param horiz Boolean parameter indicating whether boxplot was plotted horizontally. Default value is FALSE
#' @param fitToBoxWidth Boolean parameter indicating whether the points are to spread only within the width of the box. Default value is TRUE
#' @param xpd A Boolean value or NA. If FALSE, all plotting is clipped to the plot region, if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region
#' @param widthCex A numerical value giving the amount by which amount the points are spread out should be magnified relative to their default. Default value is 1
#' @keywords points spread boxplot
#' @export
#' @examples
#' # Generate some example points - drawn from normal distribution and randomly assign them to categories
#' randomSamples <- data.frame(Values = rnorm(500), Category = sample(c('A', 'B', 'C', 'D', 'E'), size=500, replace=TRUE))
#'
#' # Plot a boxplot of the samples from the normal distribution versus there categories - multiple boxplots
#' boxplot(Values ~ Category, data = randomSamples, lwd = 2)
#'
#' # Plot the points for each category spread along the X axis
#' spreadPointsMultiple(data=randomSamples, responseColumn="Values", categoriesColumn="Category")
spreadPointsMultiple <- function(data, responseColumn, categoriesColumn, pointCex=1, col="black", pch=19, alpha=0.5, plotBins=FALSE,
                                 plotOutliers=FALSE, range=1.5, horiz=FALSE, fitToBoxWidth=TRUE, xpd=FALSE, widthCex=1){

  # Check if categories column is a factor
  if(is.factor(data[, categoriesColumn]) == FALSE){
    data[, categoriesColumn] <- as.factor(data[, categoriesColumn])
  }
  
  # Get the unique categories in the categories column
  categories <- levels(data[, categoriesColumn])
  
  # Examine each of the categories within column provided
  for(position in seq_along(categories)){

    # Check any values for the current category exist
    if(categories[position] %in% data[, categoriesColumn] == FALSE){
      warning("No data available for factor level:", categories[position])
      next
    }
    
    # Define the colour
    colour <- setOption(options=col, index=position)
    
    # Define the values
    values <- data[data[, categoriesColumn] == categories[position], responseColumn]

    # Plot the points over the current boxplot
    spreadPoints(values, position=position, pointCex, colour, pch, alpha, plotBins, plotOutliers,
                 range, horiz, fitToBoxWidth, xpd, widthCex)
  }
}

#' A function to identify the locations of the whiskers in a boxplot and identify outlier values outside the whiskers
#'
#' Function used by \code{spreadPoints()}
#' @param values A vector of numerical values
#' @param range Numerical value used to determine outliers. Outliers are defined as those outside \code{range} * InterQuartileRange
#' @keywords internal
#' @return Returns a list containing two vectors: "Upper" and "Lower". THe first value in these vectors is the whisker location. The remaining values are outliers.
identifyWhiskersAndOutliers <- function(values, range){

  # Calculate the quartiles
  quartiles <- quantile(values)

  # Initialise a vector to store the whikser positions and outliers
  whiskerInfo <- list("Upper"=c(max(values)), "Lower"=c(min(values)))

  # Calculate the inter-quartile-range
  interQuartileRange <- quartiles[4] - quartiles[2]

  # Check if there are values above the 4th quartile + interquatilerange * range
  if(whiskerInfo$Upper[1] > quartiles[4] + (range * interQuartileRange)){

    # Sort the input values
    sortedValues <- sort(values, decreasing=TRUE)

    # Examine each of the sorted values
    for(i in seq_along(sortedValues)){

      # If found the first value that is less than or equal to the 4th quartile + range * interquartilerange
      if(sortedValues[i] <= quartiles[4] + (range * interQuartileRange)){
        whiskerInfo$Upper <- c(sortedValues[i], sortedValues[seq_len(i-1)])
        break
      }
    }
  }

  # Check if there are values below the 2nd quartile - interquatilerange * range
  if(whiskerInfo$Lower[1] < quartiles[2] - (range * interQuartileRange)){

    # Sort the input values
    sortedValues <- sort(values, decreasing=FALSE)

    # Examine each of the sorted values
    for(i in seq_along(sortedValues)){

      # If found the first value that is less than or equal to the 4th quartile + range * interquartilerange
      if(sortedValues[i] >= quartiles[2] - (range * interQuartileRange)){
        whiskerInfo$Lower <- c(sortedValues[i], sortedValues[seq_len(i-1)])
        break
      }
    }
  }

  return(whiskerInfo)
}

#' A function used in development to see how well the size of a point shape was calculated.
#'
#' Function used in development by \code{spreadPoints()}
#' @param ptSize A numerical vector containing the estimated width and height of a point shape
#' @param coords A numerical vector containing the X and Y coordinates to plot a point
#' @keywords internal
testPchSizeCalculator <- function(ptSize, coords){

  # Plot a point
  points(x=coords[1], y=coords[2], pch=19, col="black")

  # Plot horizontal line to show calculated width
  points(x=c(coords[1] - (0.5*ptSize[1]),
             coords[1] + (0.5*ptSize[1])),
         y=c(coords[2], coords[2]), type="l", col="green")

  # Plot vertical line to show calculated height
  points(x=c(coords[1], coords[1]), type="l", col="green",
         y=c(coords[2] - (0.5*ptSize[2]),
             coords[2] + (0.5*ptSize[2])))
}

#' A function used in development to see the size of the bins used to spread points that fall within them
#'
#' Function used in development by \code{spreadPoints()}
#' @param bins A categorical vector detailing the start and end of each bin each value fell in Defined by \code{cut()} function in \code{spreadPoints()}
#' @param col The colour of the horizontal lines, which define the bin start and ends, to be plotted
#' @keywords internal
plotBinBoundaries <- function(bins, col){

  # Get the bin definitions
  binDefinitions <- levels(bins)

  # Examine each bin
  for(definition in binDefinitions){

    # Remove the brackets
    definition <- substr(definition, start=2, stop=nchar(definition)-1)

    # Split the bin into its bounds
    bounds <- as.numeric(strsplit(definition, split=",")[[1]])

    # Plot the bounds as horizontal lines
    abline(h=bounds, col=col)
  }
}

#' A function to calculate the size of a point shape in the current plot
#'
#' Function used by \code{spreadPoints()}
#' @param cex A numerical value giving the amount by which plotted symbols WERE magnified relative to their default in the plotted boxplot
#' @keywords internal
#' @return Returns a numerical vector containing the estimated width and height of a point shape
calculatePointShapeSize <- function(cex){

  # Get the default height of symbol in
  symbolSize <- 0.5 * par("cxy")

  return(symbolSize * cex)
}

#' A function used to identify the indices of the values falling into each bin
#'
#' Function used by \code{spreadPoints()}
#' @param bins A categorical vector detailing the start and end of each bin each value fell in. Defined by \code{cut()} function in \code{spreadPoints()}
#' @keywords internal
#' @return Returns a list detailing the indices of the values that fell in each bin. Keys for list are the bins.
identifyValuesInBins <- function(bins){

  # Initialise a list to store the indices of values in each bin
  indicesOfValuesInBins <- list()

  # Examine each of the bins each value was assigned to
  for(index in seq_along(bins)){

    # Convert from factor to character
    key <- as.character(bins[index])

    # Check if we have encountered this bin before
    if(is.null(indicesOfValuesInBins[[key]]) == FALSE){
      indicesOfValuesInBins[[key]] <- c(indicesOfValuesInBins[[key]], index)
    }else{
      indicesOfValuesInBins[[key]] <- c(index)
    }
  }

  return(indicesOfValuesInBins)
}

#' A function to assign value if multiple options are available, can recycle if index is > number of options available
#'
#' Function used by \code{addTextLabels()}
#' @param colours A single option of vector of options
#' @param index The current index of a label
#' @keywords internal
#' @return Returns the selected option based upon the index provided
setOption <- function(options, index){
  
  # Check if option is null
  option <- NULL
  if(is.null(options) == FALSE){
    # Calculate modulus - the remainder when the index is divided by the number of options provided
    modulus <- index %% length(options)
    
    # Check if modulus non-zero - there is a remainder
    if(modulus != 0){
      
      # Assign option using modulus as index
      option <- options[modulus]
      
      # If no remainder, then index must be the length of the options vector
    }else{
      option <- options[length(options)]
    }
  }
  
  return(option)
}
