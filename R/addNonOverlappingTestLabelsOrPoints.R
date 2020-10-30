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

#' Add non-overlapping text labels to plot
#'
#' This function is similar to the \code{text()} function but it will attempt to re-locate labels that will overlap
#' @param xCoords A vector containing the X coordinates for labels
#' @param yCoords A vector containing the Y coordinates for labels
#' @param labels A vector containing the labels to be plotted
#' @param cex.label A number to scale the size of the plotted labels. Defaults to 1
#' @param col.label The colour of the plotted labels. Defaults to "red". Multiple colours can be provided. If more colours than labels provided colours will be recycled.
#' @param col.line The colour of the line to plot from relocated labels to original location. Defaults to "black". Multiple colours can be provided. If more colours than labels provided colours will be recycled.
#' @param col.background An optional colour for a background polygon plotted behind labels. Defaults to NULL - won't be plotted. Multiple colours can be provided. If more colours than labels provided colours will be recycled.
#' @param lty A number detailing the type of line to plot from relocated labels to original location. 0: blank, 1: solid, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, and 6: twodash. Defaults to 1. Multiple line types can be provided. If more options than labels provided types will be recycled.
#' @param lwd A number to scale the size of line from relocated labels to original location. Defaults to 1. Multiple line widths can be provided. If more options than labels provided widths will be recycled.
#' @param border The colour of the border to be plotted around the polygon. Defaults to NA - won't be plotted. Multiple colours can be provided. If more colours than labels provided colours will be recycled.
#' @param avoidPoints A logical variable indicating whether labels shouldn't be plotted on top of points. Defaults to TRUE
#' @param keepLabelsInside A logical variable indicating whether the labels shouldn't be plotted outside of plotting region. Defaults to TRUE
#' @param cex.pt A number used to scale the points plotted on the graph that labels are to be added to. Defaults to 1
#' @keywords text label plot
#' @export
#' @examples
#' # Create some random points
#' n <- 50
#' coords <- data.frame(X=runif(n), Y=runif(n), Name="Test Label")
#'
#' # Plot them without labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#'
#' # With potentially overlapping labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' text(coords$X, coords$Y, labels=coords$Name, xpd=TRUE)
#'
#' # Plot them with non-overlapping labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' addTextLabels(coords$X, coords$Y, coords$Name, cex.label=1, col.label="black")
#'
#' # Plot them with non-overlapping labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' addTextLabels(coords$X, coords$Y, coords$Name, cex.label=1, col.background=rgb(0,0,0, 0.75), col.label="white")
addTextLabels <- function(xCoords, yCoords, labels, cex.label=1, col.label="red", col.line="black", col.background=NULL,
                          lty=1, lwd=1, border=NA, avoidPoints=TRUE, keepLabelsInside=TRUE, cex.pt=1){

  #######################################################
  # Check that the input data are in the correct format #
  #######################################################
  
  # Are each of coordinate vectors the same length?
  if(length(xCoords) != length(yCoords)){
    stop("addTextLabels() The vectors containing the X and Y coodinates must be the same length.")
  }
  if(length(xCoords) != length(labels)){
    stop("addTextLabels() The vector of labels must be the same length as the coordinate vectors.")
  }
  
  #######################
  # Get the axis limits #
  #######################

  # Get the axis limits
  axisLimits <- graphics::par("usr")

  ############################
  # Check for NA coordinates #
  ############################
  
  # Check if any NA coordinates present
  indicesOfNAs <- which(is.na(xCoords) | is.na(yCoords))
  if(length(indicesOfNAs) > 0){
    
    # Send warning
    warning("NA values present in coordinates provided. These are ignored.")
    
    # Check for each of the parameters that can have multiple parameters
    if(length(col.line) == length(xCoords)){
      col.line = col.line[-indicesOfNAs]
    }
    if(length(col.background) == length(xCoords)){
      col.background = col.background[-indicesOfNAs]
    }
    if(length(lty) == length(xCoords)){
      lty = lty[-indicesOfNAs]
    }
    if(length(lwd) == length(xCoords)){
      lwd = lwd[-indicesOfNAs]
    }
    if(length(border) == length(xCoords)){
      border = border[-indicesOfNAs]
    }

    # Remove the NA coordinates
    xCoords <- xCoords[-indicesOfNAs]
    yCoords <- yCoords[-indicesOfNAs]
    
    # Remove the respective labels
    labels <- labels[-indicesOfNAs]
  }
  
  ############################
  # Check if axes are logged #
  ############################

  # Check X axis
  xAxisLogged <- FALSE
  if(graphics::par("xlog")){

    # Note that X axis was logged
    xAxisLogged <- TRUE

    # Log the X coordinates
    xCoords <- log10(xCoords)

    # Reset the X axis logged flag - fools points and polygon commands below
    graphics::par(xlog=FALSE)
  }

  # Check Y axis
  yAxisLogged <- FALSE
  if(graphics::par("ylog")){

    # Note that Y axis was logged
    yAxisLogged <- TRUE

    # Log the Y coordinates
    yCoords <- log10(yCoords)

    # Reset the Y axis logged flag - fools points and polygon commands below
    graphics::par(ylog=FALSE)
  }

  ###############################
  # Store the point information #
  ###############################

  # Store the input coordinates and labels
  pointInfo <- list("X"=xCoords, "Y"=yCoords, "Labels"=labels, "N"=length(xCoords), "cex"=cex.pt)

  # Set the amount to pad onto height and width
  heightPad <- 0.5
  widthPad <- 0.04
  if(is.null(col.background)){
    heightPad <- 0
    widthPad <- 0
  }

  # Calculate the label heights and widths
  pointInfo <- calculateLabelHeightsAndWidths(pointInfo=pointInfo, cex=cex.label,
                                              heightPad=heightPad, widthPad=widthPad)

  ###########################################
  # Produce a list of alternative locations #
  ###########################################

  # Generate the alternative locations
  alternativeLocations <- generateAlternativeLocations(axisLimits)

  # Calculate the distance between the actual and alternative points - rescale X axis remove axis range bias
  distances <- euclideanDistancesWithRescaledXAxis(pointInfo, alternativeLocations, axisLimits)

  ###############################################################
  # Create a list to store the information about plotted points #
  ###############################################################

  # Initialise the list to store the information about plotted labels
  plottedLabelInfo <- list("X"=c(), "Y"=c(), "Height"=c(), "Width"=c(), "N"=0)

  ##############################################################
  # Add labels to plot assigning new locations where necessary #
  ##############################################################

  # Plot the point label
  for(i in seq_len(pointInfo$N)){

    # Set the colours for plotting the label - allows multiple colours and cycling through colours
    labelColour <- setOption(options=col.label, index=i)
    backgroundColour <- setOption(options=col.background, index=i)
    borderColour <- setOption(options=border, index=i)

    # Set the line characteristics
    lineColour <- setOption(options=col.line, index=i)
    lineType <- setOption(options=lty, index=i)
    lineWidth <- setOption(options=lwd, index=i)

    # Get the information for the current point
    x <- pointInfo$X[i]
    y <- pointInfo$Y[i]
    label <- pointInfo$Labels[i]
    height <- pointInfo$Heights[i]
    width <- pointInfo$Widths[i]

    # Get a new location
    newLocationIndex <- chooseNewLocation(pointInfo, i, alternativeLocations, distances, plottedLabelInfo, axisLimits, keepLabelsInside)

    # Is the current point too close to others?
    if(alternativeLocations$N != 0 && newLocationIndex != -1 && 
       (avoidPoints == TRUE || tooClose(x, y, height, width, plottedLabelInfo) || outsidePlot(x, y, height, width, axisLimits))){

      # Get the coordinates for the chosen alternate location
      altX <- alternativeLocations$X[newLocationIndex]
      altY <- alternativeLocations$Y[newLocationIndex]

      # Add line back to previous location
      addLineBackToOriginalLocation(altX=altX, altY=altY, x=x, y=y, label=label,
                                    cex=cex.label, col=lineColour, lty=lineType, lwd=lineWidth, heightPad=heightPad,
                                    widthPad=widthPad)

      # Add label
      addLabel(x=altX, y=altY, label=label,
               cex=cex.label, col=labelColour, bg=backgroundColour, border=borderColour, heightPad=heightPad, widthPad=widthPad)

      # Append the plotted label information
      plottedLabelInfo <- addPlottedLabel(x=altX, y=altY, height=height, width=width,
                                          plottedLabelInfo=plottedLabelInfo)

      # Remove the alternative plotting location used
      alternativeLocations$X <- alternativeLocations$X[-newLocationIndex]
      alternativeLocations$Y <- alternativeLocations$Y[-newLocationIndex]
      alternativeLocations$N <- alternativeLocations$N - 1
      distances <- distances[, -newLocationIndex]

    }else{

      # Add label
      addLabel(x=x, y=y, label=label,
               cex=cex.label, col=labelColour, bg=backgroundColour, border=borderColour,
               heightPad=heightPad, widthPad=widthPad)

      # Append the plotted label information
      plottedLabelInfo <- addPlottedLabel(x=x, y=y, height=height, width=width,
                                          plottedLabelInfo=plottedLabelInfo)
    }
  }

  #####################################################################################
  # Return axes logged flags to original state - for if person makes any future plots #
  #####################################################################################

  graphics::par(xlog=xAxisLogged)
  graphics::par(ylog=yAxisLogged)

}

#' Add non-overlapping points to plot
#'
#' This function is similar to the \code{points()} function but it will attempt to re-locate points that will overlap
#' @param xCoords A vector containing the X coordinates for labels
#' @param yCoords A vector containing the Y coordinates for labels
#' @param col.line The colour of the line to plot from relocated points to original location. Defaults to "black". Multiple colours can be provided. If more colours than labels provided colours will be recycled.
#' @param lty A number detailing the type of line to plot from relocated labels to original location. 0: blank, 1: solid, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, and 6: twodash. Defaults to 1. Multiple line types can be provided. If more options than labels provided types will be recycled.
#' @param lwd A number to scale the size of line from relocated labels to original location. Defaults to 1. Multiple line widths can be provided. If more options than labels provided widths will be recycled.
#' @param keepInside A logical variable indicating whether the points shouldn't be plotted outside of plotting region. Defaults to TRUE
#' @param cex A number used to scale the size of the points plotted. Defaults to 1
#' @param avoidFactor A number that increases (values > 1) or decreases (values < 1) the amount of space alloted to each point. Defaults to 1
#' @param bg A character string that defines the background colour of our point. Defaults to NULL
#' @param ... Arguments to be passed to the \code{points()} function
#' @keywords points x y plot
#' @export
#' @examples
#' # Create some random points
#' n <- 50
#' coords <- data.frame(X=runif(n), Y=runif(n), Name="Test Label")
#'
#' # Plot points and allow overlapping
#' plot(x=coords$X, y=coords$Y, bty="n", xaxt="n", yaxt="n", cex=3, xlab="X", ylab="Y")
#'
#' # Plot points and avoid overlapping
#' plot(x=NULL, y=NULL, xlim=range(coords$X), ylim=range(coords$Y), bty="n", xaxt="n", yaxt="n", xlab="X", ylab="Y")
#' addPoints(coords$X, coords$Y, cex=3, col.line="red")
addPoints <- function(xCoords, yCoords, col.line="black", lty=1, lwd=1, keepInside=TRUE, cex=1, avoidFactor=1, bg=NULL,
                      ...){
  
  ######################################
  # Create cex/bg value for each point #
  ######################################
  
  # Note the cex is to be applied to each point
  if(length(cex) != length(xCoords)){
    cex <- rep(cex, ceiling(length(xCoords)/length(cex)))[1:length(xCoords)]
  }
  
  # Note the bg is to be applied to each point
  if(length(bg) != length(xCoords)){
    bg <- rep(bg, ceiling(length(xCoords)/length(bg)))[1:length(xCoords)]
  }
  
  #######################################################
  # Check that the input data are in the correct format #
  #######################################################
  
  # Are each of coordinate vectors the same length?
  if(length(xCoords) != length(yCoords)){
    stop("addPoints() The vectors containing the X and Y coodinates must be the same length.")
  }
  
  #######################
  # Get the axis limits #
  #######################
  
  # Get the axis limits
  axisLimits <- graphics::par("usr")
  
  ############################
  # Check for NA coordinates #
  ############################
  
  # Check if any NA coordinates present
  indicesOfNAs <- which(is.na(xCoords) | is.na(yCoords))
  if(length(indicesOfNAs) > 0){
    
    # Send warning
    warning("NA values present in coordinates provided. These are ignored.")
    
    # Check for each of the parameters that can have multiple parameters
    if(length(col.line) == length(xCoords)){
      col.line <- col.line[-indicesOfNAs]
    }
    if(length(lty) == length(xCoords)){
      lty <- lty[-indicesOfNAs]
    }
    if(length(lwd) == length(xCoords)){
      lwd <- lwd[-indicesOfNAs]
    }
    if(length(cex) == length(xCoords)){
      cex <- cex[-indicesOfNAs]
    }
    if(length(bg) == length(xCoords)){
      bg <- bg[-indicesOfNAs]
    }
    
    # Remove the NA coordinates
    xCoords <- xCoords[-indicesOfNAs]
    yCoords <- yCoords[-indicesOfNAs]
  }
  
  ############################
  # Check if axes are logged #
  ############################
  
  # Check X axis
  xAxisLogged <- FALSE
  if(graphics::par("xlog")){
    
    # Note that X axis was logged
    xAxisLogged <- TRUE
    
    # Log the X coordinates
    xCoords <- log10(xCoords)
    
    # Reset the X axis logged flag - fools points and polygon commands below
    graphics::par(xlog=FALSE)
  }
  
  # Check Y axis
  yAxisLogged <- FALSE
  if(graphics::par("ylog")){
    
    # Note that Y axis was logged
    yAxisLogged <- TRUE
    
    # Log the Y coordinates
    yCoords <- log10(yCoords)
    
    # Reset the Y axis logged flag - fools points and polygon commands below
    graphics::par(ylog=FALSE)
  }
  
  ###############################
  # Store the point information #
  ###############################
  
  # Calculate the height and width of point on current plot
  pointSize <- calculatePointSize(axisLimits, sizeFactor=avoidFactor)
  
  # Store the input coordinates and labels
  # !Note need to make addTextLabels have multiple cex values!
  pointInfo <- list("X"=xCoords, "Y"=yCoords, "N"=length(xCoords), "Heights"=pointSize[1]*cex, 
                    "Widths"=pointSize[2]*cex, "cex"=1)
  
  ###########################################
  # Produce a list of alternative locations #
  ###########################################
  
  # Generate the alternative locations
  alternativeLocations <- generateAlternativeLocations(axisLimits)
  
  # Calculate the distance between the actual and alternative points - rescale X axis remove axis range bias
  distances <- euclideanDistancesWithRescaledXAxis(pointInfo, alternativeLocations, axisLimits)
  
  ###############################################################
  # Create a list to store the information about plotted points #
  ###############################################################
  
  # Initialise the list to store the information about plotted labels
  plottedPointInfo <- list("X"=c(), "Y"=c(), "Height"=c(), "Width"=c(), "N"=0)
  
  ##############################################################
  # Add labels to plot assigning new locations where necessary #
  ##############################################################
  
  # Plot the point label
  for(i in seq_len(pointInfo$N)){
    
    # Set the line characteristics
    lineColour <- setOption(options=col.line, index=i)
    lineType <- setOption(options=lty, index=i)
    lineWidth <- setOption(options=lwd, index=i)
    
    # Get the information for the current point
    x <- pointInfo$X[i]
    y <- pointInfo$Y[i]
    height <- pointInfo$Heights[i]
    width <- pointInfo$Widths[i]
    
    # Get a new location
    newLocationIndex <- chooseNewLocation(pointInfo, i, alternativeLocations, distances,
                                          plottedPointInfo, axisLimits, keepInside)
    
    # Is the current point too close to others?
    if(alternativeLocations$N != 0 && newLocationIndex != -1 && 
       (tooClose(x, y, height, width, plottedPointInfo) || 
        outsidePlot(x, y, height, width, axisLimits))){
      
      # Get the coordinates for the chosen alternate location
      altX <- alternativeLocations$X[newLocationIndex]
      altY <- alternativeLocations$Y[newLocationIndex]
      
      # Add line back to previous location - from the outside of the circle
      graphics::points(x=c(altX, x), y=c(altY, y), type="l", col=col.line, lty=lty, lwd=lwd, xpd=TRUE)
      
      # Add point
      graphics::points(x=altX, y=altY, cex=cex[i], bg=bg[i], ...)
      
      # Append the plotted point information
      plottedPointInfo <- addPlottedLabel(x=altX, y=altY, height=height, width=width,
                                          plottedLabelInfo=plottedPointInfo)
      
      # Remove the alternative plotting location used
      alternativeLocations$X <- alternativeLocations$X[-newLocationIndex]
      alternativeLocations$Y <- alternativeLocations$Y[-newLocationIndex]
      alternativeLocations$N <- alternativeLocations$N - 1
      distances <- distances[, -newLocationIndex]
      
    }else{
      
      # Add point
      graphics::points(x=x, y=y, cex=cex[i], bg=bg[i], ...)
      
      # Append the plotted point information
      plottedPointInfo <- addPlottedLabel(x=x, y=y, height=height, width=width,
                                          plottedLabelInfo=plottedPointInfo)
    }
  }
  
  #####################################################################################
  # Return axes logged flags to original state - for if person makes any future plots #
  #####################################################################################
  
  graphics::par(xlog=xAxisLogged)
  graphics::par(ylog=yAxisLogged)
  
}

#' Calculate the size of a point on the current plot
#'
#' Function used by \code{addPoints()}
#' @param axisLimits The limits of the X and Y axis: (\code{c(xMin, xMax, yMin, yMax)})
#' @param sizeFactor A number that increases (values > 1) or decreases (values < 1) the amount of space alloted to each point
#' @keywords internal
#' @return Returns a vector containing the width and height of a point
calculatePointSize <- function(axisLimits, sizeFactor=1){
  
  # Get the plotting window size in inches
  plotSizeInches <- graphics::par()$pin # width, height
  widthInches <- plotSizeInches[1]
  heightInches <- plotSizeInches[2]
  
  # Get the plotting window size in the plotting units
  widthX <- axisLimits[2] - axisLimits[1]
  heightY <- axisLimits[4] - axisLimits[3]
  
  # Calculate the size of a point in the current plot
  # Cex=1 is 1/72 inches (https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/pdf.html)
  # Dividing by 72 is far too small - decided to choose 15?!?!
  pointWidth <- (widthX / widthInches) / (15/sizeFactor)
  pointHeight <- (heightY / heightInches) / (15/sizeFactor)
  
  return(c(pointWidth, pointHeight))
}

#' A function to assign value if multiple options are available, can recycle if index is > number of options available
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
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

#' Add the information associated with a text label that has been plotted
#'
#' Function used by \code{addTextLabels()}
#' @param x X coordinate of point of interest
#' @param y Y coodrinate of point of interest
#' @param height The height of the label associated with the point of interest
#' @param width The width of the label associated with the point of interest
#' @param plottedLabelInfo The coordinates and label information about the locations where a label has already plotted
#' @keywords internal
#' @return Returns a list containing information for all the plotted labels, included the one just added
addPlottedLabel <- function(x, y, height, width, plottedLabelInfo){

  plottedLabelInfo$X[plottedLabelInfo$N + 1] <- x
  plottedLabelInfo$Y[plottedLabelInfo$N + 1] <- y
  plottedLabelInfo$Heights[plottedLabelInfo$N + 1] <- height
  plottedLabelInfo$Widths[plottedLabelInfo$N + 1] <- width

  plottedLabelInfo$N <- plottedLabelInfo$N + 1

  return(plottedLabelInfo)
}

#' Plot line from new alternative location back to original
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param altX The X coordinate of new location
#' @param altY The Y coordinate of new location
#' @param x The X coordinate of original location
#' @param y The Y coordinate of original location
#' @param label The label to be plotted. Required to work out when line ends
#' @param cex The number used to scale the size of the label. Required to work out when line ends
#' @param col Colour of line to be plotted
#' @param lty A number detailing the type of line to be plotted. 0: blank, 1: solid, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, and 6: twodash.
#' @param lwd A number to scale the size of plotted line.
#' @param heightPad Multiplyer for label height should added to label to be used to pad height
#' @param widthPad Multiplyer for label width should added to label to be used to pad width
#' @keywords internal
addLineBackToOriginalLocation <- function(altX, altY, x, y, label, cex, col, lty, lwd, heightPad, widthPad){

  # Calculate the label width and height
  labelHeight <- graphics::strheight(label, cex=cex)
  labelWidth <- graphics::strwidth(label, cex=cex)

  # Calculate amount outer left/right and above/below
  xHalf <- labelWidth * (0.5 + (0.5 * widthPad))
  yHalf <- labelHeight * (0.5 + (0.5 * heightPad))

  # Create a set of points marking the boundaries of the label
  xMarkers <- c(seq(from=altX - xHalf, to=altX + xHalf, by=0.05*labelWidth), altX + xHalf)
  yMarkers <- c(seq(from=altY - yHalf, to=altY + yHalf, by=0.05*labelHeight), altY + yHalf)

  # Calculate the closest pair of X and Y coordinates to the origin
  closestX <- xMarkers[which.min(abs(xMarkers - x))]
  closestY <- yMarkers[which.min(abs(yMarkers - y))]

  # Plot the line
  graphics::points(x=c(closestX, x), y=c(closestY, y), type="l", col=col, lty=lty, lwd=lwd, xpd=TRUE)
}

#' Calculate the heights and widths of the labels in the current plotting window
#'
#' Function used by \code{addTextLabels()}
#' @param pointInfo A list storing the coordinates and labels of input points
#' @param cex The number used to scale the size of the label and therefore its height and width
#' @param heightPad Multiplyer for label height should added to label to be used to pad height
#' @param widthPad Multiplyer for label width should added to label to be used to pad width
#' @keywords internal
#' @return Returns a list storing the coordinates, labels, and the heights and widths of the labels, for input points
calculateLabelHeightsAndWidths <- function(pointInfo, cex, heightPad, widthPad){

  # Get the text label heights and lengths
  textHeights <- graphics::strheight(pointInfo$Labels)
  textWidths <- graphics::strwidth(pointInfo$Labels)

  # Multiply by cex
  textHeights <- textHeights * cex
  textWidths <- textWidths * cex

  # Add padding to widths and heights
  # Note multiplies padding by 2 - stops background polygons being directly adjacent
  pointInfo[["Heights"]] <- textHeights + (2 * heightPad * textHeights)
  pointInfo[["Widths"]] <- textWidths + (2 * widthPad * textWidths)

  return(pointInfo)
}

#' Generate a set of alternative locations where labels can be plotted if they overlap with another label
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param axisLimits The limits of the X and Y axis: (\code{c(xMin, xMax, yMin, yMax)})
#' @keywords internal
#' @return Returns a list containing the coordinates of the alternative locations
generateAlternativeLocations <- function(axisLimits){

  # Initialise a list to store the alternative locations
  alternativeLocations <- list("X"=c(), "Y"=c())

  # Define the spacer for each axis
  spacerX <- 0.01 * (axisLimits[2] - axisLimits[1])
  spacerY <- 0.01 * (axisLimits[4] - axisLimits[3])

  # Generate the set of points based upon the spacer
  for(i in seq(axisLimits[1], axisLimits[2], spacerX)){
    for(j in seq(axisLimits[3], axisLimits[4], spacerY)){

      alternativeLocations$X[length(alternativeLocations$X) + 1] <- i
      alternativeLocations$Y[length(alternativeLocations$Y) + 1] <- j
    }
  }
  #graphics::points(alternativeLocations$X, alternativeLocations$Y, col=rgb(0,0,0, 0.5), pch=20, xpd=TRUE)

  # Note the number of alternative locations created
  alternativeLocations[["N"]] <- length(alternativeLocations$X)

  return(alternativeLocations)
}

#' Plot a label with optional polygon background
#'
#' Function used by \code{addTextLabels()}
#' @param x The X coordinate at which label is to be plotted
#' @param y The Y coordinate at which label is to be plotted
#' @param label The label to be plotted
#' @param cex The number used to scale the size of the label
#' @param col The colour of the label to be plotted
#' @param bg The colour of the polygon to be plotted. If NULL no polygon plotted
#' @param border The colour of the polygon border. If NA, no border plotted
#' @param heightPad Multiplyer for label height should added to label to be used to pad height
#' @param widthPad Multiplyer for label width should added to label to be used to pad width
#' @keywords internal
addLabel <- function(x, y, label, cex, col, bg, border, heightPad, widthPad){

  # Add a background polygon - if requested
  if(is.null(bg) == FALSE){

    # Calculate the height and width of the label
    labelHeight <- graphics::strheight(label, cex=cex)
    labelWidth <- graphics::strwidth(label, cex=cex)

    # Calculate amount outer left/right and above/below
    xHalf <- labelWidth * (0.5 + (0.5 * widthPad))
    yHalf <- labelHeight * (0.5 + (0.5 * heightPad))

    # Plot the background polygon
    graphics::polygon(x=c(x - xHalf, x - xHalf, x + xHalf, x + xHalf),
                      y=c(y - yHalf, y + yHalf, y + yHalf, y - yHalf),
                      col=bg, border=border, xpd=TRUE)
  }


  # Add label
  graphics::text(x=x, y=y, labels=label, xpd=TRUE, cex=cex, col=col)
}

#' Remove coordinates of alternative locations that are too close to coordinates
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param altXs A vector of X coordinates for alternative locations
#' @param altYs A vector of Y coordinates for alternative locations
#' @param index The index of the point of interest in the coordinate vectors
#' @param textHeight The height of the label to be plotted at the point of interest
#' @param textWidth The width of the label to be plotted at the point of interest
#' @param distances The distances between the actual and alternative locations
#' @keywords internal
#' @return Returns a list of the coordinates of the alternative locations that weren't too close and the distance matrix of the alternate locations to the actual locations
removeLocationAndThoseCloseToItFromAlternatives <- function(altXs, altYs, index, textHeight, textWidth, distances){
  remove <- c(index)
  for(i in 1:length(altXs)){

    if(i == index){
      next
    }

    if(abs(altXs[index] - altXs[i]) < textWidth &&
       abs(altYs[index] - altYs[i]) < textHeight){
      remove[length(remove) + 1] <- i
    }
  }

  altXs <- altXs[-remove]
  altYs <- altYs[-remove]
  distances <- distances[, -remove]

  return(list("X" = altXs, "Y" = altYs, "distances"=distances))
}

#' A function to choose (from the alternative locations) a new location for a label to be plotted at
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param pointInfo A list storing the information for the input points
#' @param index The index of the point of interest
#' @param alternativeLocations The coordinates of the alternative locations
#' @param distances The distances between the alternative locations and the input points
#' @param plottedLabelInfo The coordinates and label information about the locations where a label has already plotted
#' @param axisLimits The limits of the X and Y axis: (\code{c(xMin, xMax, yMin, yMax)})
#' @param keepLabelsInside A logical variable indicating whether the labels shouldn't be plotted outside of plotting region
#' @keywords internal
#' @return Returns the index of the chosen alternative location
chooseNewLocation <- function(pointInfo, index, alternativeLocations, distances, plottedLabelInfo, axisLimits, keepLabelsInside){

  # graphics::points(alternativeLocations$X, alternativeLocations$Y, pch=19, xpd=TRUE,
  #        col=rgb(1,0,0, distances[index, ] / max(distances[index, ])))

  # Get the information about the current point
  x <- pointInfo$X[index]
  y <- pointInfo$Y[index]
  height <- pointInfo$Heights[index] * pointInfo$cex
  width <- pointInfo$Widths[index] * pointInfo$cex

  # Get the indices of the alternative locations as an ordered
  orderedAlternateLocationIndices <- order(distances[index, ])

  # Initialise a variable to store the index of the selected alternative location
  indexOfSelectedAlternativeLocation <- -1

  # Examine each of the alternate locations in order
  for(i in orderedAlternateLocationIndices){

    # Get the coordinates of the current alternative location
    altX <- alternativeLocations$X[i]
    altY <- alternativeLocations$Y[i]

    # Check current alternative location isn't too close to plotted labels or the plotted input points or label will overlap with plot edges
    if(overlapsWithPlottedPoints(x=altX, y=altY, height=height, width=width, pointInfo=pointInfo) == FALSE &&
       tooClose(x=altX, y=altY, height=height, width=width, plottedLabelInfo) == FALSE &&
       (keepLabelsInside == FALSE || outsidePlot(x=altX, y=altY, height=height, width=width, axisLimits=axisLimits) == FALSE)){

      # Store the current index
      indexOfSelectedAlternativeLocation <- i
      break
    }
  }

  return(indexOfSelectedAlternativeLocation)
}

#' Checks whether a point is too close to any of the plotted points
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param x X coordinate of point of interest
#' @param y Y coodrinate of point of interest
#' @param height The height of the label associated with the point of interest
#' @param width The width of the label associated with the point of interest
#' @param pointInfo A list storing the information for the input points - that have been plotted
#' @keywords internal
#' @return Returns a logical variable to indicate whether the point of interest was too close to any plotted points
overlapsWithPlottedPoints <- function(x, y, height, width, pointInfo){

  result <- FALSE
  for(i in seq_len(pointInfo$N)){

    if(abs(x - pointInfo$X[i]) < width && abs(y - pointInfo$Y[i]) < height){
      result <- TRUE
      break
    }
  }

  return(result)
}

#' Checks whether adding a label at the current point will ending up being outside of the plotting window
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param x X coordinate of point of interest
#' @param y Y coodrinate of point of interest
#' @param height The height of the label associated with the point of interest
#' @param width The width of the label associated with the point of interest
#' @param axisLimits The limits of the X and Y axis: (\code{c(xMin, xMax, yMin, yMax)})
#' @keywords internal
#' @return Returns a logical variable to indicate whether the point of interest was too close to any plotted labels
outsidePlot <- function(x, y, height, width, axisLimits){

  # Calculate half width and height
  halfWidth <- 0.5 * width
  halfHeight <- 0.5* height

  # Check if adding a label at the current point would overlap with the plotting window edges
  result <- FALSE
  if(x + halfWidth > axisLimits[2] ||
     x - halfWidth < axisLimits[1] ||
     y + halfHeight > axisLimits[4] ||
     y - halfHeight < axisLimits[3]){
    result <- TRUE
  }

  return(result)
}

#' Checks whether a point is too close to any of the plotted labels
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param x X coordinate of point of interest
#' @param y Y coodrinate of point of interest
#' @param height The height of the label associated with the point of interest
#' @param width The width of the label associated with the point of interest
#' @param plottedLabelInfo The coordinates and label information about the locations where a label has already plotted
#' @keywords internal
#' @return Returns a logical variable to indicate whether the point of interest was too close to any plotted labels
tooClose <- function(x, y, height, width, plottedLabelInfo){

  # Check if logged axes were used
  if(graphics::par("xlog")){
    x <- log10(x)
  }
  if(graphics::par("ylog")){
    y <- log10(y)
  }

  # Check if the current point is too close to any of the plotted locations
  result <- FALSE
  for(i in seq_len(plottedLabelInfo$N)){

    if(abs(x - plottedLabelInfo$X[i]) < (0.5 * plottedLabelInfo$Widths[i]) + (0.5 * width) &&
       abs(y - plottedLabelInfo$Y[i]) < (0.5 * plottedLabelInfo$Heights[i]) + (0.5 * height)){
      result <- TRUE
      break
    }
  }

  return(result)
}

#' Calculate the euclidean distance between two sets of points. Note: Rescales X axis to match scale of Y
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param pointInfo A list storing the information for the input points
#' @param alternativeLocations A list storing the coordinates of the alternative locations
#' @param axisLimits The limits of the X and Y axis: (\code{c(xMin, xMax, yMin, yMax)})
#' @keywords internal
#' @return Returns the distances between the sets of points provided
euclideanDistancesWithRescaledXAxis <- function(pointInfo, alternativeLocations, axisLimits){

  # Calculate the axis ranges
  xRange = axisLimits[2] - axisLimits[1]
  yRange = axisLimits[4] - axisLimits[3]

  # Calculate the xFactor
  xFactor <- yRange / xRange

  # Initialise a matrix to store distances - note that it is non-symmetric!!!
  distances <- matrix(NA, nrow=pointInfo$N, ncol=alternativeLocations$N)

  # Fill the matrix with distances
  for(row in seq_len(nrow(distances))){

    for(col in seq_len(ncol(distances))){

      # Calculate the distance between the current pair of points
      # REMEMBER to correct the X values for the axes ranges
      distances[row, col] <- euclideanDistance(x1=pointInfo$X[row] * xFactor,
                                               y1=pointInfo$Y[row],
                                               x2=alternativeLocations$X[col] * xFactor,
                                               y2=alternativeLocations$Y[col])
    }
  }

  return(distances)
}

#' Calculate the euclidean distance between two points
#'
#' Function used by \code{addTextLabels()} and \code{addPoints()}
#' @param x1 The X coordinate of the first point
#' @param y1 The Y coordinate of the first point
#' @param x2 The X coordinate of the second point
#' @param y2 The Y coordinate of the second point
#' @keywords internal
#' @return Returns the distance between the points provided
euclideanDistance <- function(x1, y1, x2, y2){
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}
