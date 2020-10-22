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
#packageDirectory <- "/Users/josephcrispell/Desktop/basicPlotteR/"
#usethis::create_package(packageDirectory)

## Documenting changes
#setwd(packageDirectory)
#document()

## Install
#setwd("..")
#install("basicPlotteR")

#' A function to produce a radar chart designed for mapping expertise for a range of skills
#'
#' Given a set of skills and scores for expertise, the function produces a radar chart as a tool to map our skills status and progress.
#' @param scores Numeric vector of scores for each skill provided
#' @param names Vector of character strings providing the name of each skill
#' @param levels Vector of labels for each skills level
#' @param polygon.col The fill colour for the skills polygon. Default value is \code{"red"}.
#' @param polygon.alpha The transparency of the fill colour for the skills polygon. Default value is 0.1.
#' @param polygon.pch The shape of the points on skills polygon. Default value is 19. Ignored if \code{addPoints} is FALSE.
#' @param axisLabelPad The spacing between the name of each skill and the radar chart. Default value is 1.2.
#' @param circles A boolean value indicating whether to plot skills levels as polygons or circles. Default value is FALSE.
#' @param add A boolean value indicating whether to create a new radar chart or add to an existing one. Default value is FALSE.
#' @param main Plot title. Default value is empty.
#' @param margins A numeric vector specifying the sizes of the margins in inches (bottom, left, top, right). Default value is \code{c(3,3,3,3)}.
#' @param addPoints A boolean value indicating whether to plot points in skills polygon corners. Default value is FALSE.
#' @param radar.col The colour of the skills levels. Default value is \code{rgb(0,0,0, 0.5)}.
#' @param radar.lty The line type for the skills levels. Default value is 1.
#' @param radar.lwd The line width for the skills levels. Default value is 0.5.
#' @param levels.font The font of the skills levels labels. Default value is 1.
#' @param levels.cex The size of the skills levels labels. Default value is 1.
#' @param labels.font The font of the names of the skills. Default value is 2.
#' @param labels.cex The size of the names of the skills. Default value is 1.
#' @param levelsAxesAngle The angle of the skills levels axis. Default value is 0.
#' @param levelsLabelsAngle The angle of the skills levels labels on skills levels axis. Default value is 0.
#' @keywords progress skills radar
#' @export
#' @examples
#' # Create a radar chart to map our current status for selected skills
#' radarChart(scores=c(4,3.5,2,3,3,3), 
#'            names=c("Programming", "Statistics", "Databases", "Projects",
#'                    "Web", "Versioning"),
#'            levels=c("Unaware","Aware","Working","Practitioner", "Expert"))
#' 
#' # Add to the radar chart where we aim to get to
#' radarChart(scores=c(4,3.75,3,4,3,4), 
#'            names=c("Programming", "Statistics", "Databases", "Projects",
#'                    "Web", "Versioning"),
#'            levels=c("Unaware","Aware","Working","Practitioner", "Expert"),
#'            polygon.col="blue", add=TRUE)
#' 
#' # Add a legend
#' legend("topright", 
#'        legend=c("Current", "Aim"), 
#'        text.col=c(rgb(1,0,0, 0.5), rgb(0,0,1, 0.5)), 
#'        bty="n", text.font=2, cex=1.5, xpd=TRUE)
radarChart <- function(scores, names, levels, 
                       polygon.col="red", polygon.alpha=0.1, polygon.pch=19,
                       axisLabelPad=1.2, circles=FALSE, add=FALSE, main="",
                       margins=c(3,3,3,3), addPoints=FALSE, 
                       radar.col=rgb(0,0,0, 0.5), radar.lty=1, radar.lwd=0.5,
                       levels.font=1, levels.cex=1, labels.font=2, labels.cex=1,
                       levelsAxesAngle=0, levelsLabelsAngle=0){
  
  # Check scores and names are the same length
  if(length(scores) != length(names)){
    stop(paste0("The number of scores (", length(scores), ") provided doesn't match the number of names provided (", length(names), ")"))
  }
  
  # Check scores aren't outside levels
  levelsRange <- 1:length(levels)
  if(sum(scores < 1) > 0 || sum(scores > length(levels)) > 0){
    stop("The scores provided don't fall on or within the specified levels.")
  }
  
  # Count number of levels
  nLevels <- length(levels)
  
  # Generate the axis points (for each skill)
  axesEnds <- generateEquiDistantPointsOnCircle(length(scores), radius=nLevels)
  axesInfo <- data.frame("X"=axesEnds[, 1], "Y"=axesEnds[, 2])
  
  # Get and set the plotting margins
  currentMar <- par()$mar
  par(mar=margins)
  
  # Check not adding to existing plot
  if(add == FALSE){
    
    # Create an empty plot
    plot(x=NULL, y=NULL, xlim=c(-nLevels, nLevels), ylim=c(-nLevels, nLevels),
         bty="n", asp=1, main=main, yaxt="n", xaxt="n", xlab="", ylab="")
    
    # Add in category titles
    text(x=axesInfo$X * axisLabelPad, y=axesInfo$Y * axisLabelPad,
         labels=names, xpd=TRUE, font=labels.font, cex=labels.cex)
    
    # Add each category line
    for(index in seq_along(scores)){
      lines(x=c(axesInfo[index, "X"] * 1/nLevels, axesInfo[index, "X"]),
            y=c(axesInfo[index, "Y"] * 1/nLevels, axesInfo[index, "Y"]),
            lwd=radar.lwd, lty=radar.lty, col=radar.col)
    }
    
    # Add in levels
    for(level in 1:nLevels){
      points <- generateEquiDistantPointsOnCircle(ifelse(circles, 360, length(scores)), radius=level)
      polygon(points, border=radar.col, col=rgb(0,0,0, 0), lwd=radar.lwd, lty=radar.lty)
    }
    
    # Add level labels
    axesCoords <- calculateAxesCoordinatesAtEachLevel(axesInfo, nLevels, circles, levelsAxesAngle)
    text(axesCoords, labels=levels, font=levels.font, cex=levels.cex, srt=360-levelsLabelsAngle)
  }
  
  # Add in a skills polygon
  polygon(x=scores/nLevels * axesInfo$X,
          y=scores/nLevels * axesInfo$Y,
          border=polygon.col, col=basicPlotteR::setAlpha(polygon.col, polygon.alpha))
  if(addPoints){
    points(x=scores/nLevels * axesInfo$X,
           y=scores/nLevels * axesInfo$Y,
           pch=polygon.pch, col=polygon.col)
  }
  
  # Reset plotting margins
  par(mar=currentMar)
}

#' Define the coordinates for the radar level labels
#'
#' Function used by \code{radar()} to calculate locations of radar level labels based on angle
#' @param axesInfo A data.frame of X and Y coordinates for end points of each skills axes
#' @param nLevels An integer for the number of radar levels
#' @param circles A boolean value reporting whether radar chart uses circles or polygons for levels
#' @param angle A numeric value representing the angle for the radar levels axes
#' @keywords internal
#' @return Returns a data.frame of the X and Y coordinates of level label coordinates for axes.
calculateAxesCoordinatesAtEachLevel <- function(axesInfo, nLevels, circles, angle){
  
  # Calculate the possible positions on the edges of polygon or circle
  possibleAxesLocations <- generatePointsAlongPolygon(axesInfo, nPoints=360)
  if(circles){
    possibleAxesLocations <- generateEquiDistantPointsOnCircle(360, radius=nLevels)
  }
  
  # Select the position using angle specified
  axesLocation <- possibleAxesLocations[(angle %% 360)+1, ]
  
  # Calculate the level coordinates from centre to point selected
  coords <- data.frame(X=((1/nLevels)*(1:nLevels)) * axesLocation$X,
                       Y=((1/nLevels)*(1:nLevels)) * axesLocation$Y)
  
  return(coords)
}

#' Generate equidistant points on a circle
#'
#' Function used by \code{radarChart()} to create the locations for the skills axes
#' @param nPoints Integer for the number of points to position on circle
#' @param radius Numeric value for the radius of the circle. Default value is 1.
#' @param origin Numeric vector of length 2 that provides the origin coordinates of the circle. Defaults to \code{c(0,0)}
#' @keywords internal
#' @return Returns a data.frame of the X and Y coordinates of points around the circle.
generateEquiDistantPointsOnCircle <- function(nPoints, radius=1, origin=c(0,0)){
  
  # Code taken from: https://stackoverflow.com/questions/5300938/calculating-the-position-of-points-in-a-circle
  
  # Define theta
  start <- 0
  end <- (2*pi) - (2*pi)/nPoints
  theta <- seq(start, end, length.out=nPoints)
  
  # Calculate the coordinates
  x <- origin[2] + (radius * sin(theta))
  y <- origin[1] + (radius * cos(theta))
  
  # Store the coordinates in a dataframe
  return(data.frame("X"=x, "Y"=y, "Theta"=theta))
}

#' Places set number of points along the edges of a polygon.
#'
#' Function used by \code{calculateAxesCoordinatesAtEachLevel()} to create possible locations for the end of the levels axes
#' @param coords A data.frame of X and Y coordinates for end points of each skills axes
#' @param nPoints The number of points to position on polygon, must be at least twice the number of skills axes
#' @keywords internal
#' @return Returns a data.frame of the X and Y coordinates of points along polygon edges. Note includes polygon coordinates.
generatePointsAlongPolygon <- function(coords, nPoints){
  
  # Calculate the points per edge on polygon
  nPointsPerEdge <- floor((nPoints - nrow(coords)) / nrow(coords))
  
  # Create a dataframe to store the point coordinates
  points <- data.frame(X=NA, Y=NA)
  
  # Examine each edge
  for(start in seq_len(nrow(coords))){
    
    # Note the end point
    end <- ifelse(start == 6, 1, end + 1)
    
    # Position points on edge
    pointsOnEdge <- data.frame(X=coords[start, "X"] + ((1/nPointsPerEdge)*(1:nPointsPerEdge)) * (coords[end, "X"] - coords[start, "X"]),
                               Y=coords[start, "Y"] + ((1/nPointsPerEdge)*(1:nPointsPerEdge)) * (coords[end, "Y"] - coords[start, "Y"]))
    points <- rbind(points, c(coords[start, "X"], coords[start, "Y"]))
    points <- rbind(points, pointsOnEdge)
  }
  
  # Remove first NA row
  points <- points[is.na(points$X) == FALSE, ]
  
  return(points)
}
