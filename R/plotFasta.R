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

#' Plot the different nucleotides present in an alignment of nucleotide sequences
#'
#' Function uses coloured polygons to illustrate which nucleotides are present in each sequence at each position
#' @param nucleotideAlignment A matrix or a list containing the DNA sequences, or an object of class "alignment", "phyDat", or "DNAbin"
#' @param pdfFileName A character string providing the name of a PDF to send plot into. Default value is NULL (plot not sent to file)
#' @param pdfWidth The width of the graphics region in inches. Default value is 14.
#' @param pdfHeight The height of the graphics region in inches. Default value is 7.
#' @param labelSpace Numeric multiplier to change the size of the space alotted to the sequence labels
#' @param lineForSequenceNames The number of lines into the margin at which the sequence names will be plotted. Default value is 0.
#' @param sequenceLabelCex Numeric multiplier to change the size of the sequence labels. Default value is 0.5.
#' @param xLabCex Numeric multiplier to change size of X axis label. Default value is 1.
#' @param xTicksCex Numeric multipler to change size of X axis tick labels. Default value is 1.
#' @param legendCex Numeric multiplier to change size of legend (points and text). Default value is 1.
#' @param showNucleotide Boolean flag to decide whether or not to plot nucleotide characters onto figure. Defaults to off.
#' @param removeUninformative Boolean flag to decide whether to remove sites where all sequences have the same allele. Default is \code{FALSE}.
#' @keywords nucleotide alignment DNAbin plot
#' @export
#' @examples
#' # Create an example dataset
#' nSequences <- 50
#' nSites <- 100
#' alignment <- matrix(sample(c("A", "C", "G", "T"), nSequences*nSites, replace=TRUE),
#'                     nrow=nSequences, ncol=nSites)
#' rownames(alignment) <- paste("Seq-", 1:nSequences)
#' 
#' # Plot the FASTA
#' plotFASTA(alignment, xTicksCex = 1.5, xLabCex=1.5, sequenceLabelCex=0.4)
plotFASTA <- function(nucleotideAlignment, pdfFileName=NULL, pdfWidth=14, pdfHeight=7, labelSpace=1,
                      lineForSequenceNames=0, sequenceLabelCex=0.5, xLabCex=1, xTicksCex=1,
                      legendCex=1, showNucleotide=FALSE, removeUninformative=FALSE){
  
  # Convert the nucleotides to upper case
  nucleotideAlignment <- toupper(nucleotideAlignment)
  
  # Check if we don't want uninformative sites
  if(removeUninformative){
    
    # Count the number of each nucleotide at each site
    nNucleotidesAtEachSite <- sapply(1:ncol(nucleotideAlignment), countNucleotidesAtSite, nucleotideAlignment)
    
    # Remove the uninformative sites
    nucleotideAlignment[, nNucleotidesAtEachSite > 1]
  }
  
  # Open a pdf if requested
  if(is.null(pdfFileName) == FALSE){
    grDevices::pdf(pdfFileName, width=pdfWidth, height=pdfHeight)
  }
  
  # Get and set the plotting margins
  currentMar <- graphics::par()$mar
  graphics::par(mar=c(4.1, 5*labelSpace, 3.1, 0.5))
  
  # Note the colour of each nucleotide
  nucleotideColours <- list('A'="green", 'C'="blue", 'G'="black", 'T'="red", 'N'="white")
  
  # Note the number of sequences and sites
  nSequences <- nrow(nucleotideAlignment)
  nSites <- ncol(nucleotideAlignment)
  
  # Create an empty plot
  plot(x=NULL, y=NULL, xlim=c(1, nSites), ylim=c(1, nSequences), bty="n", yaxt="n", ylab="", xlab="Position",
       cex.lab=xLabCex, cex.axis=xTicksCex)
  
  # Calculate the size of nucleotide to fit within box in matrix
  defaultNucleotideWidth <- graphics::strwidth("A")
  
  # Examine each sequence
  for(sequenceIndex in seq_len(nSequences)){
    
    # Examine each position in the current sequence
    for(siteIndex in seq_len(nSites)){
      
      # Get the nucleotide at the current position
      nucleotide <- nucleotideAlignment[sequenceIndex, siteIndex]
      
      # Get the colour for the current nucleotide
      nucleotideColour <- nucleotideColours[[nucleotide]]
      if(is.null(nucleotideColour)){
        warning("Nucleotide not recognised:", nucleotide, "only recognises A, C, G, T, or N",
                "(sequence index =", sequenceIndex, ", site index =", siteIndex, ").")
      }
      
      # Draw a polygon for the current poisition coloured by nucleotide
      graphics::polygon(x=c(siteIndex-0.5, siteIndex-0.5, siteIndex+0.5, siteIndex+0.5), 
                        y=c(sequenceIndex-0.5, sequenceIndex+0.5, sequenceIndex+0.5, sequenceIndex-0.5),
                        border=grDevices::rgb(0,0,0,0), col=nucleotideColour)
    }
    
    # Overlay the nucleotides for the current sequence
    if(showNucleotide){
      graphics::text(x=seq_len(nSites), y=rep(sequenceIndex, nSites), 
                     labels=nucleotideAlignment[sequenceIndex, ],
                     col="white", cex=0.5)
    }
  }
  
  # Add the sequence names
  graphics::axis(side=2, at=seq_len(nSequences), labels=rownames(nucleotideAlignment), tick=FALSE, las=1, 
                 line=-1.5+lineForSequenceNames, cex.axis=sequenceLabelCex)
  
  # Add nucleotide legend
  axisLimits <- graphics::par()$usr
  yAxisLength <- axisLimits[4] - axisLimits[3]
  graphics::legend(x=nSites/2, y=0.98*yAxisLength, horiz=TRUE, xpd=TRUE, pch=22, col="grey", bty="n", xjust=0.5,
                   legend=names(nucleotideColours), pt.bg=unlist(nucleotideColours), pt.cex=1.5*legendCex,
                   cex=legendCex)
  
  # Reset the plotting margins
  graphics::par(mar=currentMar)
  
  # Close the PDF if requested
  if(is.null(pdfFileName) == FALSE){
    grDevices::dev.off() 
  }
}

#' Count the number of different nucleotides at a position in a sequence alignment 
#'
#' Function counts the number of each nucleotide (A, C, G, and T) at position in a nucleotide alignment. Used by \code{plotFASTA} function
#' @param position A numeric integer indicating the site in the nucleotide alignment to examine
#' @param nucleotideAlignment A matrix of DNA sequences as characters
#' @keywords internal
#' @return Returns the number of different nucleotides at each position in the input alignment
countNucleotidesAtSite <- function(position, nucleotideAlignment){
  
  # Get the unique nucleotides at the current position in the alignment
  nucleotides <- unique(nucleotideAlignment[, position])
    
  # Remove the N, if present
  nucleotides <- nucleotides[nucleotides != 'N']
    
  return(length(nucleotides))
}

#' Count the number of different nucleotides at a position in a sequence alignment 
#'
#' Function converts the nucleotide alignment to a format that will work with the \code{plotFASTA} function
#' @param nucleotideAlignment A matrix or a list containing the DNA sequences, or an object of class "alignment", "phyDat", or "DNAbin"
#' @keywords internal
#' @return Returns a nucleotide alignment as matrix of nucleotides in upper case
checkAlignmentClass <- function(nucleotideAlignment){
  
  # Check if the input alignment is in the right format
  if(class(nucleotideAlignment) == "DNAbin"){
    nucleotideAlignment <- as.character(nucleotideAlignment)
  }else if(class(nucleotideAlignment) == "alignment" || class(nucleotideAlignment) == "phyDat"){
    nucleotideAlignment <- as.character(ape::as.DNAbin(nucleotideAlignment))
  }else if(class(nucleotideAlignment) != "matrix"){
    stop("Class of input nucleotide alignment not recognised:", class(nucleotideAlignment), 
         "please provide alignment as a character matrix or in a \"alignment\", \"phyDat\", or \"DNAbin\" format.")
  }
  
  # Convert the nucleotides to upper case
  nucleotideAlignment <- toupper(nucleotideAlignment)
  
  return(nucleotideAlignment)
}

