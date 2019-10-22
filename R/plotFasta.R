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
                      legendCex=1, showNucleotide=FALSE){
  
  # Convert the nucleotides to upper case
  nucleotideAlignment <- toupper(nucleotideAlignment)
  
  # Check if the input aligment is in the right format
  if(class(nucleotideAlignment) == "DNAbin"){
    nucleotideAlignment <- as.character(nucleotideAlignment)
  }else if(class(nucleotideAlignment) == "alignment" || class(nucleotideAlignment) == "phyDat"){
    nucleotideAlignment <- as.character(as.DNAbin(nucleotideAlignment))
  }else if(class(nucleotideAlignment) != "matrix"){
    stop("Class of input nucleotide alignment not recognised:", class(nucleotideAlignment), 
         "please provide alignment as a character matrix or in a \"alignment\", \"phyDat\", or \"DNAbin\" format.")
  }
  
  # Open a pdf if requested
  if(is.null(pdfFileName) == FALSE){
    pdf(pdfFileName, width=pdfWidth, height=pdfHeight)
  }
  
  # Get and set the plotting margins
  currentMar <- par()$mar
  par(mar=c(4.1, 5*labelSpace, 3.1, 0.5))
  
  # Note the colour of each nucleotide
  nucleotideColours <- list('A'="green", 'C'="blue", 'G'="black", 'T'="red", 'N'="white")
  
  # Note the number of sequences and sites
  nSequences <- nrow(nucleotideAlignment)
  nSites <- ncol(nucleotideAlignment)
  
  # Create an empty plot
  plot(x=NULL, y=NULL, xlim=c(1, nSites), ylim=c(1, nSequences), bty="n", yaxt="n", ylab="", xlab="Position",
       cex.lab=xLabCex, cex.axis=xTicksCex)
  
  # Calculate the size of nucleotide to fit within box in matrix
  defaultNucleotideWidth <- strwidth("A")
  
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
      polygon(x=c(siteIndex-0.5, siteIndex-0.5, siteIndex+0.5, siteIndex+0.5), 
              y=c(sequenceIndex-0.5, sequenceIndex+0.5, sequenceIndex+0.5, sequenceIndex-0.5),
              border=rgb(0,0,0,0), col=nucleotideColour)
    }
    
    # Overlay the nucleotides for the current sequence
    if(showNucleotide){
      text(x=seq_len(nSites), y=rep(sequenceIndex, nSites), 
           labels=nucleotideAlignment[sequenceIndex, ],
           col="white", cex=0.5)
    }
  }
  
  # Add the sequence names
  axis(side=2, at=seq_len(nSequences), labels=rownames(nucleotideAlignment), tick=FALSE, las=1, 
       line=-1.5+lineForSequenceNames, cex.axis=sequenceLabelCex)
  
  # Add nucleotide legend
  legend(x=nSites/2, y=nSequences + (0.15*nSequences), horiz=TRUE, xpd=TRUE, pch=22, col="grey", bty="n", xjust=0.5,
         legend=names(nucleotideColours), pt.bg=unlist(nucleotideColours), pt.cex=1.5*legendCex,
         cex=legendCex)
  
  # Reset the plotting margins
  par(mar=currentMar)
  
  # Close the PDF if requested
  if(is.null(pdfFileName) == FALSE){
    dev.off() 
  }
}
