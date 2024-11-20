#' Reclassify LAZ to LASUNCLASSIFIED
#'
#' This tool reclassifies all of the LAZ/LAS files within a list to change a series of classes to become unclassified. This is useful for seg cal and other various PowerPoint presentations.
#'
#' @param lazFile filename : This can be an item in a list or a singular filename
#' @param lazClasses list : This is a list of classes to be reclassified to LASUNCLASSIFIED. Please refer to the format in which they are already classed
#'
#' @return LAZ/LAS files
#' @export reclassifyLAZ
#'
#' @examples
#' parentDirectory <- readClipboard() # Read the directory of a folder containing various LAS/LAZ files into your clipboard and run this line
#' setwd(parentDirectory)
#' files <- list.files(pattern="*.laz")
#' for(file in files){
#'   reclassifyLAZ(file)
#' }
reclassifyLAZ <- function(lazFile,lazClasses = c(3,4,5,6)){
  las <- lidR::readLAS(lazFile)
  for(class in lazClasses){
    las$Classification[las$Classification == class] <- LASUNCLASSIFIED
  }
  dir.create("output",showWarnings = FALSE)
  outFile <- paste0(getwd(),"\\output\\",lazFile)
  lidR::writeLAS(las,outFile)
}
