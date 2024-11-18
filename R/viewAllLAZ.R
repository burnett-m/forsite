#' viewAllLAZ
#' Displays all LAZ files  from a list, one by one.
#'
#' @param lazFile filename: A string representative of a singular LAZ/LAS file
#'
#' @return The RGL window displays a LAS point cloud
#'
#' @export viewAllLAZ
#' @examples
#' parentDir <- readClipboard() # Read the directory path of a folder containing several LAZ/LAS files
#' setwd(parentDir)
#' files <- list.files(pattern="*.laz")
#' for(file in files){
#'   viewAllLAZ(file)
#' }
viewAllLAZ <- function(lazFile){
  temp_laz <- lidR::readLAS(lazFile)
  if(lidR::is.empty(temp_laz)){ # In case there is no LAZ
    cat(paste0("Skipping empty laz file: ",lazFile))
    return()
  }
  # Access XYZ
  x <- temp_laz@data$X
  y <- temp_laz@data$Y
  z <- temp_laz@data$Z
  # Plot
  rgl::open3d()
  rgl::plot3d(x,y,z,col=terrain.colors(length(z))[as.numeric(cut(z, breaks=length(z)))],size=3,main=lazFile,axes=F,xlab="",ylab="",zlab="")#,type="s")

  # Wait for the user to close the rgl window before continuing to the next file
  cat("Close the rgl window to continue to the next file.\n")
  while (rgl::rgl.cur() > 0) {
    Sys.sleep(0.1)  # Keep the window open and check periodically if it's closed
  }
}

