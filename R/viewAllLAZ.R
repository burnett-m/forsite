#' viewAllLAZ
#' Displays all LAZ files  from a list, one by one.
#'
#' @param lazFile filenames: A string representative of a singular LAZ/LAS file
#' @param plotView boolean: TRUE if you want a generalized view of the point cloud with file name, FALSE if you want a superior rendering with poor filename quality
#'
#' @return The RGL window displays a LAS point cloud
#'
#' @export viewAllLAZ
#' @examples
#' parentDir <- readClipboard() # Read the directory path of a folder containing several LAZ/LAS files
#' setwd(parentDir)
#' files <- list.files(pattern="*.laz")
#' for(file in files){viewAllLAS(file)}
viewAllLAZ <- function(lazFile,plotView = FALSE){
  temp_laz <- lidR::readLAS(lazFile)
  if(lidR::is.empty(temp_laz)){ # In case there is no LAZ
    cat(paste0("Skipping empty laz file: ",lazFile))
    return()
  }
  if(isTRUE(plotView)){
    # Access XYZ
    x <- temp_laz@data$X
    y <- temp_laz@data$Y
    z <- temp_laz@data$Z
    # Plot
    rgl::open3d()
    rgl::plot3d(x,y,z,col=terrain.colors(length(z))[as.numeric(cut(z, breaks=length(z)))],size=3,main=lazFile,axes=F,xlab="",ylab="",zlab="")#,type="s")

  }
  else{
    lidR::plot(temp_laz,size=4)
    rgl::bgplot3d({
      par(bg='black')
      plot.new()
      abline(v = min(temp_laz@data$X),col='white')
      title(main = lazFile,line=0.5,adj=1,col.main='white',cex.main=0.8,axis=TRUE)
    })
  }
  # Wait for the user to close the rgl window before continuing to the next file
  cat("Close the rgl window to continue to the next file.\n")
  while (rgl::rgl.cur() > 0) {
    Sys.sleep(0.1)  # Keep the window open and check periodically if it's closed
  }
}

