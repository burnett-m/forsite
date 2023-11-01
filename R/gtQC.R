#' Ground Truth Quality Control
#'
#' Used from your respective _inProgress folder to attach your name to the boxes and display all items simultaneously up until 120 RGL windows (and then stop since RGL is prone to crashing if too many windows are open)
#'
#' @param parentDirectory string. The full '_inProgress' directory after your user name. This process will rename every box in this folder with "_"+username.
#' @param buffered logical. Change to `TRUE` if you would like to visualize the buffered LAZ description
#'
#' @return A series of RGL windows visualizing the LAZ
#' @export gtQC
#'
#' @examples
#' # parentDir <- readClipboard() # After copying the GT boxes directory to your clipboard
#' # gtQc(parentDir)
gtQC <- function(parentDirectory, buffered = FALSE){
  setwd(parentDirectory)
  folds <- list.files()

  # Get user information
  directories <- sapply(strsplit(parentDirectory,"\\\\"),"[") # Get list of folders in directory
  user <- directories[length(directories)] # Select last directory for user name
  # Find unique attributes in list
  # delimitedFolds <- data.frame(do.call('rbind',strsplit(as.character(folds),"_",fixed=TRUE)))
  # nonFolderElements <- list()
  # for(i in c(1:nrow(delimitedFolds))){
  #   if(isTRUE(delimitedFolds[i,1]==delimitedFolds[i,2])){ # Remove non folders
  #     folds <- folds[-i]
  #     nonFolderElements <- rbind(nonFolderElements,i)
  #   }
  #   # else(if(isTRUE(grepl('Box',delimitedFolds[3]))){
  #   #   delimitedFolds[i,1] <- paste0(delimitedFolds[i,1],"_",delimitedFolds[i,2])
  #   # })
  # }

  folds <- list.files()
  folds <- folds[dir.exists(folds)]

  # limit boxes to 120
  points <- list()
  folders <- list()
  for(i in folds){ # Go through points SHP and find the box with 120th point
    setwd(paste0(parentDirectory,"/",i))
    pointsDBF <- foreign::read.dbf(list.files(pattern="*Points.dbf"))
    for(j in as.character(pointsDBF$Stereo_SPP)){
      points <- rbind(points,j) # create count to 100
    }
    if(nrow(points) > 122){
      break
    }
    folders <- rbind(folders,i)
  }
  for(folder in folders){
    if(isFALSE(grepl(user,folder))){ # If username is not already in folder name, rename the folders
      file.rename(folder,paste0(folder,"_",user))
    }
  }

  print("Displaying the following GT Boxes : ") # Print the names of the GT Boxes that will be displayed
  for(folder in folders){print(folder)}

  # Display all LAS
  for(i in folders){
    setwd(paste0(parentDirectory,"/",i,"/trees"))
    if(isTRUE(buffered)){setwd("buffered")} # Go to buffered LAS to display those files instead

    lasFiles <- list.files(pattern="*.laz")
    for(l_file in lasFiles){
      l_file_name <- gsub("_buffered","",l_file)
      las <- lidR::readLAS(l_file)
      if(tolower(substr(l_file_name,nchar(l_file_name)-5,nchar(l_file_name)-4)) == 'sn' ||tolower(substr(l_file_name,nchar(l_file_name)-5,nchar(l_file_name)-4)) == 'dp' ){ # Plot Sn differently
        lidR::plot(las,color='Intensity',axis=TRUE,pal=grDevices::grey.colors(20,start=0.1),legend=TRUE,size=4)
        rgl::bgplot3d({
          graphics::par(bg='black')
          graphics::plot.new()
          graphics::abline(v = min(las$X),col='white')
          graphics::title(main = gsub(".laz","",l_file_name),line=1,adj=0,col.main='white',cex.main=0.9)
        })
      }
      else{
        lidR::plot(las,size=4,pal=grDevices::terrain.colors(50),axis=TRUE)
        rgl::bgplot3d({
          graphics::par(bg='black')
          graphics::plot.new()
          graphics::abline(v = min(las$X),col='white')
          graphics::title(main = gsub(".laz","",l_file_name),line=1,adj=0,col.main='white',cex.main=0.9)
        })
      }
    }
  }
}
