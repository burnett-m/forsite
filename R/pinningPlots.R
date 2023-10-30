# Pinning Plots function
#' Pinning Plots
#'
#' This function will help build a foundation for pinning your plots. It is not perfect, and it will not capture any of the understory, but it will help to get you started.
#'
#' @param parentDirectory string: The parent directory of your InProgress folder. The final folder in this directory must be your name (example:"...GT_Boxes/_inProgress/Michael). The easiest way to use this is to copy the directory (in Windows File Explorer, copy the entire directory at the top of the window, and in your R window, assign it to a variable like this : parentDir <- readClipboard(). Then use the 'parentDir' variable in this place).
#' @param folderCount numerical: Since you will be working one folder at a time, start with the number 1 and after you have finished pinning that plot, use the number 2 (or if you immediately move that first plot to the Final folder, you can use 1 again).
#' @param user string: Your name (the last folder name, after '_inProgress')
#' @param overstory string: Overstory tree type. Default is 'CN', but you can change to 'DC' after viewing the plot.
#' @param understory string: Understory tree type. Default is 'CU', but you can change to 'DU' after viewing the plot.
#' @param windowSize numeric or function: Length or diameter of the moving window used to detect the local maxima in the units of the input data (usually meters). If it is numeric a fixed window size is used. If it is a function, the function determines the size of the window at any given location on the canopy. By default function takes the height of a given pixel or point as its only argument and return the desired size of the search window when centered on that pixel/point. This can be controled with the 'ws_args' parameter
#' @param minHeight numeric: Minimum height of a tree. Threshold below which a pixel or a point cannot be a local maxima. Default is 2.
#'
#' @return A roughly pinned plot in a treeTops.shp file. Must of the understory will be excluded.
#'
#' @export pinningPlots
#' @examples
#' # Use the following template
#' # parentDir <- readClipboard() # after copying the file directory to your clipboard
#' # pinningPlots(parentDir,1,'Michael')
pinningPlots <- function(parentDirectory, folderCount, user, overstory="CN", understory="CU", windowSize=2, minHeight=5){
  setwd(parentDirectory)
  folds <- list.files()
  if(isFALSE(grepl(user,folds[1]))){
    file.rename(folds[1],paste0(folds[1],"_",user))
  }
  folds <- list.files()
  print(folds[folderCount])

  # Run these commands if there's no boundary file
  boundaryCommand <- paste0("lasboundary -i ",gsub(paste0("_",user),"",folds[folderCount]),".las",
                            " -o ",r"{boundary.shp}")
  shell(paste0("cd ",folds[folderCount]," && ",boundaryCommand))

  las <- lidR::readLAS(paste0(parentDirectory,"\\",folds[folderCount],"\\Buffered\\",gsub(paste0("_",user),"",folds[folderCount]),".las"))
  ttops <- lidR::locate_trees(las,lidR::lmf(ws=windowSize,hmin = minHeight))
  x <- lidR::plot(las,bg="white",size=4)
  lidR::add_treetops3d(x,ttops)

  inputAccess <- utils::menu(c("Yes","No"),"Would you like to proceed? (Type a number)")
  if(inputAccess!=1){stop()}

  #localSHP <- read_sf(paste0(parentDir,"\\",folds[count],"\\",gsub("_Michael","",folds[count]),".shp"))
  localSHP <- sf::read_sf(paste0(parentDirectory,"\\",folds[folderCount],"\\boundary.shp"))
  localSHP <- sf::st_zm(localSHP)
  sf::st_transform(ttops,sf::st_crs(localSHP))
  sf::st_crs(ttops) <- sf::st_crs(localSHP)
  ttops1 <- sf::st_intersection(ttops,localSHP)
  ttops <- ttops[ttops$treeID %in% ttops1$treeID,]
  names(ttops)[1] <- 'NAME'
  ttops$NAME <- as.character(ttops$NAME)
  # Populate naming section
  lasRange <- range(las$Z)
  lasThresh <- (lasRange[2]*0.75)+(lasRange[1]*(1-0.75)) # Cut off to seperate L0 and L1
  for(i in c(1:nrow(ttops))){
    if(isTRUE(ttops$Z[i]>lasThresh)){
      ttops$NAME[i] <- paste0(overstory,"_1.00")
    }
    else{
      ttops$NAME[i] <- paste0(understory,"_1.00")
    }
  }
  # Remove any stem < 5m
  minLas <- min(las$Z)
  ttops <- ttops[which(ttops$Z >= minLas+minHeight),]
  ttops$COLOR <- 255
  ttops$COLOR <- as.integer(ttops$COLOR)
  ttops$COMMENT <- ""

  sf::write_sf(ttops,paste0(parentDirectory,"\\",folds[folderCount],"\\treeTops.shp"),layer_options="SHPT=POINTZ")
}
