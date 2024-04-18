# Pinning Plots function
#' Pinning Plots
#'
#' This function will help build a foundation for pinning your plots. It is not perfect, and it will not capture any of the understory, but it will help to get you started.
#'
#' @param parentDirectory string: The parent directory of your InProgress folder. The final folder in this directory must be your name (example:"...GT_Boxes/_inProgress/Michael). The easiest way to use this is to copy the directory (in Windows File Explorer, copy the entire directory at the top of the window, and in your R window, assign it to a variable like this : parentDir <- readClipboard(). Then use the 'parentDir' variable in this place).
#' @param folderCount numerical: Since you will be working one folder at a time, start with the number 1 and after you have finished pinning that plot, use the number 2 (or if you immediately move that first plot to the Final folder, you can use 1 again).
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
pinningPlots <- function(parentDirectory, folderCount, overstory="CN", understory="CU", windowSize=2.8, minHeight=5){
  setwd(parentDirectory) # set working directory
  folds <- list.files() # list folders

  # Get user information
  directories <- sapply(strsplit(parentDirectory,"\\\\"),"[") # Get list of folders in directory
  user <- directories[length(directories)] # Select last directory for user name

  # Add user name to end of directory
  if(isFALSE(grepl(user,folds[folderCount]))){
    file.rename(folds[folderCount],paste0(folds[folderCount],"_",user))
  }
  folds <- list.files()
  print(folds[folderCount]) # Print folder name to console

  # Run these commands if there's no boundary file
  ## boundaryCommand <- paste0("lasboundary -i ",gsub(paste0("_",user),"",folds[folderCount]),".las",
  ##                           " -o ",r"{boundary.shp}")
  ## shell(paste0("cd ",folds[folderCount]," && ",boundaryCommand))

  # Get either LAS or LAZ files
  if(file.exists(paste0(parentDirectory,"\\",folds[folderCount],"\\",gsub(paste0("_",user),"",folds[folderCount]),".las"))){
    unbufferedLAS <- lidR::readLAS(paste0(parentDirectory,"\\",folds[folderCount],"\\",gsub(paste0("_",user),"",folds[folderCount]),".las")) # Read unbuffered LAS
  }
  else{unbufferedLAS <- lidR::readLAS(paste0(parentDirectory,"\\",folds[folderCount],"\\",gsub(paste0("_",user),"",folds[folderCount]),".laz"))} # Read unbuffered LAS

  boundarySF <- lidR::st_concave_hull(unbufferedLAS) # Make boundary for clipping

  if(file.exists(paste0(parentDirectory,"\\",folds[folderCount],"\\",gsub(paste0("_",user),"",folds[folderCount]),"_buffered.las"))){
    las <- lidR::readLAS(paste0(parentDirectory,"\\",folds[folderCount],"\\",gsub(paste0("_",user),"",folds[folderCount]),"_buffered.las")) # Read buffered LAS to access treetops from it
  }
  else{las <- lidR::readLAS(paste0(parentDirectory,"\\",folds[folderCount],"\\",gsub(paste0("_",user),"",folds[folderCount]),"_buffered.laz"))} # Read buffered LAS to access treetops from it

  ttops <- lidR::locate_trees(las,lidR::lmf(ws=windowSize,hmin = minHeight)) # Locate tree tops
  x <- lidR::plot(las,bg="white",size=4) # Plot LAS
  lidR::add_treetops3d(x,ttops) # Plot tree tops

  # Take a break to give us a chance to review tree tops
  inputAccess <- utils::menu(c("Yes","No"),"Would you like to proceed? (Type a number)")
  if(inputAccess!=1){stop()}

  #localSHP <- read_sf(paste0(parentDir,"\\",folds[count],"\\",gsub("_Michael","",folds[count]),".shp"))
  #localSHP <- sf::read_sf(paste0(parentDirectory,"\\",folds[folderCount],"\\boundary.shp"))
  localSHP <- sf::st_zm(boundarySF) # Remove Z
  localSHP_PRJ <- is.na(sf::st_crs(localSHP))
  if(isFALSE(localSHP_PRJ)){ # Only follow next step if there is a coordinate system in LAS
    sf::st_transform(ttops,sf::st_crs(localSHP)) # Reproject
  }
  sf::st_crs(ttops) <- sf::st_crs(localSHP) # Copy projection in case first option didn't succeed

  ttops1 <- sf::st_intersection(ttops,localSHP) # Clip points to boundary
  ttops <- ttops[ttops$treeID %in% ttops1$treeID,] # Only keep elements within boundary
  names(ttops)[1] <- 'NAME' # Rename column
  ttops$NAME <- as.character(ttops$NAME)
  # Populate naming section
  dtm <- lidR::rasterize_terrain(las,res=1,algorithm=lidR::tin()) # Create DTM from LAS
  ttops$DTM <- terra::extract(dtm,ttops)[,2] # Get DTM cell value at pixel location
  ttops$NormHeight <- ttops$Z - ttops$DTM # Access normalized height
  MeanHeight_ttops <- mean(ttops$NormHeight)
  for(i in c(1:nrow(ttops))){ # Name OS and US
    if(isTRUE(ttops$NormHeight[i]>MeanHeight_ttops)){
      ttops$NAME[i] <- paste0(overstory,"_1.00")
      ttops$SPECIES[i] <- overstory
    }
    else{
      ttops$NAME[i] <- paste0(understory,"_1.00")
      ttops$SPECIES[i] <- overstory
    }
  }

  # lasRange <- range(las$Z) # Get range of LAS
  # lasThresh <- (lasRange[2]*0.75)+(lasRange[1]*(1-0.75)) # Cut off to seperate L0 and L1
  # for(i in c(1:nrow(ttops))){ # Name OS and US
  #   if(isTRUE(ttops$Z[i]>lasThresh)){
  #     ttops$NAME[i] <- paste0(overstory,"_1.00")
  #   }
  #   else{
  #     ttops$NAME[i] <- paste0(understory,"_1.00")
  #   }
  # }
  # Remove any stem < 5m
  # minLas <- min(las$Z)
  # ttops <- ttops[which(ttops$Z >= minLas+minHeight),]
  ttops <- ttops[which(ttops$NormHeight >= minHeight),]

  ttops$COLOR <- 255 # Add color column for Fugro Viewer
  ttops$COLOR <- as.integer(ttops$COLOR)
  ttops$COMMENT <- "" # Add comment column for Fugro Viewer
  ttops$Visibility <- "1.00"
  for(i in c(1:nrow(ttops))){ttops$POI_INDEX[i] <- i}
  ttops$Edge <- ""

  sf::write_sf(ttops,paste0(parentDirectory,"\\",folds[folderCount],"\\treeTops.shp"),layer_options="SHPT=POINTZ") # Write SHP
  if(isTRUE(localSHP_PRJ)){ # Only follow next step if there is a coordinate system in LAS
    file.remove(paste0(parentDirectory,"\\",folds[folderCount],"\\treeTops.prj"))
  }

  # Zip files in easy to import file
  setwd(paste0(parentDirectory,"\\",folds[folderCount]))
  files4Zip <- list.files(pattern="treeTops*")
  zip("treeTops.zip",files4Zip)
  file.remove(files4Zip)
}
