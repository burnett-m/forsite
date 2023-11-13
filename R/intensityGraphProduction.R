#' Intensity Graph Production
#'
#' This tool uses the merged LAS and merged SHP to produce an overall intensity graph. You will need an output directory which will store all  raw intensity files along with plots and the final tables indicating the number points and shapes per species and the intensity histogram data. (Option for future : it would be easy to include an option for merging results between different project areas and projections, if need be)
#'
#' @param agtLAS string. Location of merged AGT LAS/LAZ file. Right click on the file name while holding down the "SHIFT" key. Select "Copy as path". In the R console, type "readClipboard()" and manually manipulate the directory so that it is a string between two quotation marks without the final backslash.
#' @param agtSHP string. Location of merged AGT shapefile. Please follow the same instructions as agtLAS for inputting agtSHP.
#' @param outDir string. Output directory to store raw intensity data, raw histogram data, species table, and plots. After creating this output directory, select the file path at the top of the File Explorer window and simply type into the console - outDir <- readClipboard()
#' @param aoiName string. The name of the project AOI. This will be included as a title in all of the plots.
#' @param decid string list. List of deciduous species (NOTE : Please check the default list and if any species is not included in it, or incorrectly labelled deciduous, you will need to manually set this list).
#' @param conifer string list. List of coniferous species (NOTE : Please check the default list and if any species is not included in it, or incorrectly labelled conifer, you will need to manually set this list).
#' @param dead string list. List of dead species (NOTE : Please check the default list and if any species is not included in it, or incorrectly labelled dead, you will need to manually set this list).
#'
#' @return Four plots displaying the intensity distribution of all species within the AOI
#' @export intensityGraphProduction
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics hist
#' @importFrom stats setNames
#' @importFrom utils write.csv
#' @importFrom dplyr bind_cols
#'
#' @examples
#' # Copy the agtLAS path to clipboard
#' readClipboard()
#' #[1] "\"C:\\Users\\...\\2446_WF_AreaA_AGT_20230915_merged.las\""
#' agtLAS <- "C:\\Users\\...\\2446_WF_AreaA_AGT_20230915_merged.las"
#' # Copy agtSHP path to clipboard
#' readClipboard()
#' #[1] "\"C:\\Users\...\\2446_WF_AreaA_AGT_20230915_mergedInitialShapes.shp\""
#' agtSHP <- "C:\\Users\\...\\2446_WF_AreaA_AGT_20230915_mergedInitialShapes.shp"
#' outDir <- "C:\\IntensityGraphs\\WF_AreaA"
#' aoiName <- "West Frasier Area A"
#' #intensityGraphProduction(agtLAS,agtSHP,outDir,aoiName)
intensityGraphProduction <- function(agtLAS,agtSHP,outDir,aoiName='AGT',decid=c('AL','AW','PB','BW','SY','BA','BE','MA','EL','AH','HO','WO','OK','BC','MB','HB','HL','SM','DC','AC','AT','EP','HW'),conifer=c('FB','LT','PL','SB','SW','SP','CR','WP','PI','CW','FD','SX'),dead=c('DP','DB','DS','DF','SN')){
  print("Reading Data...")
  requireNamespace("dplyr")
  #require(dplyr)
  las <- lidR::readLAS(agtLAS)
  shp <- sf::read_sf(agtSHP)
  lidR::st_crs(las) <- sf::st_crs(shp)

  shp$species <- substr(shp$NAME,nchar(shp$NAME)-1,nchar(shp$NAME))
  shp_unique <- unique(substr(shp$NAME,nchar(shp$NAME)-1,nchar(shp$NAME)))
  shp_list <- dplyr::group_split(shp,species)
  shp_num <- nrow(shp)
  lidR::st_crs(las) <- sf::st_crs(shp)

  # Separate the las by species
  # Initializes the progress bar
  print("Separating LAS by species and outputting raw intensity data...")
  pb <- utils::txtProgressBar(min = 0,  max = length(shp_unique), style = 3, width = 50, char = "=")
  for(i in shp_unique){
    shp_temp <- shp[shp$species==i,]
    shp_temp_zmRm <- sf::st_zm(shp_temp)
    clipped <- lidR::clip_roi(las,shp_temp_zmRm)
    temp_int <- list()
    temp_class <- list()
    for(c_l in c(1:length(clipped))){
      t_i <- as.data.frame(clipped[[c_l]]$Intensity)
      temp_int <- rbind(temp_int,t_i[1])
      t_c <- as.data.frame(clipped[[c_l]]$Classification)
      temp_class <- rbind(temp_class,t_c[1])
    }
    temp_df <- data.frame(temp_int,temp_class)
    colnames(temp_df) <- c('Intensity','Classification')
    temp_df <- temp_df[temp_df$Classification == 1 | temp_df$Classification == 2,]
    if(isFALSE(dir.exists(paste0(outDir,"/RawIntensity")))){dir.create(paste0(outDir,'/',"RawIntensity"))}
    write.csv(temp_df,file=paste0(outDir,"/RawIntensity/",i,"_rawIntensity.csv"))
    utils::setTxtProgressBar(pb, which(shp_unique==i)) # Close the connection
  }
  #beepr::beep(3) # Alert me when it's finished
  close(pb) # Close the connection


  ### Make Frequency Table
  print("Creating frequency table...")
  excel <- openxlsx::createWorkbook()
  csvFiles <- list.files(path=paste0(outDir,"/RawIntensity"),pattern="*rawIntensity.csv")

  # Get intensity max
  i_max <- max(las@data$Intensity)
  # Define the breaks separation
  if(i_max < 5000){by_n <- 200}
  if(i_max >=5000 && i_max <= 25000){by_n <- 1000}
  if(i_max > 25000 && i_max <= 75000){by_n <- 2000}
  if(i_max > 75000){by_n <- 5000}
  # Define the breaks
  br = seq(0,i_max+by_n,by=by_n)

  # Prepare intensity XLSX file
  total <- data.frame()
  for(i in csvFiles){
    temp_data <- data.table::fread(paste0(outDir,"/RawIntensity/",i))
    temp_name <- gsub("_rawIntensity.csv","",i)
    freq = hist(as.numeric(temp_data$Intensity),breaks=br,plot=FALSE)
    df <- data.frame(br[-length(br)],freq$counts)
    names(df) <- c('Bin','Frequency')
    df$Percentage <- (df$Frequency / sum(df$Frequency)) * 100
    df$species <- temp_name
    df_names <- names(df)
    for(n in c(1:length(df_names))){df_names[n] <- paste0(temp_name,'_',df_names[n])}
    names(df) <- df_names
    openxlsx::addWorksheet(excel,paste0(temp_name,'_Histogram'))
    openxlsx::writeData(excel,sheet=paste0(temp_name,'_Histogram'),x = df)
  }
  openxlsx::saveWorkbook(excel,paste0(outDir,'/IntensityHistogram.xlsx'),overwrite=TRUE)

  # Get the number of trees within each species
  print("Outputting species_count.csv")
  spec_sum <- data.frame(matrix(nrow=length(shp_unique),ncol=2))
  colnames(spec_sum) <- c('Species','Count')
  for(i in c(1:length(shp_unique))){
    spec_sum$Species[i] <- shp_unique[i]
    spec_sum$Count[i] <- sum(shp$species==shp_unique[i])
  }
  write.csv(spec_sum,paste0(outDir,'/species_count.csv'),row.names = FALSE)

  # Prepare intensity histograms for display
  sheet <- readxl::excel_sheets(paste0(outDir,'/IntensityHistogram.xlsx'))
  xlDF <- lapply(setNames(sheet,sheet),function(x) readxl::read_excel(paste0(outDir,'/IntensityHistogram.xlsx'),sheet=x)) # Apply sheet names to DF names
  xlDF <- bind_cols(xlDF) # Attach all DF together
  xlDF_percent <- xlDF[grepl('Percentage',names(xlDF))] # Get only percentages
  xlDF_percent$Bin <- seq(0,nrow(xlDF_percent)-1)
  xlDF_percent <- xlDF_percent[-c(1,nrow(xlDF_percent)),]
  # Format DF to be read as multi line plot
  xlDF_final <- data.frame()
  for(i in c(1:ncol(xlDF_percent))){
    if(i == ncol(xlDF_percent)){break} # This shouldn't be necessary
    temp_name <- toupper(substr(names(xlDF_percent[,i]),1,2))
    temp_df <- data.frame(c(xlDF_percent[ncol(xlDF_percent)],xlDF_percent[i]))
    temp_df$SSP <- temp_name
    names(temp_df) <- c('Intensity','Percentage','Species')
    xlDF_final <- rbind(xlDF_final,temp_df)
  }
  xlDF_final$Percentage <- xlDF_final$Percentage * 0.01

  # plot
  #vis <- viridis::viridis_pal()
  vis <- rainbow
  xlDF_vis <- vis(length(unique(xlDF_final$Species))) # Get unique colours from viridis
  ssp_unique <- unique(xlDF_final$Species) # Get unique list of SSP
  # All Species
  plot_all <- ggplot2::ggplot(data=xlDF_final,ggplot2::aes(x=Intensity,y=Percentage,color=Species,group=Species)) +
    ggplot2::geom_line(linewidth=1) + ggplot2::scale_color_manual(values = xlDF_vis) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_hline(yintercept = seq(0.01, max(xlDF_final$Percentage),by=0.01),alpha=0.5) +
    ggplot2::theme_classic() + ggplot2::ggtitle("All Species Intensity Histogram") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5,size=34),axis.text=ggplot2::element_text(size=15),
                   axis.title=ggplot2::element_text(size=22),legend.title=ggplot2::element_text(size=22),legend.text=ggplot2::element_text(size=15))
  # Deciduous
  dc <- subset(xlDF_final,Species %in% ssp_unique[ssp_unique %in% decid]) # Make subset
  plot_dc <- ggplot2::ggplot(data=dc,ggplot2::aes(x=Intensity,y=Percentage,color=Species,group=Species)) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::scale_color_manual(values = xlDF_vis[match(decid, ssp_unique)]) + ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_hline(yintercept = seq(0.01, max(xlDF_final$Percentage),by=0.01),alpha=0.5) +
    ggplot2::theme_classic() + ggplot2::ggtitle(paste0(aoiName," Intensity - Deciduous")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5,size=34),axis.text=ggplot2::element_text(size=15),
                   axis.title=ggplot2::element_text(size=22),legend.title=ggplot2::element_text(size=22),legend.text=ggplot2::element_text(size=15))
  # Conifer
  cn <- subset(xlDF_final,Species %in% ssp_unique[ssp_unique %in% conifer]) # Make subset
  plot_cn <- ggplot2::ggplot(data=cn,ggplot2::aes(x=Intensity,y=Percentage,color=Species,group=Species)) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::scale_color_manual(values = xlDF_vis[match(conifer, ssp_unique)]) + ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_hline(yintercept = seq(0.01, max(xlDF_final$Percentage),by=0.01),alpha=0.5) +
    ggplot2::theme_classic() + ggplot2::ggtitle(paste0(aoiName," Intensity - Coniferous")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5,size=34),axis.text=ggplot2::element_text(size=15),
                   axis.title=ggplot2::element_text(size=22),legend.title=ggplot2::element_text(size=22),legend.text=ggplot2::element_text(size=15))
  # Dead
  de <- subset(xlDF_final,Species %in% ssp_unique[ssp_unique %in% dead]) # Make subset
  plot_de <- ggplot2::ggplot(data=de,ggplot2::aes(x=Intensity,y=Percentage,color=Species,group=Species)) + ggplot2::geom_line(linewidth=1) +
    ggplot2::scale_color_manual(values = xlDF_vis[match(dead, ssp_unique)]) + ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_hline(yintercept = seq(0.01, max(xlDF_final$Percentage),by=0.01),alpha=0.5) +
    ggplot2::theme_classic() + ggplot2::ggtitle(paste0(aoiName," Intensity - Dead")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5,size=34),axis.text=ggplot2::element_text(size=15),
                   axis.title=ggplot2::element_text(size=22),legend.title=ggplot2::element_text(size=22),legend.text=ggplot2::element_text(size=15))

  # Save Plots
  print(paste0("Saving plots in directory : ", outDir, "\\Plots"))
  if(isFALSE(dir.exists(paste0(outDir,"\\Plots")))){dir.create(paste0(outDir,"\\Plots"))}
  ggplot2::ggsave(paste0(outDir,"\\Plots\\AllSpecies_Plot.jpeg"),plot_all,width=20,height=10)
  ggplot2::ggsave(paste0(outDir,"\\Plots\\DeciduousSpecies_Plot.jpeg"),plot_dc,width=20,height=10)
  ggplot2::ggsave(paste0(outDir,"\\Plots\\ConiferSpecies_Plot.jpeg"),plot_cn,width=20,height=10)
  ggplot2::ggsave(paste0(outDir,"\\Plots\\DeadSpecies_Plot.jpeg"),plot_de,width=20,height=10)

}
