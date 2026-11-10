#' Generate Pinning Plots: Place Plots
#'
#' This script is not an improvement upon the existing python script, but an alternative. You may use it after rasterizing the vegetation inventory using `rasterize_vegInv()`
#'
#' @param inputRasters Character. Full paths to a list of all input rasters (CHM, Intensity, Point Density, and any other overviews merged raster **BUT NOT THE vegetation inventory**)
#' @param output Character. Full path to the output directory
#' @param vegInv Character. Full path to the vegetation inventory raster (if it's available)
#' @param sStrata Integer. Number of Stratas. Default number is 5
#' @param nSamples Integer. Number of samples to produce for each raster input (default is 100).
#' @param minHeight Double. Minimum height (default is 5)
#' @param multipleStrataSets Boolean. Set to TRUE if you would like to generate one set of plots stratified to each input raster. Set to FALSE (default setting) to stack every raster and generate set of plots for all the input rasters.
#' @param allocation Character. Allocation algorithm to be used. Either prop (default) for proportional allocation, optim for optimal allocation (equal sampling cost)  -  NOTE: This method is far more computionally expensive, only use for small project AOIs, equal for equal number of samples (defined by nSamples) for each strata.
#' @param method Character. "random" (default) performs traditional stratified random sampling where probability to sample each cell within each stratum is equal assuming default parameters. "Queinnec" is performed in 2 stages: Rule 1 - Sample within grouped stratum pixels defined within the wrow, wcol parameters (each set to 3); Rule 2 - If no samples exist to satisfy Rule 1 individual stratum pixels are sampled. The rule applied to allocate each sample is defined in the rule attribute of output samples. Random is the superior option for smaller AOIs. If the sraster provided contains factor values, the algorithm will automatically convert these into the numeric factor levels and perform sampling using those values. The categories (factor values) will be extracted and appended to the algorithm output as the 'category' attribute.
#'
#' @returns Stratified plot locations and plot outlines for seg cal plots. This script can be followed with `generatePinningPlots_CreatePlotFolders()` to prepare the plot folders for pinning
#' @export
#'
#' @examples
#' #rasterTiffs <- c("C:\\Forsite\\Sakaw\\Sakaw_SFVI_Dissolved.tif",
#' #"C:\\Forsite\\Sakaw\\veg\\overviews\\CHM_OVERVIEW_COG.tif",
#' #"C:\\Forsite\\Sakaw\\veg\\overviews\\INTENSITY_OVERVIEW_COG.tif","C:\\Forsite\\Sakaw\\veg\\overviews\\POINTCOUNT_OVERVIEW_COG.tif")
#' #generatePinningPlots_PlacePlots(rasterTiffs, "C:\\Forsite\\Sakaw\\temp")
#'
generatePinningPlots_PlacePlots <- function(inputRasters,output,vegInv=NULL,sStrata=3,nSamples=100,minHeight=5,multipleStrataSets=FALSE,allocation="prop", method="random"){
  number_of_rasters <- length(inputRasters)
  results <- vector("list", number_of_rasters) # Make variable to save results in a list

  if(isTRUE(multipleStrataSets)){
    message("Generating one set of samples for each raster.")
    for(tif in seq_along(inputRasters)){
      message(paste0("Processing ",inputRasters[tif]))
      r <- terra::rast(inputRasters[tif]) # Read raster
      sr <- sgsR::strat_quantiles(mraster = r,nStrata = sStrata, plot=FALSE) # Generate nStrata number of stratas for each raster
      terra::plot(sr,main=paste("Raster",basename(inputRasters[tif]),"- Strata")) # Display raster stratas
      samples <- sgsR::sample_strat(sraster=sr,nSamp=nSamples,method="random",allocation="equal",plot=TRUE) # Place samples for each strata
      results[[tif]] <- list(strata = sr,plots=samples) # Save results in variable
    }
    names(results) <- tools::file_path_sans_ext(basename(inputRasters))
    if(!is.null(vegInv)){
      message(paste0("Processing vegetation inventory raster."))
      overview_extent <- terra::ext(r)
      veg_r <- terra::rast(vegInv)
      veg_r <- terra::resample(veg_r, r, method = "bilinear")
      veg_r <- terra::project(veg_r,r,method="bilinear")
      sr <- sgsR::strat_quantiles(mraster = veg_r,nStrata = sStrata, plot=FALSE)
      results[[tif+1]] <- sr # Store results in list
      names(results)[tif+1] <- paste0("strata_",basename(vegInv))
    }
    message("Finished processing the rasters...")

    chm <- terra::rast(inputRasters[grepl("CHM_OVERVIEW",inputRasters)]) # Read CHM

    # Prepare plots based on minimum height
    for(i in seq_along(results)){
      message(paste0("Preparing plots for ",names(results)[i],"..."))
      samples <- results[[i]]$plots # Access plot locations
      # Write points SHP
      terra::writeVector(terra::vect(samples),file.path(output,paste0(names(results)[i],"_samples.shp")),filetype="ESRI Shapefile",overwrite=TRUE)
      # Create plots
      plot_points <- sf::read_sf(file.path(output,paste0(names(results)[i],"_samples.shp")))
      plotOutlines <- sf::st_buffer(plot_points,11.28) # Make plot outlines
      message("Extracting max CHM values for each plot...")
      max_vals <- terra::extract(chm,plotOutlines,fun=max,na.rm=TRUE) # Get max CHM value from plot outline
      plotOutlines$CHM_max <- max_vals[[2]] # Add max CHM value to outlines shp
      message(paste0("There are ",as.character(sum(plotOutlines$CHM_max > minHeight,na.rm=TRUE))," plots above the minimum height. These plots will be generated."))
      plotOutlines <- plotOutlines[plotOutlines$CHM_max > minHeight,] # Keep only plots above minHeight
      sf::write_sf(plotOutlines,file.path(output,paste0(names(results)[i],"_plotOutlines.shp"))) # Save plot outlines
      message(paste0("Wrote ",file.path(output,paste0(names(results)[i],"_plotOutlines.shp"))))
    }
  }
  if(isFALSE(multipleStrataSets)){
    message("One set of samples from the stacked input rasters being processed.")
    for(tif in seq_along(inputRasters)){
      message(paste0("Processing ",inputRasters[tif]))
      r <- terra::rast(inputRasters[tif])
      sr <- sgsR::strat_quantiles(mraster = r,nStrata = sStrata, plot=FALSE)
      results[[tif]] <- sr # Store results in list
    }
    names(results) <- paste0("strata_",basename(inputRasters)) # rasters must have "strata" prefix

    if(!is.null(vegInv)){ # Complete this section if there's a vegetation inventory
      message(paste0("Processing vegetation inventory raster."))
      veg_r <- terra::rast(vegInv)
      if(terra::crs(veg_r) != terra::crs(r)){
        message(basename(vegInv)," is in a different projection compare to overviews. Reprojecting...")
        veg_r <- terra::project(veg_r,r)
      }
      veg_r <- terra::resample(veg_r, r, method = "bilinear")
      veg_r <- terra::project(veg_r,r,method="bilinear")
      sr <- sgsR::strat_quantiles(mraster = veg_r,nStrata = sStrata, plot=FALSE)
      print(sr)
      results[[tif+1]] <- sr # Store results in list
      names(results)[tif+1] <- paste0("strata_",basename(vegInv))
    }
    strataList <- lapply(results, function(x) x$strata) # Reconfigure results
    combinedStrata <- sgsR::strat_map(sraster=strataList,stack=TRUE,plot=TRUE)#,filename="combined_strata.tif",overwrite=TRUE)
    message("Generating samples...")
    samples <- sgsR::sample_strat(sraster = combinedStrata$strata, nSamp = nSamples, allocation = allocation, method=method, plot=TRUE, wrow=3, wcol=3)
    sf::write_sf(samples, file.path(output,"StrataSamplePlotLocations.shp"))
    message(paste0("Samples saved to ",file.path(output,"StrataSamplePlotLocations.shp"),". Reading CHM..."))
    chm <- terra::rast(inputRasters[grepl("CHM_OVERVIEW",inputRasters)]) # Read CHM
    vals <- terra::extract(chm, samples)
    samples$CHM <- vals[[2]]
    plot_points_valid <- samples[!is.na(samples$CHM),]
    plotOutlines <- sf::st_buffer(plot_points_valid,11.28)
    message("Extracting max CHM values for each plot...")
    max_vals <- terra::extract(chm,plotOutlines,fun=max,na.rm=TRUE)
    plotOutlines$CHM_max <- max_vals[[2]]
    message(paste0("There are ",as.character(sum(plotOutlines$CHM_max > minHeight,na.rm=TRUE))," plots above the minimum height. These plots will be generated."))
    plotOutlines <- plotOutlines[plotOutlines$CHM_max > minHeight,]
    sf::write_sf(plotOutlines,file.path(output,"StrataSample_plotOutlines.shp")) # Save plot outlines
    message(paste0("Wrote ",file.path(output,"StrataSample_plotOutlines.shp")))
  }

}

