# forsite - R Package
![license](https://img.shields.io/badge/License-R--package-green) 

R package for the modelling team at Forsite


## Installation guide
`install.packages('remotes')`

Also install the tibble package separately since for some reason, that sf package dependency doesn't download on its own.

`install.packages('tibble')`

`remotes::install_github('burnett-m/forsite')`

Now that the library has been installed on your machine, now import the library
`library(forsite)`


## Pinning Plots
First copy your working directory (your folder within the 'inProgress' folder) to your clipboard. Add it to a variable named 'parentDir'

`parentDir <- readClipboard()`

Then, use the first folder in the list of folders (it counts based off of the first digit, so if you have two plots in your working directory which are plots 8 and 41, plot41 will be first since R lists folders based off of the first digit).

`pinningPlots(parentDir,1)`

If you want to change the overstory and understory, add `understory='DC'` to the function after your username. Same for windowSize and minHeight. For more information about the function, type `?pinningPlots` to the R console.

`pinningPlots(parentDir,1,overstory='DC')`

## viewAllLAZ
This tool allows the user to view all LAZ/LAS files within a directory one-by-one. Use `?viewAllLAZ` for a full description of how to use it.

## GT QC (defunct)
This tool renames your folder to include your name at the end (if not already done so) and visualizes all of the GT Boxes you've included in that folder (up until 120, and then stops before the RGL windows forces R to crash).

First, copy your working directory (your folder within the 'inProgress' folder) to your clipboard. Add it to a variable named 'parentDir'

`parentDir <- readClipboard()`

`gtQC(parentDir)`

Then, you have the option to view the buffered or unbuffered LAZ. The default selection is to view the unbuffered, but if you would like to view the buffered LAZ, add `buffered = TRUE`. For example, `gtQC(parentDir,buffered = TRUE)`

## Intensity Graph Production (defunct)
This tool uses the merged LAS and shapefile containing all of the AGT stems merged within it to produce intensity graphs representing the intensity distribution amongst deciduous, conifer, dead, and all species. Please follow the instructions carefully to input the files correctly.

For both *agtLAS* and *agtSHP* inputs, you will need to hold the "SHIFT" key down as you select the appropriate files (merged LAS and merged SHP), and DO NOT immediately type `agtLAS <- readClipboard()`. This will not input this file correctly. To see why, you must first type `readClipboard()` into the console to see how the path to that file is displayed. There are an extra set of quotation marks and an extra backslash that must be ommitted. You will need to do this for both files, and manually copy and paste it into the variable name (that ends with the file suffix, not a backslash).

The output directory can be selected by simply creating a directory, selecting the file path at the top of the File Explorer window, and in the console, typing `outputDirectory <- readClipboard()`. This directory will contain all of the raw intensity files within a  **RawIntensity** folder. The raw histogram summary and a speciesCount.csv (containing number of shapes and points for each species) file will be exported to the output directory. The plots will be outputted to the **Plots** directory

Please use an appropriate AOI name that will be used in the plot titles.

Before continuing, please check the list of *decid*, *conifer*, and *dead* by typing in the console `?intensityGraphProduction`. I have inputted a default list of species I believe to be unique to each category, but future projects may reuse some of those abbreviations for a different species. Or, one of the species in your AOI may be absent from that list. In which case, you should input the list manually with a `c()`. For example, `dead <- c('SN','DA')`. If all of the species in the default lists are appropriate, you can leave these variables alone.

`intensityGraphProduction(agtLAS, agtSHP, outputDirectory, "Current Project", conifer = c("SP","FB","FD"))`
