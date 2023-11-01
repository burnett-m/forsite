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

## GT QC
This tool renames your folder to include your name at the end (if not already done so) and visualizes all of the GT Boxes you've included in that folder (up until 120, and then stops before the RGL windows forces R to crash).

First, copy your working directory (your folder within the 'inProgress' folder) to your clipboard. Add it to a variable named 'parentDir'

`parentDir <- readClipboard()`

`gtQC(parentDir)`

Then, you have the option to view the buffered or unbuffered LAZ. The default selection is to view the unbuffered, but if you would like to view the buffered LAZ, add `buffered = TRUE`. For example, `gtQC(parentDir,buffered = TRUE)`

