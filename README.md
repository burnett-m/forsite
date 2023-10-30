# forsite - R Package
![license](https://img.shields.io/badge/License-R--package-green) 

R package for the modelling team at Forsite


## Installation guide
You will need to have LASTools already installed on your machine. Please visit the [LASTools webpage](https://lastools.github.io/) to get the latest version. Then you will need to add it to your PATH. 

`install.packages('remotes')`

Also install the tibble package separately since for some reason, that sf package dependency doesn't download on its own.

`install.packages('tibble')`

`remotes::install_github('burnett-m/forsite')`

Now that the library has been installed on your machine, now import the library
`library(forsite)`


## Pinning Plots
First copy your working directory (your folder within the 'inProgress' folder) to your clipboard. Add it to a variable named 'parentDir'

`parentDir <- readClipboard()`

Then, using the first folder in the list of folders (it counts based off of the first digit, so if you have two plots in your working directory which are plots 8 and 41, plot41 will be first since R lists folders based off of the first digit), use the pinningPlots function with your username.

`pinningPlots(parentDir,1,'Michael')`

If you want to change the overstory and understory, add `understory='DC'` to the function after your username. Same for windowSize and minHeight. For more information about the function, type `?pinningPlots` to the R console.