# heatmapS
Tools for producing flexible heatmaps, inclduding zooming, splitting, picket plots, etc.

## License
[GPL-3](https://cran.r-project.org/web/licenses/GPL-3)

## Description
The package has a number of tools supporting more flexible heatmaps. The graphics is grid-like using the old graphics system. The main function is \code{heatmap.n2}, which is a wrapper around the various functions constructing individual parts of the heatmap, like sidebars, picket plots, legends etc. The function supports zooming and splitting, i.e., having (unlimited) small heatmaps underneath each other in one plot deriving from the same data set, e.g., clustered and ordered by a supervised clustering method.

## Installation
### CRAN
install.packages("heatmapS")
### Latest development version
install.packages("devtools")  
devtools::install_github("vfey/heatmapS")

## Usage

