# heatmapS
Tools for producing flexible heatmaps, inclduding zooming, splitting, picket plots, etc.

## License
[GPL-3](https://cran.r-project.org/web/licenses/GPL-3)

## Description
The package has a number of tools supporting more flexible heatmaps. The graphics is grid-like using the old graphics system. The main function is \code{heatmap.n2}, which is a wrapper around the various functions constructing individual parts of the heatmap, like sidebars, picket plots, legends etc. The function supports zooming and splitting, i.e., having (unlimited) small heatmaps underneath each other in one plot deriving from the same data set, e.g., clustered and ordered by a supervised clustering method.

## Installation
### CRAN
`install.packages("heatmapS")`
### Latest development version
```
install.packages("devtools")  
devtools::install_github("vfey/heatmapS")
```

## Usage
### A simple example
Generate a random 10x10 matrix, order it using default clustering methods and split it into each 2 groups along both rows and columns:
```
mat <- matrix(c(rnorm(50, mean = 1), rnorm(50, mean = -1)), nrow = 10)
dl <- heatmap.n2(mat, col = "BuWtRd", rowMembers=rep(1:2, each=5), colMembers=rep(1:2, each=5),
  labRow=paste0("gene-", 1:10), labCol=paste0(c("A", "B"), rep(1:5, 2)), r.cex=0.8)
```
![simple_example_heatmap](https://user-images.githubusercontent.com/69206181/129165169-de5059a4-5957-44df-b18d-5421ed9d4776.png)
