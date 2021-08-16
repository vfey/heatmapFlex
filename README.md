# heatmapFlex
Tools for producing flexible heatmaps, inclduding zooming, splitting, picket plots, etc.

## License
[GPL-3](https://cran.r-project.org/web/licenses/GPL-3)

## Description
The package has a number of tools supporting more flexible heatmaps. The graphics is grid-like using the old graphics system. The main function is \code{heatmap.n2}, which is a wrapper around the various functions constructing individual parts of the heatmap, like sidebars, picket plots, legends etc. The function supports zooming and splitting, i.e., having (unlimited) small heatmaps underneath each other in one plot deriving from the same data set, e.g., clustered and ordered by a supervised clustering method.

## Installation
### CRAN
`install.packages("heatmapFlex")`
### Latest development version
```
install.packages("devtools")  
devtools::install_github("vfey/heatmapFlex")
```

## Usage
### A simple example
Generate a random 10x10 matrix and plot it using default values (which admittedly is not pretty):
```
mat <- matrix(rnorm(100), nrow = 10)
dl <- heatmap.n2(mat)
```
![simple_example_heatmap](https://user-images.githubusercontent.com/69206181/129168686-c706f02f-cb07-45b9-ab58-a703a09f41ae.png)

### A split heatmap
Generate a random 10x10 matrix with two distinct sets, order it using default clustering methods, split it into each two groups along both rows and columns and adjust colour palette and dendrogram dimensions:
```
mat <- matrix(c(rnorm(50, mean = 1), rnorm(50, mean = -1)), nrow = 10)
dl <- heatmap.n2(mat, col = "BuWtRd", rowMembers=rep(1:2, each=5), colMembers=rep(1:2, each=5),
  labRow=paste0("gene-", 1:10), labCol=paste0(c("A", "B"), rep(1:5, 2)), r.cex=0.8,
  dendroheight = lcm(2.2), dendrowidth = lcm(2.4))
```
![split_adjust_example_heatmap](https://user-images.githubusercontent.com/69206181/129165169-de5059a4-5957-44df-b18d-5421ed9d4776.png)

### A zoomed heatmap
Generate a random 10x10 matrix with two distinct sets and plot it using the same colour palette  and adjustments as in the previous example. After it has been plotted to a screen graphics device and calling \code{zoom_heatmap} it can be zoomed into by clicking two distinct points inside the plot.
```
mat <- matrix(c(rnorm(50, mean = 1), rnorm(50, mean = -1)), nrow = 10)
# IMPORTANT: Assign the plot parameters to an object that may be used by 'zoom_heatmap'.
dl <- heatmap.n2(mat, col = "BuWtRd", labRow=paste0("gene-", 1:10), labCol=paste0(c("A", "B"), rep(1:5, 2)),
                 r.cex=0.8, dendroheight = lcm(2.2), dendrowidth = lcm(2.4))
zoom_heatmap(dl)
```

![zoomed_example_heatmap](https://user-images.githubusercontent.com/69206181/129567742-66d35168-cf07-4c52-9e05-8d6b56682637.png)
