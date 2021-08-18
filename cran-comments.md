---
title: CRAN package `heatmapFlex`
---

## Resubmission 2021-08-18
This is a resubmission. The version was increased to 0.1.2 after addressing the comments by CRAN staff member Julia Haider:

* added missing Rd-tags: zoom_heatmap.Rd: \value

In addition, I made minor corrections and additions to the documentation.

## Resubmission 2021-08-17
This is a resubmission. The version was increased to 0.1.1 after addressing the comments by CRAN staff member Julia Haider:

* added () behind all function names in the description texts (DESCRIPTION file) and removed the `\code{}` markup
* omitted examples using un-exported functions (`heatmapFlex:::draw_heatmap(dl)`)
* removed `\dontrun{}` from examples as it was not necessary
* added calls of `on.exit()` after making changes to graphical parameters (`par`) to ensure that the settings are reset when the function is exited.
  - __NOTE__: Changes are reset selectively as nested calls make selective changes to graphical parameters and resetting all parameters in every function will destroy the heatmap. Instead, default graphical parameters are captured and reset on exit in the main function `heatmap.n2()`.

In addition, I have made the following changes/additions:

* added a vignette
* added an example
* made corrections and additions to the documentation
* added functions `zoom_heatmap()` and `draw_heatmap()` to exports to enable zooming and give the user the opportunity to customise an existing "display list".

## Test environments
* local OS X install: x86_64-apple-darwin17.0, R 4.0.2
* win-builder (devel and release)
* CentOS Linux release 7.9.2009 (Core) [:core-4.1-amd64:core-4.1-noarch], R 4.0.4

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

```
R CMD check --as-cran heatmapFlex_0.1.2.tar.gz
.
* checking CRAN incoming feasibility ... NOTE     
Maintainer: ‘Vidal Fey <vidal.fey@gmail.com>’
.
```
