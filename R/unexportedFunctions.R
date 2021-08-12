# Un-exported functions from other packages needed internally
## Some methods for the heatmap were rewritten from scratch making use of functions not exported from the source package.
## We have to "borrow" those functions and make them "our own" to avoid warnings as the use of ':::' is not allowed in CRAN.
## For this we use 'utils::getFromNamespace' and add the utils package to Imports in the DESCRIPTION file and roxygen definition.

#' @noRd
get_unexported <- function(fun, pkg)
{
  utils::getFromNamespace(fun, pkg)
}
