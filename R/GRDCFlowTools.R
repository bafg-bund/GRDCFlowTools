#' GRDCFlowTools: A package for deriving upstream basins from flow direction maps
#'
#' The package comprises these functions:
#' createFlowGraph
#' createSplitFlowGraph
#' getBasinCells
#' getBasinCellsSplitGraph
#' getWatershed
#'
#' @examples
#' \dontrun{
#' library(raster)
#' data("volcano")
#' v <- raster(volcano, xmn=2667400, xmx=2668010, ymn=6478700, ymx=6479570, crs="+init=epsg:27200")
#' fd <- terrain(v, opt = "flowdir")
#' tempfilename <- paste0(tempfile(),".asc")
#' raster::writeRaster(fd, filename = tempfilename)
#' rl <- GRDCFlowTools::createSplitFlowGraph(fdirpath = tempfilename, ny = 20, rootpath = tempdir())
#' GRDCFlowTools::getBasinFromSplitGraph(cell = 4123, resultList = rl)
#' }
#'
#' @docType package
#' @name GRDCFlowTools
NULL
#> NULL