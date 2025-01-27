#' get Basin Cells from Flow Graph
#'
#' Derive all inflow cells to a given cell number of a flow direction raster
#'
#' @param cell cell number
#' @param flowgraph igraph object with flow network topology
#'
#' @return vector of cell numbers
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' data("volcano")
#' v <- raster(volcano, xmn=2667400, xmx=2668010, ymn=6478700, ymx=6479570, crs="+init=epsg:27200")
#' fd <- terrain(v, opt = "flowdir")
#' fdGraph <- GRDCFlowTools::createFlowGraph(fd)
#' getBasin <- GRDCFlowTools::getBasinCells(4123, fdGraph)
#' }


getBasinCells <- function(cell, flowgraph) {
  cell <- cell[which(as.character(cell) %in% names(igraph::V(flowgraph)))]

  message("...")
  ego <- igraph::make_ego_graph(flowgraph,
                                1000000,
                                mindist = 1,
                                as.character(cell),
                                mode="in")
  cells <- unlist(lapply(ego,FUN = function(x)  {
    as.numeric(names(igraph::V(x)))
  }))
  return(cells)
}
