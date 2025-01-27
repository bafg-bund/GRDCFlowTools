#' get Basin Cells from Split Flow Graph
#'
#' Derive all inflow cells to a given cell number of a splitted flow direction raster
#'
#' @param cell cell number
#' @param resultList list returned by function createSplitFlowGraph
#' @param index force function to search in graph with this index
#'
#' @return vector of cell numbers
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' data("volcano")
#' fd <- terrain(v, opt = "flowdir")
#' tempfilename <- paste0(tempfile(),".asc")
#' writeRaster(fd, tempfilename)
#' fdSplitGraph <- GRDCFlowTools::createSplitFlowGraph(tempfilename, ny = 2, rootpath = tempdir())
#' getBasin <- GRDCFlowTools::getBasinFromSplitGraph(4123, fdSplitGraph)
#' }
getBasinFromSplitGraph <- function(cell, resultList, index = NULL) {


  if (is.null(index)) {
    index <- which(sapply(resultList, FUN = function(x, thiscell = cell[1]) {
      (thiscell >= x$nmin && thiscell <= x$nmax)
    }))
  }

  message(paste0("Index: ", index))

  message(paste0("read: ", resultList[[index]]$path))
  gg <- readRDS(resultList[[index]]$path)

  if (any(as.character(cell) %in% names(igraph::V(gg)))) {

    cell <- cell[which(as.character(cell) %in% names(igraph::V(gg)))]

    message("...")
    ego <- igraph::make_ego_graph(gg,
                                  1000000,
                                  mindist = 1,
                                  as.character(cell),
                                  mode="in")
  } else {
    return(NA)
  }
  rm(gg)

  cells <- unlist(lapply(ego,FUN = function(x)  {
    as.numeric(names(igraph::V(x)))
  }))


  message("found ", length(cells)," cells")




  if (index < length(resultList)) {
    lowerBoundaryCells <- cells[which(as.numeric(cells) < resultList[[index]]$bmin)]
    if (length(lowerBoundaryCells) > 0) {
      morecells <- getBasinFromSplitGraph(lowerBoundaryCells, resultList, index = index+1)
      if(!all(is.na(morecells))) cells <- c(cells, morecells)
    }
  }

  if (index > 1) {
    upperBoundaryCells <- cells[which(as.numeric(cells) > resultList[[index]]$bmax)]
    if (length(upperBoundaryCells) > 0) {
      morecells <- getBasinFromSplitGraph(upperBoundaryCells, resultList, index = index-1)
      if(!all(is.na(morecells))) cells <- c(cells, morecells)
    }
  }

  return(cells)
}


