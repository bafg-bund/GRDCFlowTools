#' igraph object from flow direction raster
#'
#' Creates an igraph graph from a flow direction raster
#'
#' @param fdir a raster object with flow directions (1,2,4,8,16,32,64,128)
#' @param discharge optional: raster object with discharge values
#'
#' @return object of class igraph with flow network topology
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
#' }


createFlowGraph <- function(fdirpath, discharge=NULL) {
  library(raster)
  fdir <- raster(fdirpath)
  dataType(fdir) <- "INT2S"
  ncol <- ncol(fdir)
  nrow <- nrow(fdir)
  xmn <- xmin(fdir)
  xmx <- xmax(fdir)
  ymn <- ymin(fdir)
  ymx <- ymax(fdir)


  rasterTemplate <- raster::raster(ncols = ncol,
                                   nrows = nrow,
                                   xmn = xmn,
                                   xmx = xmx,
                                   ymn = ymn,
                                   ymx = ymx)
  raster::values(rasterTemplate) <- c(1:(ncol*nrow))

  fdir.target <- rasterTemplate

  directions <- c(1, 2, 4, 8, 16, 32, 64, 128)
  shift <- c(1, ncol+1, ncol, ncol-1, -1, -ncol-1, -ncol, -ncol+1)

  fdir.values <- raster::values(fdir)

  for (i in seq_along(directions)) {
    fdir.target[which(fdir.values == directions[i])] <-
      fdir.target[which(fdir.values == directions[i])] + shift[i]

  }


  c1 <- as.character(raster::values(rasterTemplate)[!is.na(fdir.values) &
                                                      fdir.values>0])
  c2 <- as.character(raster::values(fdir.target)[!is.na(fdir.values) &
                                                   fdir.values>0])

  edges <- as.vector(rbind(c1, c2))
  rm(c1)
  rm(c2)


  gg <- igraph::graph(edges = edges, directed=T)
  return(gg)
}
