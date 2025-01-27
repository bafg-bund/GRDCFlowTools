#' igraph object from flow direction raster (splitted)
#'
#' Creates a list of igraph objects from a splitted flow direction raster
#'
#' @param fdirpath path to a raster object with flow directions (1,2,4,8,16,32,64,128)
#' @param ny number of strips the raster will be splitted into
#' @param rootpath path to directory where igraph objects will be written to
#'
#' @return list of named list (slot path: path to graph; slot nmin: minimum cell number of this graph; slot nmax: maximum cell number of this graph)
#'
#' @export
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
#' }


createSplitFlowGraph <- function(fdirpath, ny = 10, rootpath=NULL) {
  library(SpaDES.tools)
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

  dataType(rasterTemplate) <- "INT4S"

  raster::values(rasterTemplate) <- c(1:(ncol*nrow))

  fdir.target <- rasterTemplate

  directions <- c(1, 2, 4, 8, 16, 32, 64, 128)
  shift <- c(1, ncol+1, ncol, ncol-1, -1, -ncol-1, -ncol, -ncol+1)

  fdir.values <- raster::values(fdir)

  for (i in seq_along(directions)) {
    fdir.target[which(fdir.values == directions[i])] <-
      fdir.target[which(fdir.values == directions[i])] + shift[i]

  }

  if (is.null(rootpath)) rootpath <- tempdir()

  path <- file.path(rootpath, "splitRasterTemplate")

  rasterTemplate.split <- SpaDES.tools::splitRaster(r = rasterTemplate,
                                                    nx = 1,
                                                    ny = ny,
                                                    path = path,
                                                    rType = "INT4S")
  rm(rasterTemplate)

  path <- file.path(rootpath, "splitRaster")

  fdir.target.split <- SpaDES.tools::splitRaster(r = fdir.target,
                                                 nx = 1,
                                                 ny = ny,
                                                 path = path,
                                                 rType = "INT4S")
  rm(fdir.target)
  path <- file.path(rootpath, "splitFdirRaster")

  fdir.split <- SpaDES.tools::splitRaster(r = fdir,
                                          nx = 1,
                                          ny = ny,
                                          path = path,
                                          rType = "INT2S")

  rm(fdir)
  path <- file.path(rootpath, "igraph")
  dir.create(path, showWarnings = FALSE)

  resultList <- list()

  for (i in seq_along(fdir.target.split)) {
    message(paste0("calculating graph for band ", i))
    fdir.values <- raster::values(fdir.split[[i]])
    c1 <- as.character(raster::values(rasterTemplate.split[[i]])[!is.na(fdir.values) &
                                                                   fdir.values>0])
    c2 <- as.character(raster::values(fdir.target.split[[i]])[!is.na(fdir.values) &
                                                                fdir.values>0])
    edges <- as.vector(rbind(c1, c2))
    rm(c1)
    rm(c2)
    gg <- igraph::graph(edges = edges, directed=T)
    rm(edges)
    thispath <- file.path(path, paste0("graph_", i, ".rds"))
    saveRDS(gg,file.path(thispath))
    resultList[[i]] <- list(path=thispath,
                            nmin=rasterTemplate.split[[i]]@data@min,
                            nmax=rasterTemplate.split[[i]]@data@max,
                            bmin=rasterTemplate.split[[i]]@data@min+ncol,
                            bmax=rasterTemplate.split[[i]]@data@max-ncol)

  }

  return(resultList)
}
