#' get Basin from Flow Graph
#'
#' derive and create a shapefile of all inflow cells to a given cell number of a flow direction raster
#'
#' @param cell cell number
#' @param fdir a raster object with flow directions (1,2,4,8,16,32,64,128)
#'
#' @return shapefile as object class sf
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' data("volcano")
#' v <- raster(volcano, xmn = 2667400, xmx = 2668010, ymn = 6478700, ymx = 6479570, crs="+init=epsg:27200")
#' fd <- terrain(v, opt = "flowdir")
#' fdGraph <- GRDCFlowTools::createFlowGraph(fd)
#' getCells <- GRDCFlowTools::getBasinCells(4123,fdGraph)
#' watershed <- GRDCFlowTools::getWatershed(getCells, fd)
#' }


getWatershed <- function(cell, fdir) {
  library(raster)
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
  rasterTemplate[as.numeric(cell)] <- 1
  mask <- terra::rast(rasterTemplate)
  createShapefile <- terra::as.polygons(mask)
  createShapefile <- sf::st_as_sf(createShapefile)
  return(createShapefile)
}
