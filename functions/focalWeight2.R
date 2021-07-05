focalWeight2 <- function (x, d, type = c("uniform", "circle", "Gauss", "rectangle")) 
{
  # adds 'uniform' to raster::focalWeight()
  type <- match.arg(type)
  x <- res(x)
  if (type == "uniform") {
    .uniform.weight(x, d)
  }
  else if (type == "circle") {
    raster:::.circular.weight(x, d[1])
  }
  else if (type == "Gauss") {
    if (!length(d) %in% 1:2) {
      stop("If type=Gauss, d should be a vector of length 1 or 2")
    }
    raster:::.Gauss.weight(x, d)
  }
  else {
    raster:::.rectangle.weight(x, d)
  }
}
