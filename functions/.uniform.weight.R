.uniform.weight <- function (rs, lambda) 
{
  # browser()
  if (length(lambda) == 1) {
    d <- 3 * lambda
  }
  else {
    d <- lambda[2]
    lambda <- lambda[1]
  }
  nx <- 1 + 2 * floor(d/rs[1])
  ny <- 1 + 2 * floor(d/rs[2])
  m <- matrix(ncol = nx, nrow = ny)
  xr <- (nx * rs[1])/2
  yr <- (ny * rs[2])/2
  r <- raster::raster(m, xmn = -xr[1], xmx = xr[1], ymn = -yr[1], 
              ymx = yr[1], crs = "+proj=utm +zone=1 +datum=WGS84")
  p <- raster::xyFromCell(r, 1:ncell(r))^2
 
  # browser()
  
  # https://math.stackexchange.com/questions/445536/continuous-uniform-distribution-over-a-circle-with-radius-r
  # m <- (2*(sqrt((lambda^2)-(p[, 1] + p[, 2])^2)))/(lambda^2 * pi)
  ### ABOVE WORKS ###
  m <- 1/(lambda^2 * pi) * (abs(p[, 1] + p[, 2]) <= lambda^2)
  ### But this is correct ###
  
  # m[is.na(m)] <- 0
  # plot(m)
  
  # gaussian example
  # m <- 1/(2 * pi * sigma^2) * exp(-(p[, 1] + p[, 2])/(2 *
                                                        # sigma^2))
  m <- matrix(m, ncol = nx, nrow = ny, byrow = TRUE)
  # browser()
  # normalize convolution
  # m <- m/sum(m)
}