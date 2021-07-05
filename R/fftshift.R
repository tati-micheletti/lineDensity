fftshift <- function(x) {
  # browser()
  if(!is.matrix(x))
  {
    br=length(x)/2
    x[c((br+1):length(x),1:br)]
  } else {
    brx=dim(x)[1]/2
    bry=dim(x)[2]/2
    x[c((brx+1):(2*brx),1:brx),c((bry+1):(2*bry),1:bry)]
  }
}