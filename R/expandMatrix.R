expandMatrix <- function(m, target, sides=c('all', 'topleft', 'topright', 'bottomleft', 'bottomright')){
  # m: matrix to be expanded
  # target: target matrix or vector of length 2 c(ncol, nrow)
  # sides: where to add rows and columns? c('all', 'topleft', 'topright', 'bottomleft', 'bottomright')
  # browser("add extra row and col in case rowDiff is not even")
  if(is.matrix(target)){
    rowDiff <- NROW(target)-NROW(m)
    colDiff <- NCOL(target)-NCOL(m)
  } else {
    rowDiff <- target[2]-NROW(m)
    colDiff <- target[1]-NCOL(m)
  }
  
  
  switch(sides, 
         all = {
           # bind new rows
           tmpm <- matrix(0,nrow=rowDiff/2, ncol=NCOL(m))
           if(rowDiff %% 2 == 1) { #check if rowDiff is odd; add extra row
             tmpm2 <- matrix(0,nrow=(rowDiff/2)+1, ncol=NCOL(m))
             m <- rbind(tmpm2,m,tmpm)
           } else { # if even no need to add extra row
             m <- rbind(tmpm,m,tmpm)
           }
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff/2)
           if(rowDiff %% 2 == 1) { #check if rowDiff is odd; add extra col
             tmpm2 <- matrix(0,nrow=NROW(m),ncol=(colDiff/2)+1)
             m <- cbind(tmpm2,m,tmpm)
           } else { # if even no need to add extra col
             m <- cbind(tmpm,m,tmpm)
           }
         },
         top = {
           # bind new rows
           tmpm <- matrix(0,nrow=rowDiff, ncol=NCOL(m))
           m <- rbind(tmpm,m)
         },
         topleft = {
           # bind new rows
           tmpm <- matrix(0,nrow=rowDiff, ncol=NCOL(m))
           m <- rbind(tmpm,m)
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff)
           m <- cbind(tmpm,m)
         },
         left ={
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff)
           m <- cbind(tmpm,m)
         },
         topright = {
           # bind new rows
           tmpm <- matrix(0,nrow=rowDiff, ncol=NCOL(m))
           m <- rbind(tmpm,m)
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff)
           m <- cbind(m,tmpm)
         },
         right ={
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff)
           m <- cbind(m,tmpm)
         },
         bottomright = {
           # bind new rows
           tmpm <- matrix(0,nrow=rowDiff, ncol=NCOL(m))
           m <- rbind(m,tmpm)
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff)
           m <- cbind(m,tmpm)
         },
         bottom = {
           # bind new cols
           tmpm <- matrix(0,nrow=rowDiff, ncol=NCOL(m))
           m <- rbind(m,tmpm)
         },
         bottomleft = {
           # bind new rows
           tmpm <- matrix(0,nrow=rowDiff, ncol=NCOL(m))
           m <- rbind(m,tmpm)
           # bind new cols
           tmpm <- matrix(0,nrow=NROW(m),ncol=colDiff)
           m <- cbind(tmpm,m)
         }
  )
  
  return(m)
}
