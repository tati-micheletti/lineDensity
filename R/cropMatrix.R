cropMatrix <- function(m, target, sides=c('all', 'topleft', 'topright', 'bottomleft', 'bottomright')){
  # m: matrix to be expanded
  # target: target matrix or vector of length 2 c(ncol, nrow)
  # sides: where to add rows and columns? c('all', 'topleft', 'topright', 'bottomleft', 'bottomright')
  # browser("add extra row and col in case rowDiff is not even")
  nrowM <- NROW(m)
  ncolM <- NCOL(m)
  
  if(is.matrix(target)){
    rowDiff <- nrowM-target[2]
    colDiff <- ncolM-target[1]
  } else {
    rowDiff <- nrowM-NROW(target)
    colDiff <- ncolM-NCOL(target)
  }
  
  
  switch(sides, 
         all = {
           # remove top and bottom rows
           tmpm <- rowDiff/2
           if(rowDiff %% 2 == 1) { #check if rowDiff is odd; add extra row
             tmpm2 <- tmpm+1
             m <- m[-(c(1:tmpm,(nrowM-(1:tmpm2)+1))),]
           } else { # if even no need to add extra row
             m <- m[-(c(1:tmpm,(nrowM-(1:tmpm)+1))),]
           }
           # remove left and right cols
           tmpm <- colDiff/2
           if(rowDiff %% 2 == 1) { #check if rowDiff is odd; add extra col
             tmpm2 <- tmpm+1
             m <- m[,-(c(1:tmpm,(ncolM-(1:tmpm2)+1)))]
           } else { # if even no need to add extra col
             m <- m[,-(c(1:tmpm,(ncolM-(1:tmpm)+1)))]
           }
         },
         left ={
           # remove left cols
           m <- m[,-(c(1:colDiff))]
         },
         topleft = {
           # remove top rows and left cols
           m <- m[-(c(1:rowDiff)),-(c(1:colDiff))]
         },
         top = {
           # remove top rows
           m <- m[-c(1:rowDiff),]
         },
         topright = {
           # remove top rows and right cols
           m <- m[-(c(1:rowDiff)),-(c(ncolM-(1:colDiff)+1))]
         },
         right ={
           # remove right cols
           m <- m[,-c(ncolM-(1:colDiff)+1)]
         },
         bottomright = {
           # remove bottom rows and right cols
           m <- m[-c(nrowM-(1:rowDiff)+1),-c(ncolM-(1:colDiff)+1)]
         },
         bottom = {
           # remove bottom rows
           m <- m[-c(nrowM-(1:rowDiff)+1),]
         },
         bottomleft = {
           # remove bottom rows and left cols
           m <- m[-c(1:rowDiff),-c(ncolM-(1:colDiff)+1)]
         }
  )
  
  return(m)
}
