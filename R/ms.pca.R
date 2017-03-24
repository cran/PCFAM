ms.pca <-
function(X,corXresid,threshold,top){
  covX = fastcov(X)
    diag(corXresid) = 0
    whichbig = grep(T, corXresid > threshold)
    covX[whichbig] = median(covX)
    myeigen = eigen(covX)
    output = list(eigenvector = myeigen$vectors[, 1:top], eigenvalue = myeigen$values[1:top])
    return(output)
 }
