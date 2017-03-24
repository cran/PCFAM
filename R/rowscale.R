rowscale <-
function(X){
 n=dim(X)[2]
 X=X-rowMeans(X)
 X=X/sqrt(rowSums(X^2)/(n-1))
 return(X)}
