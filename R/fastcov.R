fastcov <-
function(X){
 m=nrow(X);n=ncol(X)
 barX=matrix(colMeans(X),m,n,byrow=T)
 S=(1/(m-1))*t(X-barX)%*%(X-barX)
 return(S)
 }
