colcenter <-
function(X){
 X=t(t(X)-colMeans(X))
 return(X)
 }
