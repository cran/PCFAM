mysqrtm <-
function(a,symmetric=F){
 # Code taken with slight modification from John Johnson
 # http://realizationsinbiostatistics.blogspot.com/2008/08/matrix-square-roots-in-r_18.html
 a.eig <- eigen(a,symmetric=symmetric)
 a.eig$values[a.eig$values<0]=1e-12
 a.sqrt <- a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% t(a.eig$vectors)
 return(a.sqrt)}
