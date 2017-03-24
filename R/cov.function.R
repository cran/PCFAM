cov.function<-
function(data.matrix){
	m=dim(data.matrix)[1]
	n=dim(data.matrix)[2]
	barX=matrix(colMeans(data.matrix),m,n,byrow=T)
	S=(1/(m-1))*t(data.matrix-barX)%*%(data.matrix-barX)
	return(S)
}