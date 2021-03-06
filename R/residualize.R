residualize <-
function(X){
 corX=cor(X)
 corXtilde=corX
 corXtilde[corXtilde>1/8]=1/8
 diag(corXtilde)=1
 myeigen=eigen(corXtilde)
 lambda=myeigen$values
 mad=median(abs(lambda-median(lambda)))
 n=length(lambda)
 spacing=lambda[1:(n-2)]-lambda[2:(n-1)]
 logspacing=log(spacing)
 mymean=mean(logspacing)
 mysd=sd(logspacing)
 myupper=mymean+4*mysd
 k=sum(logspacing>myupper)
 P=myeigen$vectors[,1:k]
 H=P%*%t(P)
 HXT=H%*%t(X)
 residX=X-t(HXT)
 residX=rowscale(residX)
 return(residX)
 }
