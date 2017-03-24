findfamilies <-
function(x,threshold=.4){ # x is a correlation matrix
 #threshold=.4
 I=nrow(x)
 x=x-diag(rep(1,I))
 whichexceed=sort(grep(T,x>=threshold))
 coord=rowcol(I,I,elements=whichexceed)

 cluster=rep(Inf,I)
 counter=1
 for (i in (1:(I-1))){
  current=c(i,grep(T,x[i,((i+1):I)]>=threshold)+i)
 
  if (length(current)>1){ # consider which clusters already assigned
   minnumber=min(cluster[current],counter)
   cluster[current]=minnumber;counter=counter+1}
  }
 cluster[cluster==Inf]=0

 return(cluster)}
