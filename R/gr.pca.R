gr.pca <-
function(data.input,index.family,myfam,weight,top,family.size,inflation){
##### data.input has been row centered
  newX=data.input
for (j in (2:family.size)){
t.family=which(table(myfam)==j)
for (i in (1:length(t.family))){
index=which(myfam==as.numeric(names(t.family))[i])
vf=data.input[,index]
v=apply(vf,1,mean)
uf=v/sqrt(sum(v^2))
v.length=sqrt(colSums(vf^2))
ui=t(t(vf)/v.length)
uip=ui-as.matrix(uf)%*%t(t(ui)%*%as.matrix(uf))
uip=t(t(uip)/sqrt(colSums(uip^2)))

newX[,index]=newX[,index]*weight + (1-weight)*(uf*j^(-1/2)+uip*(1-j^(-1))^(1/2))*v.length

if (inflation==1){
fam.original=newX[,index]
mu=colMeans(fam.original)
sigma=apply(fam.original,2,sd)
fam=t(t(scale(newX[,index]))*sigma+mu)
newX[,index]=fam
temp.1=cor(data.input[,index],data.input[,-index])
temp.1.new=cor(newX[,index],newX[,-index])
slope.temp=rep(0,dim(temp.1)[1])
for (k in (1:dim(temp.1)[1])){
 slope.temp[k]=summary(lm(temp.1.new[k,]~temp.1[k,]))$coef[2,1]
  if (cor(temp.1.new[k,],temp.1[k,])>0.8) newX[,index[k]]=newX[,index[k]]/slope.temp[k]
}
}
}
}
      myeigen.cov=eigen(cov.function(newX))
output=list(data.new=newX,topPCs=myeigen.cov$vectors[,1:top],topEigenvalue=myeigen.cov$values[1:top])
return(output)
}
