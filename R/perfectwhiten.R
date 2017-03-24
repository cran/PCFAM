perfectwhiten <-
function(Xun,Xfam,delta=3e-4,threshold=0.35,eta=NULL,addfuzz=F){
 Xplus=cbind(Xun,Xfam)
 m=dim(Xun)[1]
 Xplusscaled=t(scale(t(Xplus)))
 if (addfuzz==T){set.seed(1); Xoffset=rnorm(m,0,.05);Xplusscaled=Xplusscaled+Xoffset}
 Xplusscaled.colmeans=colMeans(Xplusscaled)

 X=colcenter(Xplusscaled)/sqrt(m-1)
 n=ncol(X)
 nun=ncol(Xun)
 nfam=n-nun

 famcor=fastcov(scale(Xplusscaled[,(nun+1):n])) 
 diag(famcor)=0
 whichbig=grep(T,famcor>threshold)
 numbig=length(whichbig)
 
 if (numbig==0) {print("no family pairs found")}
 if (numbig>0) {print(paste(numbig/2, "pairs are being substituted"))}

 mytol=1e-100
 XTX=t(X)%*%X
 M=XTX
 if (is.null(eta)){eta=median(M)}
 Mtarget=M;Mtarget[(nun+1):n,(nun+1):n][whichbig]=eta

 M11=M[1:nun,1:nun]
 M11=M11+delta
 #if (gen==F){diag(M11)=diag(M11)+delta}
 #M11inv=ginv(M11,tol=mytol)
 M11inv=solve(M11,tol=mytol)
 M12=M[1:nun,(nun+1):n]
 M21=t(M12)
 M22=M[(nun+1):n,(nun+1):n]
 M22tilde=M22
 if (length(whichbig)>0) {M22tilde[whichbig]=eta}
 Mtilde=M
 Mtilde[(nun+1):n,(nun+1):n]=M22tilde
 S=M21%*%M11inv%*%M12
 D=mysqrtm(solve(M22-S))%*%mysqrtm(M22tilde-S)
 Ifam=diag(rep(1,nfam))
 C=M11inv%*%M12%*%(Ifam-D)
 Iun=diag(rep(1,nun))
 zero=matrix(0,nfam,nun)
 AT=cbind(rbind(Iun,zero),rbind(C,D))
 Y=X%*%AT

 Y=Y*sqrt(m-1) # This is the Y analogue of Xplusscaled, but with variation comparable to original
 sdratio=apply(Y,2,sd)/sqrt(diag(Mtarget))
 Y=t(t(Y)/sdratio)

 Ynotcolcentered=t(t(Y)+Xplusscaled.colmeans) # This is the Y analogue of Xplusscaled,but with variation comparable to  original
 covY=fastcov(Y)

 return(list(Xplusscaled=Xplusscaled,Y=Y,Ynotcolcentered=Ynotcolcentered,M=M,Mtilde=Mtilde,whichbig=whichbig,covY=covY))}
