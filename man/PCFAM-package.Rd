\name{PCFAM-package}
\alias{PCFAM-package}
\alias{PCFAM}
\docType{package}
\title{
Computation of ancestry scores with mixed families and unrelated
individuals
}
\description{
This package provides ancestry scores based on genotype data, and is robust to the presence of close-degree family members. Four main novel algorithms are represented: (i) Geometric rotation (within-family data orthogonalization); (ii) matrix substitution based on the decomposition of a target family-orthogonalized covariance matrix; (iii) covariance-preserving whitening, retaining covariances between unrelated pairs while orthogonalizing family members (Note: the function \link{perfectwhiten} generates a new dataset which keeps the same covariance structure as the original set); (iv) using family-averaged data to obtain loadings for projection of family members.
}
\details{
\tabular{ll}{
Package: \tab PCFAM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-10-11\cr
License: \tab GPL 2\cr
LazyLoad: \tab yes\cr

}

}
\author{
Yi-Hui Zhou

Maintainer: Yi-Hui Zhou <yihui_zhou@ncsu.edu>
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals. arXiv:1606.08416
}

\examples{
X <- matrix(rbinom(1000*20,2,0.4),1000,20)
X[,1]=X[,2]*0.9
X=rowscale(X)
Xresid=residualize(X)
corXresid=cor(Xresid)
myfam=findfamilies(corXresid,0.1)
K=3
myms.pca=ms.pca(X,corXresid,0.1,K)
familyave.result=familyave(X,myfam,top=K)
}
