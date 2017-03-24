\name{perfectwhiten}
\alias{perfectwhiten}
\title{
The covariance preserving whitening function.
}
\description{
This algorithm generates a new scaled 'genotype' dataset which keeps the same covariance structure as the original data, except that family members have been made orthogonal to each other, and singletons are unchanged.
}
\usage{
perfectwhiten(Xun, Xfam, delta = 3e-04, threshold = 0.35, eta = NULL, addfuzz = F)
}

\arguments{
  \item{Xun}{
A matrix of (possibly scaled) genotypes, (number of SNPs)*(number of singletons)
}
  \item{Xfam}{
A matrix of (possibly scaled) genotypes, (number of SNPs)*(number of individuals belonging to families)
}
  \item{delta}{
A slight offset used to ensure that the target covariance matrix is of full rank
}
  \item{threshold}{
The correlation threshold used to determine pairs of relatives. The choice should be less than the degree desired. For example, 0.35 captures first degree relatives (expected correlation 0.5), 0.15 captures first and second degree relatives (expected correlation for second degree relatives is 0.25).
}
  \item{eta}{
This argument is the replacement value used for matrix substitution. The default is NULL, resulting in substitution by the median. 
}
  \item{addfuzz}{
The default is FALSE. Deprecated.
}
}


\value{
\item{Xplusscaled}{The row-scaled full genotype data, including both singletons and family members} 
\item{Y}{The (scaled) genotype matrix after whitening, and should have a covariance matrix very close to Mtarget. Column means are zero}
\item{Ynotcolcentered}{The same as Y, but with column means matching those of Xplusscaled}
\item{M}{The covariance matrix of the full data}
\item{Mtilde}{The covariance matrix after matrix substitution of all family pairs identified with correlations exceedingeta}
\item{whichbig}{The set of indexes of M that have correlation exceeding threshold}
\item{covY}{The covariance matrix of Y, useful to compare to M or to Mtarget}
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui ZHou, Fred A. Wright
}



\examples{
X <- matrix(rbinom(1000*20,2,0.4),1000,20)
X[,1]=X[,2]*0.9
X=rowscale(X)
Xresid=residualize(X)
library(PCFAM)
corXresid=cor(Xresid)
myfam=findfamilies(corXresid,0.1)
K=3
perfect.result=perfectwhiten(X[,which(myfam==0)],X[,which(myfam==1)])


}
