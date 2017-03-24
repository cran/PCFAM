\name{familyave}
\alias{familyave}
\title{
Family average approach
}
\description{
This function implements the family-averaging algorithm, with loadings based on the combined data from singletons and family averages, then projected to all.
}
\usage{
familyave(Xall,myfam, top = 5)
}
\arguments{
  \item{Xall}{
The original input genotype dataset
}
  \item{myfam}{
The identified family IDs. Each singleton forms his/her own family.
}
  \item{top}{
The number ancestry scores desired.
}
}
\details{
The function averages the genotype information in each family, re-inflates to have appropriate variability, andtreats as a 'singleton' for the purpose of loading calculation. Ancestry scores are obtained by projection to all.
}
\value{
Output the top ancestry scores by combining family data with singletons}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui Zhou
}
\examples{
X <- matrix(rbinom(1000*20,2,0.4),1000,20)
X[,1]=X[,2]*0.9
X=rowscale(X)
Xresid=residualize(X)
corXresid=cor(Xresid)
myfam=findfamilies(corXresid,0.1)
K=3
familyave.result=familyave(X,myfam,top=K)

}
