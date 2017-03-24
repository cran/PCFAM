\name{cov.function}
\alias{cov.function}
\title{
Sample covariance calculator
}
\description{
Obtain a sample covariance matrix 
}
\usage{
cov.function(data.matrix)
}
\arguments{
  \item{data.matrix}{
Input mxn data matrix
}
}

\value{
return the nxn sample covariance matrix
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui Zhou
}
\examples{
X <- matrix(rbinom(1000*20,2,0.4),1000,20)
cov.X=cov.function(X)
}