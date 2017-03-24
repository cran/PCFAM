\name{rowcol}
\alias{rowcol}

\title{
Indicators for of the row and column of the original matrix
}
\description{
This function identifies the rows and columns of elements in a matrix, e.g. the family members identified based on the correlation matrix.
}
\usage{
rowcol(I, J, elements)
}
\arguments{
\item{I}{The number of rows of the matrix (scalar)}
\item{J}{The number of columns of the matrix (scalar)}
\item{elements}{A vector of matrix element indexes}
}
\value{
\item{whichrow}{The rows of elements in the matrix}
\item{whichcol}{The columns of elements in the matrix}
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
}
