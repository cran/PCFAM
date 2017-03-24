\name{ms.pca}
\alias{ms.pca}

\title{
The matrix substitution approach
}
\description{
This function provides the matrix subsitution algorithm. The main idea is to replace the high covariance value entries in the covariance matrix which are produced by family members by a small value (e.g. median covariance). 
}
\usage{
ms.pca(X, corXresid, threshold, top)
}

\arguments{
  \item{X}{
The input data matrix
}
 \item{corXresid}{
The correlation of the genotypes after residualization for any evidence of larger scale ancestry. Used to identify close-degree family members in a manner robust to large-scale ancestry.
}
  \item{threshold}{
Covariance values of identified family members are set to the threshold.
}
  \item{top}{
The number of ancestry scores to obtain.
}
}

\value{
\item{eigenvector}{Eigenvectors after using the matrix substitution method}
\item{myeigen}{The top eigenvalues and eigenvectors}
}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui Zhou
}