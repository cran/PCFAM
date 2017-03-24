\name{residualize}
\alias{residualize}
\title{
Residualization and scale of the original genotype data
}
\description{
Thus function performs a simple residualization of a row-scaled genotype dataset, removing
 large-scale population stratification. Output is a residualized dataset appropriate for computing
  correlations such that family members can be easily identified.
  The function assumes X is row-scaled
}
\usage{
residualize(X)
}
\arguments{
  \item{X}{
The original input genotype dataset
}
 
}
\details{
This function pre-treatment the data before applying the findfamily function.
}
\value{
Outputs the new row-scaled genotype matrix after residualization}
\references{
Computation of ancestry scores with mixed families and unrelated individuals.  arXiv:1606.08416.
}
\author{
Yi-Hui Zhou
}
