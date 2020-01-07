\name{GPB-package}
\alias{GPB-package}
\alias{GPB}
\docType{package}
\title{
Generalized Poisson Binomial Distribution
}
\description{
This package computes the distribution functions for the Generalized Poisson Binomial distribution. It provides the cdf, pmf, quantile function, and random number generation for the distribution.
}
\details{
Generalized Poisson Bionomial distribution is defined as distribution of sum of independent, non-identically distributed Bernoulli variables which are associated with integer values. This package provides an computing method for Generalized Poisson Binomial distributions. The fomula that computes the cdf of Generalized Poisson Binomial distribution is based on the discrete Fourier transform of the characteristic function.
}
\author{
Yili Hong, Man Zhang, and R Core Team

Maintainer: Yili Hong
}

\keyword{ package }

\examples{
pgpb(kk=0:11, pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))
dgpb(kk=0:11, pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))
qgpb(qq=c(.1,.3), pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))
rgpb(m=3, pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))

}
