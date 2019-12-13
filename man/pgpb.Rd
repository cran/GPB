\name{pgpb}
\alias{pgpb}
\alias{dgpb}
\alias{qgpb}
\alias{rgpb}
\alias{gpb.args.check}

\title{
Generalized Poisson Binomial Distribution
}
\description{
The cdf, pmf, quantile function, and random number generator function for Generalized Poisson Binomial distribution.
}
\usage{
pgpb(kk, pp, aval, bval, wts = NULL)
dgpb(kk, pp, aval, bval, wts = NULL)
qgpb(qq, pp, aval, bval, wts = NULL)
rgpb(m, pp, aval, bval, wts = NULL)
gpb.args.check(pp, aval, bval, wts)
}

\arguments{
  \item{kk}{
   The values where the cdf or pmf to be evaluated.
}
  \item{pp}{
The vector for \eqn{p_k}'s which are the sucess probabilities for indicators.
}
  \item{aval}{ The smaller possible values of each indicator, default to be 0. Integer values needed.
}
  \item{bval}{ The larger possible values of each indicator, default to be 1. Integer values needed.
}
  \item{wts}{ The weights for \eqn{p_k}'s. Positive integer values needed.
}
  \item{qq}{ The values where the quantile function to be evaluated.
}
  \item{m}{ The number of random numbers to be generated.
}
}

\value{
Returns the cdf, pmf, quantiles, and random numbers.
}

\author{
Yili Hong
}

\references{
Man. Zhang, Y. Hong, and N. Balakrishnan (2018). ``The generalized Poisson-binomial distribution and the computation of its distribution function,''  Journal of Statistical Computation and Simulation, Vol. 88, pp. 1515-1527.

Singleton, R. C. (1969). An algorithm for computing the mixed radix fast Fourier transform. IEEE Transactions on Audio and Electroacoustics, Vol. 17, pp. 93-103.
}

\examples{

pgpb(kk=0:11, pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))
dgpb(kk=0:11, pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))
qgpb(qq=c(.1,.3), pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))
rgpb(m=3, pp=c(.1, .2, .3), aval=c(1,0,0), bval=c(2,3,1), wts=c(1,2,2))

## when a, b share large common dividers, the results of following cases is the same
pgpb(kk=c(60,70,80,90), pp=c(.1, .2, .3), aval=c(10,20,30), bval=c(20,30,40), wts=c(1,1,1))
pgpb(kk=6:9, pp=c(.1, .2, .3), aval=c(1,2,3), bval=c(2,3,4), wts=c(1,1,1))

## when a, b are non-integer values, the values of kk, aval, bval can multiply powers of 10
aval=c(0.1,0.2,0.3)*10
bval=c(0.2,0.3,0.4)*10
kk=(0.6*10):(1.0*10)
pgpb(kk=kk, pp=c(.1, .2, .3), aval=aval, bval=bval, wts=c(1,1,1))

}


