\name{restaureRealData}
\alias{restaureRealData}
\alias{restaureRealData,LongData}
\alias{restaureRealData,LongData-method}

\title{
~ Function: restaureRealData ~
}
\description{
  This function revert the effect of \code{\link{scale}} by restauring
  the initial values of trajectories.
}
\usage{
restaureRealData(object)
}
\arguments{
  \item{object}{\code{[LongData]}: Object containnig trajectories to restaure.}
}

\details{
  This function revert the effect of \code{\link{scale}} by restauring
  the initial values of trajectories.
}
\value{
None. (this function change internaly the field of an object, it does not return any values.)
}


\references{Article "KmL: K-means for Longitudinal Data", in
  Computational Statistics, Volume 25, Issue 2 (2010), Page 317. \cr
  Web site: \url{http://christophe.genolini.free.fr/kml}
}

\section{Author(s)}{
  Christophe Genolini\cr
  INSERM U669 / PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  Modal'X / Universite Paris Ouest-Nanterre- La Defense \cr\cr

  Contact author : \email{genolini@u-paris10.fr}
}

\examples{
##################
### Building LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:12
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
ld1 <- as.longData(array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(12*8*2,0,3),dim=c(12,8,2)))

### Scaling
plot(ld1)
scale(ld1)

### Only the y-axe change...
plot(ld1)

### Back to the first version of the data
restaureRealData(ld1)
plot(ld1)
}
