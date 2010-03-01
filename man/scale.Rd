\name{scale}
\alias{scale}
\alias{scale,LongData}
\alias{scale,LongData-method}

\title{
  ~ Function: scale for LongData ~
}
\description{
\code{scale} normalized the trajectories of the different variable of a
  \code{\linkS4class{LongData}} object.
}
\usage{
scale(x, center = TRUE, scale = TRUE)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containnig trajectories to be scale.}
  \item{center}{\code{[logical]} or \code{[vector(numeric)]}: Value that
    will be substract from each mesurement of a variable. If \code{center=TRUE}, the mean of
    each variable is used. Otherwise, \code{center} should have a value
    for each variables.}
  \item{scale}{\code{[logical]} or \code{[vector(numeric)]}: Value that
    will divided, after the substration, each mesurement of a variable.
    If \code{scale=TRUE}, the standard deviation of
    each variable is used. Otherwise, \code{scale} should have a value
    for each variables.}
}

\details{
  When variable with different unit are used jointly, it might be necessary to
  standardize them in order to give the same influance to
  all of them. \code{scale} normalized each variable of a
  \code{LongData}.

  More precisely, all the value x[i,j,k] of the variable k will be scale
  according to the classic formula \code{(x[i,j,k]- m_k)/s_k} where
  m_k and s_k are respectively the k-ieme value of the argument
  \code{center} and \code{scale}.

  Note that \code{center=TRUE} is a special value that set \code{m_k=mean(x[,,k])}.
  Similarly, \code{scale=TRUE} is a special value that set \code{s_k=sd(x[,,k])}.
}

\value{
  \code{scale} directly
  modify the internal value of the \code{LongData}. In this
  case, no value is return.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{method}
