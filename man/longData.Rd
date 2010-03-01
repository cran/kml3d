\name{longData}
\alias{longData}
\alias{longData,ANY,ANY,ANY,ANY,ANY-method}
\alias{longData,missing,missing,missing,missing,missing-method}
\title{ ~ Function: longData ~}
\description{
\code{longData} is the constructor of the class \code{\linkS4class{LongData}}.
}
\usage{
longData(traj, idAll, time, varNames, maxNA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[array(numeric)]}: structure containning the trajectories.}
  \item{idAll}{\code{[vector(character)]}: single identifier for each trajectory (ie each individual).}
  \item{time}{\code{[vector(numeric)]}: time at which measures are made.}
  \item{varNames}{\code{[character]}: names of the variable being measured.}
  \item{maxNA}{\code{[numeric] or [vector(numeric)]}:
    Fixes the number of maximum missing values that can be tolerate in a
    trajectories; Should this number be exceed \code{maxNA}, the
    trajectories would be removed from \code{traj} and \code{idFewNA}.}
}

\details{
  \code{longData} construct a object of class
  \code{\linkS4class{LongData}}. It does not provide any default
  values except for maxNA which is length(time)-2.
}
\value{
  An object of class \code{\linkS4class{LongData}}.
}

\seealso{\link{as.longData}, \linkS4class{LongData}}


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
### Building longData
tr1 <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2))

longData(
    traj=tr1,
    idAll=as.character(c(100,102,103)),
    time=c(1,2,4,8,15),
    varNames=c("P","A"),
    maxNA=3
    )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{ts}
\keyword{cluster}
\keyword{methods}



