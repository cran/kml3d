\name{plot3d,LongData}
\Rdversion{1.1}
\alias{plot3d,LongData-method}
\alias{plot3d,LongData,missing-method}
\alias{plot3d,LongData,Partition-method}

\title{ ~ Function: plot for LongData ~ }
\description{
  Plot two variables of a \code{\linkS4class{LongData}} object in 3D, optionnaly
  relatively to a \code{\linkS4class{Partition}}.
}

\usage{
\S4method{plot3d}{LongData,missing}(x,y,varY=1,varZ=2,paramTraj=parTraj(),nbSample=200)
\S4method{plot3d}{LongData,Partition}(x,y,varY=1,varZ=2,paramTraj=parTraj(type="n"),paramMean=parMean(),nbSample=200)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containing the trajectories to plot.}
  \item{y}{\code{[Partition]}: \code{\linkS4class{Partition}} defining the
    clusters.}
  \item{varY}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the first variable to display. 1 by default.}
  \item{varZ}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the second variable to display. 2 by default.}
  \item{paramTraj}{\code{[parLongData]}: Set the graphical parameters
    used to plot  the trajectories of the \code{LongData}. See \code{\linkS4class{ParLongData}}
    for details.}
  \item{paramMean}{\code{[parLongData]}:  Set the graphical parameters
    used to plot the mean trajectories of each clusters \code{LongData}
    (only when \code{y} is non missing). See \code{\linkS4class{ParLongData}}
    for details.}
  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
    be time consumming. This parameters fixe the maximum numbre of
    trajectories (randomly chosen) that will be drawn.}
}

\details{
  Plot two variables of a \code{\linkS4class{LongData}} object in 3D. It
  use the \code{\link[rgl:rgl]{rgl}} library. The user can make the
  graphical representation turn using the mouse.
}

\seealso{\code{\linkS4class{LongData}}}

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
### Real example on array

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
h <- function(id,t)(id\%\%4-0.5)*(20-t)
ld4 <- as.longData(array(cbind(outer(id2,time,f),outer(id2,time,g),outer(id2,time,h))+rnorm(120*8*3,0,3),dim=c(120,8,3)))

### Basic plot
plot(ld4)

### plot3d, variable 1 and 2
plot3d(ld4)

### plot3d, variable 1 and 3
plot3d(ld4,varZ=3)
plot3d(ld4,paramTraj=parTraj(col="red"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{hplot}