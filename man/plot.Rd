\name{plot,ClusterLongData}
\Rdversion{1.1}
\alias{plot}
\alias{plot,ClusterLongData}
\alias{plot,ClusterLongData-method}
\alias{plot,ClusterLongData,missing-method}
\alias{plot,ClusterLongData,ANY-method}

\title{ ~ function: plot for ClusterLongData ~ }
\description{
  \code{plot} the a \code{\linkS4class{ClusterLongData}} object.
}

\usage{
\S4method{plot}{ClusterLongData,ANY}(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200,toPlot=c("both"),nbCriterion=100)
}

\arguments{
  \item{x}{\code{[ClusterLongData]}: Object containing the trajectories
    to plot according to a \code{\linkS4class{Partition}} define by \code{y}.}
  \item{y}{\code{[numeric]}: Define \code{Partition} P that will be use to plot
    the object. \code{P} is a \code{Partition} hold in the field \code{c2},
    \code{c3}, ... \code{c26}. If \code{y=c(a,b)}, then \code{P} is
    the \code{Partition} number \code{b} with \code{a} clusters. If \code{y=a},
    then \code{P} is the partition number \code{1} with \code{a}
    clusters. If \code{y} is missing, \code{P} is the \code{Partition} with the best
    criterion.}
  \item{paramTraj}{\code{[parLongData]}: Set the graphical parameters
    used to plot the trajectories of the \code{LongData}. See \code{\linkS4class{ParLongData}}
    for details.}
  \item{paramMean}{\code{[parLongData]}: Set the graphical parameters
    used to plot the mean trajectories of each clusters \code{LongData}
    (only when \code{y} is non missing). See \code{\linkS4class{ParLongData}}
    for details.}
  \item{paramWindows}{\code{[ParWindows]}: Set the graphical display of
    the windows. See \code{\linkS4class{ParWindows}} for details.}
  \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
    be time consumming. This parameters fixe the maximum numbre of
    trajectories (randomly chosen) that will be drawn.}
  \item{toPlot}{\code{[character]}: define the graph to plot. If "traj",
    then the trajectories are plot (like with function
    \code{\link{plot,LongData}}).
    If "criterion", then the criterion are plot (like with function
    \code{\link{plot,ListClustering}}). If "both", then that graphic
    windows is split in two and both graphs are displayed. If "none",
    there is no graphical display.}
  \item{nbCriterion}{[numeric]: set the maximum number of criterion to
    display for a specific clusters number.}
}

\details{
  \code{plot} each variable of a \code{\linkS4class{ClusterLongData}}
  according to the \code{Partition} define by \code{y}.
}

\seealso{\code{\linkS4class{ClusterLongData}}}


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
id3=1:12000
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
ld3 <- as.longData(array(cbind(outer(id3,time,f),outer(id3,time,g))+rnorm(12000*8*2,0,3),dim=c(12000,8,2)))
par(ask=TRUE)

### Basic plot
plot(ld3)

### Change the windows orientation
plot(ld3,paramWindows=windowsCut(c(2,1)))

### Change some parameters
plot(ld3,paramTraj=parTraj(col="red"))

### Plot only 1000 trajectories
plot(ld3,nbSample=1000)

### Plot all the trajectories
plot(ld3,nbSample=Inf)
par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{aplot}