\name{expandParLongData}
\alias{expandParLongData}
\alias{expandParLongData,ParLongData,Partition}
\alias{expandParLongData,ParLongData,Partition-method}
\alias{expandParLongData,ParLongData,numeric}
\alias{expandParLongData,ParLongData,numeric-method}

\title{
~ Function: expandParLongData ~
}
\description{
  Change the values of an object
  \code{\linkS4class{ParLongData}} to make them fit with a
  \code{\linkS4class{Partition}} or with a number of clusters.
}
\usage{
expandParLongData(xParLongData, y)
}

\arguments{
  \item{xParLongData}{\code{[ParLongData]}: The object to expand.}
  \item{y}{\code{[Partition]} or \code{[numeric]}: see detail.}
}
\details{
  \code{\linkS4class{ParLongData}} object can hold values that are easy
  to specify (like \code{col="clusters"} or \code{pch="symbol"}) but that can not
  be directly used by functions \link[=plot,LongData-method]{plot} and
  \link[=plot3d,LongData-method]{plot3d}. This function modify theses values to
  make them fit with \code{plot} and \code{plot3d} expectations.

  The field \code{col} and \code{pch} are the ones concern by this
  function.

  If \code{y} is a \code{Partition}, \code{col} and \code{pch} are
  extanded to fit with the number of individual. If \code{y} is a number of
  clusters, \code{col} and \code{pch} are extanded to fit with the
  number of clusters.

  If \code{col='clusters'}, a color is affected to each clusters. Then
  the field \code{col} receive a vector of color such that each
  individual (if \code{y} is a \code{Partition}) or each clusters (if
  \code{y} is a number of clusters) get its corresponding color.

  If \code{pch='letters'}, a letters is affected to each clusters. Then
  the field \code{pch} receive a vector of letters such that each
  individual (if \code{y} is a \code{Partition}) or each clusters (if
  \code{y} is a number of clusters) get its corresponding letters.

  Same think if \code{pch='symbols'}.
}
\value{
An object of class \code{\linkS4class{ParLongData}}
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
###################
### Some parameters for trajectories
(paramTraj <- parTraj(col="clusters"))

### Expand to a small partition with 3 clusters
part <- partition(LETTERS[rep(1:3,4)])
expandParLongData(paramTraj,part)


###################
### Some parameters for the mean trajectories
paramMean <- parMean()

### If there is 3 clusters :
expandParLongData(paramMean,3)

### If there is 5 clusters :
expandParLongData(paramMean,5)
}

