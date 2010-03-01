\name{clusterLongData}
\alias{cld}
\alias{clusterLongData}
\alias{clusterLongData,ANY,ANY,ANY,ANY,ANY,ANY-method}
\alias{clusterLongData,missing,missing,missing,missing,missing,missing-method}
\title{ ~ Function: clusterLongData (or cld) ~}
\description{
  \code{clusterLongData} (or \code{cld} in short) is the constructor
  of the class \code{\linkS4class{ClusterLongData}}.
}
\usage{
clusterLongData(traj, idAll, time, varNames, maxNA = length(time) - 2,criterionActif="calinski")
cld(traj, idAll, time, varNames, maxNA = length(time) - 2,criterionActif="calinski")
}

\arguments{
  \item{traj}{\code{[array(numeric)]}: structure containning the trajectories.}
  \item{idAll}{\code{[vector(character)]}: single identifier for each trajectory (ie each individual).}
  \item{time}{\code{[vector(numeric)]}: time at which measures are made.}
  \item{varNames}{\code{[character]}: names of the variable being measured.}
  \item{maxNA}{\code{[numeric]} or \code{[vector(numeric)]}:
    Fixes the number of maximum missing values that can be tolerate in a
    trajectory; Should this number be exceed \code{maxNA}, the
    trajectory would be removed from \code{traj} and \code{idFewNA}.}
  \item{criterionActif}{\code{[character]} Set the criterion that will
    be use by fonctions that need a single criterion
    (like \link[=plot,ClusterLongData-method]{plot} or \link{ordered}).}
}


\details{
  \code{clusterLongData} construct a object of class
  \code{\linkS4class{ClusterLongData}}. It does not provide any default
  values except for maxNA which is \code{length(time)-2}. It is not realy
  friendly user function; use \code{\link{as.clusterLongData}} instead.
}
\value{
 An object of class \code{\linkS4class{ClusterLongData}}.
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
###############
### Building an array
tr1n <- array(c(1,2,NA, 1,4,NA, 6,1,8, 10,NA,2, 3,NA,NA,
                4,NA,5,  6,3,4, 3,4,4, 4,NA,NA, 5,5,4),
            dim=c(3,5,2))


###############
### clusterLongData

### With maxNA=3
longData(traj=tr1n,
    idAll=as.character(c(100,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=3
)

### With maxNA=2
### Individual 104 is exclude
longData(traj=tr1n,
    idAll=as.character(c(100,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=3
)

}

