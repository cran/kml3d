\name{generateArtificialLongData}
\alias{gald}
\alias{generateArtificialLongData}


\title{~ Function: generateArtificialLongData (or gald) ~}

\description{
  This function builp up an artificial longitudinal data set an turn it
  into an object of class \code{\linkS4class{ClusterLongData}} or \code{\linkS4class{LongData}}.
}
\usage{
gald(nbEachClusters=c(50,50,50),time=0:10,varNames=c("V1","V2"),
    functionClusters=list(function(t){c(0,0)},function(t){c(10,10)},function(t){c(10-t,10-t)}),
    functionNoise=function(t){c(rnorm(1,0,3),rnorm(1,0,3))},
    decimal=2,percentOfMissing=0,clusterLD=TRUE)

generateArtificialLongData(nbEachClusters=c(50,50,50),time=0:10,varNames=c("V1","V2"),
    functionClusters=list(function(t){c(0,0)},function(t){c(10,10)},function(t){c(10-t,10-t)}),
    functionNoise=function(t){c(rnorm(1,0,3),rnorm(1,0,3))},
    decimal=2,percentOfMissing=0,clusterLD=TRUE)
}

\arguments{
  \item{nbEachClusters}{\code{[vector(numeric)]}: number of trajectories that each
    cluster must contain.
  }
  \item{time}{\code{[vector(numeric)]}: time at which measures are made.}
  \item{varNames}{\code{[vector(character)]}: names of the variables.}
  \item{functionClusters}{\code{[vector(numeric) <- function(t)]} or
    \code{[list(vector(numeric <- function(t))]}: lists the functions
    defining the average trajectories of each cluster.
    The function shall return a value for each variable of \code{varNames}.
    If a single function is given, it is duplicated for all groups (see detail).
  }
  \item{functionNoise}{\code{[vector(numeric) <- function(t)]} or
    \code{[list(vector(numeric <- function(t))]}: lists the functions
    generating the noise of each trajectory within its own cluster.
    The function shall return a value for each variable of \code{varNames}.
    If a single function is given, it is duplicated for all groups (see detail).
  }
  \item{decimal}{\code{[numeric]}: number of decimals used to round up values.}
  \item{percentOfMissing}{\code{[numeric]}: percentage (between 0 and 1)
    of missing data generated in each cluster. If a single value is
    given, it is duplicated for all groups (see detail).
  }
  \item{clusterLD}{\code{[logical]}: if \code{TRUE}, the function
    create an object \code{\linkS4class{ClusterLongData}}. If
    \code{FALSE}, it create an object \code{\linkS4class{LongData}}.
  }
}

\details{
  \code{generateArtificialLongData} (\code{gald} in short) is a
  function that contruct a set of artificial joint longitudinal data.
  Each individual is considered as belonging to a group. This group
  follows a theoretical trajectory, function of time.
  These functions (one per group) are given via the argument \code{functionClusters}.

  Within a group, the individual undergoes individal
  variations. Individual variations are given via the argument \code{functionNoise}.

  The number of individuals in each group is given by \code{nbEachClusters}.

  Finally, it is possible to add missing values randomly striking the
  data thanks to \code{percentOfMissing}.
}

\value{
  Object of class \code{\linkS4class{LongData}} or
  \code{\linkS4class{ClusterLongData}}, according to \code{clusterLD}.
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
### Default example
ex1 <- generateArtificialLongData()
ex1
kml3d(ex1,3,1)
plot3d(ex1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{datagen}  % Functions for generating data sets
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
