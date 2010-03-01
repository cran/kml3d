\name{Partition-class}
\docType{class}
\alias{Partition}
\alias{Partition-class}
\alias{[,Partition-method}
\alias{[<-,Partition-method}
\alias{show,Partition-method}

\title{~ Class: Partition ~}
\description{
  An object of class \code{Partition} is a list of cluster's names. Unlike a
  \code{\linkS4class{Clustering}}, a partition is *not* associate to a population.

}

\section{Objects from the Class}{
  Objects are mainly intend to be created by some clusterization methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...)
}

\section{Slots}{
  \describe{
    \item{\code{nbClusters}}{[numeric]: number of groups, between 1 and 26}
    \item{\code{clusters}}{[vector(factor)]: vector containing the affectation
      groups of each individual. Should be upper-case letters.}
  }
}

\section{validation rules}{
  A class \code{Partition} object must follow some rules to be valid:
  \itemize{
    \item Slots should be either all empty, or all non empty.
    \item \code{nbClusters} has to be lower or equal to 26 (twenty six clusters maximum).
    \item \code{clusters} is a factor in \code{LETTERS[1:nbCluster]}.
  }
}


\section{Construction}{
  Class \code{Partition} objects are mainly constructed by some clusterization methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...) Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{partition}}.
}


\section{Get [}{
  \describe{
    \item{Object["nbClusters"]}{[numeric]: Gets the number of clusters (the value of the slot \code{nbClusters})}
    \item{Object["clusters"]}{[vector(factor)]: Gets the cluster of each individual (the value of the slot \code{clusters})}
    \item{Object["clustersAsInteger"]}{[vector(integer)]: Gets the
      cluster of each individual and turn them into integer}
  }
}

\section{Setteur [<-}{
  \describe{
    \item{Object["nbClusters"]<-value}{[numeric]: Sets the number of
      clusters to value.}
    \item{Object["clusters"]<-value}{[vector(factor)]: Sets the cluster
  of each individual. If value is numeric, it is turn into a factor.}
  }
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

\seealso{
  Overview: \code{\link{kml3d-package}} \cr
  Classes: \code{\linkS4class{LongData}} \cr
  Methods: \code{\link{partition}}\cr
}



\examples{
### Building Partition
part <- partition(rep(c("A","B","A"),time=10),nbClusters=3)

### Get
part["clusters"]
part["nbClusters"]

### Set
part["nbClusters"]<-2
print(part)
}


\keyword{classes}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
