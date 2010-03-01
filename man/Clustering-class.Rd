\name{Clustering-class}
\docType{class}
\alias{Clustering}
\alias{Clustering-class}
\alias{[,Clustering-method}
\alias{[<-,Clustering-method}
\alias{show,Clustering-method}

\title{~ Class: Clustering ~}
\description{
  An object of class \code{Clustering} is a partition of a population
  into subgroups.  The object also contains some information like the
  percentage of trajectories in each group or some qualities criterion.
}

\section{Objects from the Class}{
  Objects are mainly intend to be created by some clusterization methods
  (like k-means, fuzzy k-means...)
}

\section{Slots}{
  \describe{
    \item{\code{nbClusters}}{[numeric]: number of groups, between 2 and 26}
    \item{\code{clusters}}{[vector(factor)]: vector containing the affectation
      groups of each individual. Should be upper-case letters.
      Note that a group might be empty.}
    \item{\code{percentEachCluster}}{[vector(numeric)]: percentage of
      trajectories contained in each group.}
    \item{\code{convergenceTime}}{[numeric]:  if the
      \code{Clustering} is obtained by running an algorithm, this
      variable saves the number of iteration
      necessary to converge.}
    \item{\code{multiplicity}}{[numeric]: using an algorithm, this
      variable saves the number of time that this particular
      \code{Clustering} is obtained. See \code{\link{regroup}} for details.}
    \item{\code{criterionNames}}{[vector(character)]: Names of the quality
      criterions used to evaluate the quality of the \code{Clustering}.}
    \item{\code{criterionValues}}{[vector(numeric)]: Value of the quality
      criterions used to evaluate the quality of the \code{Clustering}.}
    \item{\code{algorithm}}{[vector3(character)]: This variable hold
      informations about the algorithm used to get the
      \code{Clustering}. It has three value. The first (named
      \code{algo}) is the algorithm used ; the second
      (named \code{startCond}, see \link{partitionInitialise}) is the starting condition used for the
      algorithm ; the thrid (named \code{imputation}, see \link{imputation}) is the name of the
      imputation method used in case of missing value. Example:
      \code{algorithm =
	c(algo="kmeans",startCond="randomAll",imputation="LI-Bissectrice")}.}
    \item{\code{postProba}}{\code{[matrix(numeric)]}: for each
      individual, probability to belong to a specific
      clusters. Probabilities are 0 or
      1 for k-means ; they are in [0,1] for fuzzy k-means.}
  }
}


\section{Construction}{
  Class \code{Clustering} objects are mainly constructed by some clustering methods
  (like k-means, fuzzy k-means,...) Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{clustering}}.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. 'xxx' can also be the name of a criterion. In this
      case, its value is returned.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to
      \code{value}. Note that only fields
      'convergenceTime','mulitiplicity','criterionNames' and
      'criterionValues' can be changed (the other field are set at the
      contruction and can not be change latters.)}
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

\examples{
### Building Clustering
part <- partition(rep(c("A","B","A"),time=10),nbClusters=2)
myTraj <- as.longData(array(rnorm(30*5*2),dim=c(30,5,2)))

clus <- clustering(myTraj,part)

### Get
clus["clusters"]
clus["percentEachCluster"]

### Set
clus["multiplicity"]<-2
}


\keyword{classes}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
