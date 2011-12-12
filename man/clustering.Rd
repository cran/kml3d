\name{clustering}
\Rdversion{1.1}
\alias{clustering}

\title{
  ~ Function: clustering ~
}
\description{
  \code{clustering} is the constructor of the class \code{\linkS4class{Clustering}}.
}
\usage{
clustering(xLongData, yPartition, convergenceTime = 0, multiplicity = 1,criterionNames = c("calinski", "ray","davies","random"),
  algorithm = c(algo = "kmeans", startCond = "", imputation = "LI-Bissectrice")
)
}
\arguments{
  \item{xLongData}{[LongData]: longitudinal data on which the
    clusterization has been run.}
  \item{yPartition}{[Partition]: object that will be turn into a \code{Clusterization}}
  \item{convergenceTime}{[numeric]: if the clusterization has been
    obtained through an algorithm, save the number of steps of this algorithm
    before convergence.}
  \item{multiplicity}{[numeric]: if the clusterization has been
    obtained several time, this variable saves the number of time that
    this particular \code{Clustering} is obtained.}
  \item{criterionNames}{[vector(character)]: criterions used to evaluate the
    quality of the partitioning.}
  \item{algorithm}{[vector3(character)]: This variable hold
    informations about the algorithm used to get the
    \code{Clustering}. It has three value. The first (named
    \code{algo}) is the algorithm used ; the second
    (named \code{startCond}, see \link{partitionInitialise}) is the starting condition used for the
    algorithm ; the thrid (named \code{imputation}, see \link{imputation}) is the name of the
    imputation method used in case of missing value. Example:
    \code{algorithm=
      c(algo="kmeans",startCond="randomK",imputation="LI-Bissectrice")}.}
}

\details{
  In \code{Kml3d}, strickly speaking, a \code{\link{Partition}} is just a sequence of
  letters (independent of any trajectories) ; a \code{Clustering} is
  a \code{Partition} associated with a set of trajectories
  with some additional
  information like qualities criterion, size of the clusters, algorithm
  used to get the \code{Clustering}...
}

\value{
  Object of class \code{\linkS4class{Clustering}}.
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


\section{Author(s)}{
  Christophe Genolini\cr
  INSERM U669 / PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  Modal'X / Universite Paris Ouest-Nanterre- La Defense \cr\cr

  Contact author : \email{genolini@u-paris10.fr}
}


\examples{
### Creation of a partition
part <- partition(rep(c(1,2),75),2)

### Some trajectories
myLd <- gald(clusterLD=FALSE)

### Tranformation of part into a Clusterization
clustering(myLd,part)
}
