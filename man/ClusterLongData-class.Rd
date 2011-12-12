\name{ClusterLongData-class}
\Rdversion{1.1}
\docType{class}
\alias{ClusterLongData-class}
\alias{[,ClusterLongData-method}
\alias{show,ClusterLongData-method}

\title{~ Class: ClusterLongData ~}

\description{
  \code{ClusterLongData} is an object containing trajectories and
  associated clusterizations.
}

\section{Objects from the Class}{
  \code{\link{kml3d}} is an algorithm that builds a set of \code{\linkS4class{Clustering}}
  from longitudinal data (object \code{\linkS4class{LongData}}). \code{ClusterLongData}
  is the object containing the original longitudinal data and all the \code{Clustering} that \code{kml3d} finds.\cr

  When created, an \code{ClusterLongData} object simply contains initial
  data (the trajectories).
  After the execution of \code{kml3d}, it contains
  the original data and a \code{\linkS4class{ListClustering}} holding
  all the \code{\linkS4class{Clustering}} find by \code{kml3d}.

  Please note that if \code{kml3d} is executed several times, every new \code{Clustering}
  are added to the original ones, no pre-existing \code{Clustering} are erased.
}
\section{Slots}{
  \describe{
    \item{\code{idAll}}{\code{[vector(character)]}: Single identifier
      for each of the longData (each individual). Usefull for exporting clusters.}
    \item{\code{idFewNA}}{\code{[vector(character)]}: Restriction of
      \code{idAll} to the trajectories that does not have 'too many' missing
      value. See \code{maxNA} for details.}
    \item{\code{time}}{\code{[numeric]}: Time at which measures are made.}
    \item{\code{varNames}}{\code{[vector(character)]}: Names of the variable measured.}
    \item{\code{traj}}{\code{[array(numeric)]}: Contains
      the longitudianl data. Each horizontal plan (first dimension) corresponds to the trajectories of an
      individual. Vertical plans (second dimension) refer to the time at which measures
      are made. Transversal plans (the third dimension) are for variables.}
    \item{\code{dimTraj}}{\code{[vector3(numeric)]}: size of the array
      \code{traj} (ie \code{c(length(idFewNA),length(time),length(varNames))}).}
    \item{\code{maxNA}}{\code{[numeric]} or \code{[vector(numeric)]}:
      Individual whose trajectories contain more missing value than
      \code{maxNA} are exclude from \code{traj} and will no be use in
      the analysis. Their identifier is preserved in \code{idAll} but
      not in \code{idFewNA}. When \code{maxNA} is a single number, it is
      used for all the variables.}
    \item{\code{reverse}}{\code{[matrix(numeric)]}: contain the
      mean (first line) and the standard deviation (second line) used to
      normalize the data. Usefull to restaure the original data after a
      scaling operation.}
    \item{\code{criterionActif}}{[character]: Store the criterion name
      that will be used by fonctions that need a single criterion
      (like \link[=plot,ListClustering-method]{plot} or \link{ordered}).}
    \item{\code{criterionPossibles}}{[vector(character)]: Names of all
      the criterion used for \code{Clustering}. Note that all of them
      might not be available for all the \code{Clustering}.}
    \item{\code{initializationMethod}}{[vector(chararcter)]: list all
      the initialization method that has allready been used to find some
      \code{Clustering}
      (usefull to not run several time a deterministic method).}
    \item{\code{sorted}}{\code{[logical]}: are the \code{Clustering}
      curently hold in the object sorted in decreasing (according to
      \code{criterionActif}) order ?}
    \item{\code{c2}}{[list(Clustering)]: list of
      \code{Clustering} with 2 clusters.}
    \item{\code{c3}}{[list(Clustering)]: list of
      \code{Clustering} with 3 clusters.}
    \item{\code{c26}}{[list(Clustering)]: list of
      \code{Clustering} with 26 clusters.}
  }
}
\section{Extends}{
  Class \code{\linkS4class{LongData}}, directly.
  Class \code{\linkS4class{ListClustering}}, directly.
}
\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. Inherit from  \code{\linkS4class{LongData}} and
      \code{\linkS4class{ListClustering}}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.
      \code{xxx}. Inherit from  \code{\linkS4class{ListClustering}}.}
    \item{\code{\link[=plot,ClusterLongData-method]{plot}}}{Display the \code{ClusterLongData}, one
      graph for each variable, according to the best \code{\linkS4class{Clustering}}.}
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
### Building longData
traj <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2))

myCld <- clusterLongData(
    traj=traj,
    idAll=as.character(c(100,102,103)),
    time=c(1,2,4,8,15),
    varNames=c("P","A"),
    maxNA=3
)

### Show
myCld

### Get
myCld['varNames']

### Set
myCld['criterionActif']<-"test"

### Plot
plot(myCld)

}
\keyword{classes}
