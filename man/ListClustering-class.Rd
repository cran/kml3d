\name{ListClustering-class}
\docType{class}
\alias{ListClustering}
\alias{ListClustering-class}
\alias{[,ListClustering-method}
\alias{[<-,ListClustering-method}
\alias{show,ListClustering-method}
\alias{listClustering}
\alias{listClustering-method}

\title{~ Class: ListClustering ~}
\description{
  An object of class \code{ListClustering} contain several liste of
  \code{Clustering} sorted by cluster numbers.
}

\section{Objects from the Class}{
  Objects are mainly design to store the numerous \code{Clustering} found
  by \code{\link{kml3d}}.
}

\section{Slots}{
  \describe{
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
    \item{\code{c2}}{[list(Clustering)]: list of \code{Clustering} with 2 clusters.}
    \item{\code{c3}}{[list(Clustering)]: list of \code{Clustering} with 3 clusters.}
    \item{\code{c4}}{[list(Clustering)]: list of \code{Clustering} with 4 clusters.}
    \item{\code{c5}}{[list(Clustering)]: list of \code{Clustering} with 5 clusters.}
    \item{\code{c6}}{[list(Clustering)]: list of \code{Clustering} with 6 clusters.}
    \item{\code{c7}}{[list(Clustering)]: list of \code{Clustering} with 7 clusters.}
    \item{\code{c8}}{[list(Clustering)]: list of \code{Clustering} with 8 clusters.}
    \item{\code{c9}}{[list(Clustering)]: list of \code{Clustering} with 9 clusters.}
    \item{\code{c10}}{[list(Clustering)]: list of \code{Clustering} with 10 clusters.}
    \item{\code{c11}}{[list(Clustering)]: list of \code{Clustering} with 11 clusters.}
    \item{\code{c12}}{[list(Clustering)]: list of \code{Clustering} with 12 clusters.}
    \item{\code{c13}}{[list(Clustering)]: list of \code{Clustering} with 13 clusters.}
    \item{\code{c14}}{[list(Clustering)]: list of \code{Clustering} with 14 clusters.}
    \item{\code{c15}}{[list(Clustering)]: list of \code{Clustering} with 15 clusters.}
    \item{\code{c16}}{[list(Clustering)]: list of \code{Clustering} with 16 clusters.}
    \item{\code{c17}}{[list(Clustering)]: list of \code{Clustering} with 17 clusters.}
    \item{\code{c18}}{[list(Clustering)]: list of \code{Clustering} with 18 clusters.}
    \item{\code{c19}}{[list(Clustering)]: list of \code{Clustering} with 19 clusters.}
    \item{\code{c20}}{[list(Clustering)]: list of \code{Clustering} with 20 clusters.}
    \item{\code{c21}}{[list(Clustering)]: list of \code{Clustering} with 21 clusters.}
    \item{\code{c22}}{[list(Clustering)]: list of \code{Clustering} with 22 clusters.}
    \item{\code{c23}}{[list(Clustering)]: list of \code{Clustering} with 23 clusters.}
    \item{\code{c24}}{[list(Clustering)]: list of \code{Clustering} with 24 clusters.}
    \item{\code{c25}}{[list(Clustering)]: list of \code{Clustering} with 25 clusters.}
    \item{\code{c26}}{[list(Clustering)]: list of \code{Clustering} with 26 clusters.}
  }
}


\section{Construction}{
  Class \code{ListClustering} objects are mainly constructed by
  \code{\link{kml3d}}.
  Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{listClustering}} that does
  create an empty object.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. If 'xxx' is 'criterionValues', only the list of the
      value corresponding to \code{criterionActif} is returned. If 'xxx'
      is a number, the list 'cxxx' is returned.}
    \item{\code{object['xxx',criterionName]}}{Get the criterion values
      of the criterion named \code{criterionName}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to
      \code{value}. Note that fields 'c2','c3',...,'c26' can not be set
      that way ; use object['add']<-value instead.}
    \item{\code{object['add']<-Clustering}}{Add a Clustering object with
      i clusters to the field 'ci'. 'i' has not to be gived by the user,
      the Clustering is automaticly send in the correct list.}
    \item{\code{object['clear']<-ci}}{Clear the list 'ci'.}
    \item{\code{listClustering}}{Constructor. Build an empty object.}
    \item{\code{getCriterion}}{aze}
    \item{\code{sortClustering}}{aze}
    \item{\code{plotCriterion}}{aze}
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
##############
### Preparing data

### Longdata
traj <- as.longData(array(rnorm(30*5*2),dim=c(30,5,2)))

### Some clustering
part2 <- partition(rep(c("A","B","A"),time=10),nbClusters=2)
clust2 <- clustering(traj,part2)

part3 <- partition(rep(c("A","B","C"),time=10),nbClusters=3)
clust3 <- clustering(traj,part3)

part3b <- partition(rep(c("A","B","C","B","C"),time=6),nbClusters=3)
clust3b <- clustering(traj,part3b)

part4 <- partition(rep(c("A","B","A","C","D"),time=6),nbClusters=4)
clust4 <- clustering(traj,part4)

################
### ListClustering
listClus <- listClustering()
listClus['criterionActif'] <-"calinski"
plot(listClus)

listClus["add"] <- clust2
listClus["add"] <- clust2
listClus["add"] <- clust3
listClus["add"] <- clust3b
listClus["add"] <- clust4
listClus["add"] <- clust4
plot(listClus)
}


\keyword{classes}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
