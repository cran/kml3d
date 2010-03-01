\name{ordered(ListClustering)}
\Rdversion{1.1}
\alias{ordered}
\alias{ordered,ListClustering}
\alias{ordered,ListClustering-method}
\title{
  ~ Function: ordered(ListClustering) ~
}
\description{
  Sort the \code{\linkS4class{Clustering}} of a
  \code{\linkS4class{ListClustering}} according to a quality criterion.
  (the function \code{ordered} for \code{Partition} is describe
  \link[=ordered,Partition-method]{here}).
}

\usage{
ordered(x,...)
}

\arguments{
  \item{x}{[ListClustering]: Object whose \code{Clustering} should be sort.}
  \item{\dots}{Note used, for S4 compatibility only.}
}

\details{
  Sort the \code{Clustering} of a \code{ListClustering} for each
  list (sort the 'c2' list, the 'c3' list,...) according to a quality criterion.
  The criterion used to sort is the one in the field \code{criterionActif}. The
  increasing or decreasing option is selected according to the
  criterion using the constant \code{\link{CRITERION_MIN_OR_MAX}}.
  \code{Clustering} that does not have a value for the selected
  criterion are put last.
}
\value{
  This function change internaly the order of the fields \code{c2},
  \code{c3}, ... \code{c26} of an object. In addition, it return the
  permutation matrix (the matrix use to re-ordered the \code{ci}).
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
##############
### Preparing data

### Longdata
traj <- as.longData(array(rnorm(30*5*2),dim=c(30,5,2)))

### Some clustering
part2 <- partition(rep(c("A","B","A"),time=10),nbClusters=2)
clust2 <- clustering(traj,part2)
part2b <- partition(rep(c("A","B","B"),time=10),nbClusters=2)
clust2b <- clustering(traj,part2b)

part3 <- partition(rep(c("A","B","C"),time=10),nbClusters=3)
clust3 <- clustering(traj,part3)
part3b <- partition(rep(c("A","B","C","B","C"),time=6),nbClusters=3)
clust3b <- clustering(traj,part3b)

################
### ListClustering
listClus <- listClustering()
listClus['criterionActif'] <-"calinski"
plot(listClus)

listClus["add"] <- clust2
listClus["add"] <- clust2b
listClus["add"] <- clust2
listClus["add"] <- clust3
listClus["add"] <- clust3b
listClus["add"] <- clust3
plot(listClus)

###
ordered(listClus)
plot(listClus)
}
\keyword{methods}
