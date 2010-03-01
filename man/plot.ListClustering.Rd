\name{plot(ListCLustering)}
\Rdversion{1.1}
\alias{plot,ListClustering}
\alias{plot,ListClustering-method}
\alias{plot,ListClustering,missing-method}

\title{
~ Function: plot for ListClustering ~
}
\description{
  This function graphically displays the quality criterion of all the
  \code{\linkS4class{Clustering}} of a \code{\linkS4class{ListClustering}} object.
}
\usage{
\S4method{plot}{ListClustering,missing}(x, criterion=x['criterionActif'],nbCriterion = 100, standardized = TRUE)
}

\arguments{
  \item{x}{\code{[ListClustering]}: object whose quality criterion should be
    displayed.}
  \item{criterion}{\code{[character]} or \code{[vector(character)]}:
    name of the criterion(s) to plot (default= "calinksi").}
  \item{nbCriterion}{[numeric]: set the maximum number of criterion to
    display for a specific clusters number. When
    several criterions are plot, \code{nbCriterion} is set to 1 (default=100).}
  \item{standardized}{[logical]: some criterion sould be maximized, some
    other should be minimized ; some criterion take big values, some take
    small values. If \code{standardized} is \code{TRUE},
    all the criterion are scale to be in [0,1] and / or are
    inverted so the best possible value become 1 for all the criterion, and all of them should
    be maximized (default=TRUE).}
}


\details{
  This function display graphically the quality criterion (probably to
  decide the best clusters' number). It can either display all the
  value find for a specific criterion ; this is useful to see the consistency of the result : is
  the best clusterization obtain several time or only one ? It can also
  display the values find by several criterions : this helps to compare
  the best cluster's number find by various ways.
}

\value{
  No value are return. A graph is printed.
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
part2b <- partition(rep(c("A","B","A"),time=10),nbClusters=2)
clust2b <- clustering(traj,part2b)

part3 <- partition(rep(c("A","B","C"),time=10),nbClusters=3)
clust3 <- clustering(traj,part3)
part3b <- partition(rep(c("A","B","C","B","C"),time=6),nbClusters=3)
clust3b <- clustering(traj,part3b)

part4 <- partition(rep(c("A","B","A","C","D"),time=6),nbClusters=4)
clust4 <- clustering(traj,part4)
part4b <- partition(rep(c("A","B","A","C","D"),time=6),nbClusters=4)
clust4b <- clustering(traj,part4b)

################
### ListClustering
listClus <- listClustering()
listClus['criterionActif'] <-"calinski"
plot(listClus)

listClus["add"] <- clust2
listClus["add"] <- clust2b
listClus["add"] <- clust3
listClus["add"] <- clust3b
listClus["add"] <- clust4
listClus["add"] <- clust4b
plot(listClus)

### Plot a single criterion
ordered(listClus)
plot(listClus)

### Comparing several criterion
plot(listClus,nbCriterion=1,criterion=c("calinski","test"),standardized=FALSE)
plot(listClus,nbCriterion=1,criterion=c("calinski","test"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
