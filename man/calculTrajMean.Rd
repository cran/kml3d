\name{calculTrajMean}
\Rdversion{1.1}
\alias{calculTrajMean}
\title{
  ~ Function: calculTrajMean ~
}
\description{
  Given some longitudinal data and a \code{\linkS4class{Partition}},
  \code{calculTrajMean} computes the mean trajectories of each cluster.
}
\usage{
calculTrajMean(traj, part,centerMethod=function(x){mean(x,na.rm=TRUE)})
}

\arguments{
  \item{traj}{\code{[array]}: longitudinal data.}
  \item{part}{\code{[vector(character)]}: affectation of each individual.}
  \item{centerMethod}{\code{[numeric <- function(vector)]}: function used
    to compute the clusters' centers.}
}

\details{
  EM algorithm (like k-means) alternates between two phases : Esperance and
  Maximisation. During Esperance, the center of each cluster is
  evaluated. This is what \code{calculTrajMean} does.

  Note that \code{calculTrajMean} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with an array of trajectories.

  \code{affectIndiv} used with \code{\link{calculTrajMean}} simulates one k-means step.
}

\value{
  An array of dimention \code{(k,t,v)} with \code{k} number of groups, \code{t} number of
  time mesurement and \code{v} number of variables.
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
#######################
### calculTrajMean

### Some LongitudinalData
traj <- gald()["traj"]

### A partition
part <- partition(floor(runif(150,1,5)),3)
plot(as.longData(traj),part)

### Clusters center
(center <- calculTrajMean(traj,part["clusters"]))

### Unusual center
calculTrajMean(traj,part["clusters"],centerMethod=function(x)median(x,na.rm=TRUE))


#################
### K-means simulation (4 steps)
plot(as.longData(traj),part)
for (i in 1:4){
    part <- affectIndiv(traj,center)
    center <- calculTrajMean(traj,part["clusters"])
    plot(as.longData(traj),part)
}
}

