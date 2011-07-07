\name{affectIndiv}
\Rdversion{1.1}
\alias{affectIndiv}

\title{
  ~ Function: affectIndiv ~
}
\description{
  Given some longitudinal data (trajectories) and k clusters centers, \code{affectIndiv}
  affect each individual to the cluster whose center is the closest.
}
\usage{
affectIndiv(traj, clustersCenter, distance = dist3d)
}

\arguments{
  \item{traj}{\code{[array(numeric)]}: longitudinal data. Each line is an individual,
    each column is a time measurement.}

  \item{clustersCenter}{\code{[matrix]}: cluster center. Each line is a
    cluster centers, each column is a time measurement.}

  \item{distance}{\code{[character]}: distance between an
    individual and a clusters centre.}
}

\details{
  EM algorithm (like k-means) alternates between two phases : Esperance and Maximisation. During
  Maximisation, each individual is affected to the closest cluster.
  This is what \code{affectIndiv} does. The default distance
  \code{\link{dist3d}} is the euclidean distance but the user can define some non-classical distance.

  Note that \code{affectIndiv} does not work with
  a \code{\linkS4class{ClusterLongData}} object but with an array of trajectories.

  \code{affectIndiv} used with \code{\link{calculTrajMean}} simulates
  one k-means step (see examples).
}
\value{
  Object of class \code{\linkS4class{Partition}}.
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
### affectIndiv

### Some trajectories
traj <- gald()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),,]

### Affectation of each individual
part <- affectIndiv(traj,center)
plot(as.longData(traj),part)

### Unusual distance
distCor <- function(x,y){return(max(cor(x,y)))}
part2 <- affectIndiv(traj,center,distCor)
plot(as.longData(traj),part2)


#################
### K-means simulation (4 steps)
plot(as.longData(traj),part)
for (i in 1:4){
    center <- calculTrajMean(traj,part["clusters"])
    part <- affectIndiv(traj,center)
    plot(as.longData(traj),part)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

