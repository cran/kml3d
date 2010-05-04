\name{matDist3d}
\alias{matDist3d}
\title{
  ~ Function: matDist3d ~
}
\description{
  Compute the distance matrix between joint trajectories.
}
\usage{
matDist3d(data,distance=dist3d)
}
\arguments{
  \item{data}{\code{[array(numeric)]}:  Contains
    the longitudianl data. Each horizontal plan (first dimension) corresponds to the trajectories of an
    individual. Vertical plans (second dimension) refer to the time at which measures
    are made. Transversal plans (the third dimension) are for
    variables.
  }
  \item{distance}{\code{[numeric <- function(array)]}: function used to
    evaluate the distance between trajectories.
  }
}

\details{
  Compute the symetric distance matrix between joint trajectories. The distance
  between individual i and j is at [i,j] or [j,i]
}
\value{
A symetric matrix
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
### Building an array
tr1 <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4), dim=c(3,5,2))

### Computing distance matrix
matDist3d(tr1)
}