\name{dist3d}
\alias{dist3d}

\title{
   ~ Function: dist3d ~
}
\description{
  Compute the distante between two joint trajectories.
}
\usage{
dist3d(x, y, method = "euclidian", power = 2)
}

\arguments{
  \item{x}{\code{[matrix(numeric)]}: first trajectory. The colomn are
    time, the line are variables.}
  \item{y}{\code{[matrix(numeric)]}: second trajectory. The colomn are
    time, the line are variables.}
  \item{method}{\code{[character]}: method used. Should be one of the
    method used by the function \code{\link[stats:dist]{dist}}.}
  \item{power}{\code{[numeric]}: if \code{method="minkowski"},
  \code{power}is the power used.}
}

\details{
  Compute the distante between two joint trajectories, using one of the
  distance define by \code{\link[stats:dist]{dist}}.
}

\value{
A numeric
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
  ### Generate artificial data
  myCld <- gald()

  ### Distance between individual 1 and 3 (there are in the same group)
  dist3d(myCld['traj'][1,,],myCld['traj'][3,,])

  ### Distance between individual 1 and 51 (there are in two different groups)
  dist3d(myCld['traj'][1,,],myCld['traj'][51,,])
}