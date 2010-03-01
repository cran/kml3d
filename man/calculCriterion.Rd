\name{calculCriterion}
\Rdversion{1.1}
\alias{calculCriterion}
\alias{calculCriterion,matrix,Partition-method}
\alias{calculCriterion,LongData,Partition-method}

\title{
  ~ Function: calculCriterion ~
}
\description{
  Given a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}}, the fonction \code{calculCriterion}
  calcul some qualities criterion.
}
\usage{
calculCriterion(traj, part, imputationMethod = "LI-Bissectrice", criterionNames = c("calinski"))
}
\arguments{
  \item{traj}{\code{[array]}: object containning the trajectories.}
  \item{part}{\code{[vector(character)]}: partition of the trajectories.}
%  \item{xLongData}{\code{[LongData]}: object on which criterions are calculate.}
%  \item{yPartition}{\code{[Paritition]}: partition of the \code{LongData}}
  \item{imputationMethod}{\code{[character]}: if some value are missing in the
    \code{LongData}, it is necessary to impute them. The function
    \code{calculCriterion} call the function \code{\link{imputation}} using
    the method \code{imputationMethod}.}
  \item{criterionNames}{\code{[vector(character)]}: names of the
    criterions that should be calculate.}
}

\details{
  Given a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}}, the fonction \code{calculCriterion} calculate
  some qualities criterion.

%  If some individual have no clusters (ie if \code{Partition} has some
%  missing values), the corresponding trajectories are exclude from the
%  calculation.

%Note that if there is an empty cluster or an empty longData, most of
%the criterions are anavailable.

  Available criterions include : "Calinski & Harabatz" and "test"
  (random value).
}

\value{A list:
  \itemize{
    \item{calinski}{\code{[numeric]}: Calinski and Harabatz criterion
      c(k)=Trace(B)/Trace(W)*(n-k)/(k-1)}
    \item{test}{code{[numeric]}: \code{test=rnorm()}}
  }
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}},
  \code{\link{imputation}}.
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
##################
### Preparation of some artificial data
myCld <- gald()

### Correct partition
part1 <- partition(rep(1:3,each=50),3)
(cr1 <- calculCriterion(myCld,part1))
plot(myCld,part1)


### Random partition
part2 <- partition(floor(runif(150,1,4)),3)
(cr2 <- calculCriterion(myCld,part2))
plot(myCld,part2)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{methods}
