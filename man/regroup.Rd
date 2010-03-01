\name{regroup}
\Rdversion{1.1}
\alias{regroup}

\title{
 ~ Function: regroup ~
}
\description{
Remove duplicate \code{\linkS4class{Clustering}} present in a
 \code{\linkS4class{ClusterLongData}}or in a \code{\linkS4class{ListClustering}}.
}
\usage{
regroup(object,toOrder=TRUE)
}

\arguments{
  \item{object}{\code{[\linkS4class{ClusterLongData}]} or
    \code{[\linkS4class{ListClustering}]}: object
    that should be simplified.
  }
  \item{toOrder}{\code{[logical]}: shall the \code{Clustering} be
    ordered before regouped?
  }
}

\details{
  A clusterizing algorithm can find a \code{Clustering} several time. It
  is store several time in object \code{ClusterLongData}, encombering the memory.
  \code{regroup} remove the duplicate \code{\linkS4class{Clustering}}.
  Note that if the \code{ClusterLongData} is not ordered, then
  \code{regroup} sort it unless \code{toOrder=FALSE}.
}

\value{
  None (this function change internaly the field of an object, it does not return any values.)
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
### Some data easy to cluster
myCld <- gald(c(15,15,15),functionNoise=function(t){c(rnorm(1,0,1),rnorm(1,0,1))})

### run kml
kml(myCld,,3)
plot(myCld)

### Some clustering has been found several time
### regroup will suppress the duplicate one
regroup(myCld)
plot(myCld)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
