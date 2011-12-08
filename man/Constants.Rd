\name{Constants}
\alias{MAX_CLUSTERS}
\alias{CLUSTER_NAMES}
\alias{CRITERION_MIN_OR_MAX}
\alias{CHOICE_STYLE}

\docType{data}
\title{
  ~ Constants define in the package ~
}
\description{
  Constants define in the package ~
}
\usage{
MAX_CLUSTERS
CLUSTERS_NAMES
CRITERION_MIN_OR_MAX
CHOICE_STYLE
}

\value{
  MAX_CLUSTERS = 26
  CLUSTER_NAMES = paste("c",2:MAX_CLUSTERS,sep="")
  CRITERION_MIN_OR_MAX = c(calinski=1,test=1,test2=-1)
  CHOICE_STYLE <- list(
    typeTraj=c("l","l","n"),
    colTraj=c("clusters","black","black"),
    typeMean=c("b","b","b","b","l","l","n"),
    colMean=c("clusters","black","clusters","black","clusters","black","black"),
    pchMean=c("letters","letters","symbols","symbols","letters","letters","letters")
  )
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
### Maximum number of clusters that kml3d can deal with
#2:MAX_CLUSTERS

### Names of the field that save clusters in object 'ClusterLongData'
#cat(CLUSTER_NAMES,"\n")

### Shall a criterion be maximised (1) or minimised (-1) ?
#92.4*CRITERION_MIN_OR_MAX[2]

### Define the style use by choice
#CHOICE_STYLE[['typeTraj']][2]
}
\keyword{datasets}
