\name{parKml}
\alias{parKml}

\title{ ~ Function: parKml ~}

\description{
  \code{parKml} is the constructor of object \code{\linkS4class{ParKml}}.
}
\usage{
parKml(saveFreq = 100, maxIt = 200, imputationMethod = "LI-Bissectrice", distanceName = "euclidean", power = 2, distance = function() {
}, centerMethod = meanNA, startingCond = "allMethods", distanceStartingCond = function(x, y) dist(rbind(x, y)), nbCriterion = 100)
}
\arguments{
  \item{saveFreq}{\code{[numeric]}: Long computations can take several
    days. So it is possible to save the object \code{\linkS4class{ClusterLongData}}
    on which works \code{\link{kml3d}} once in a while. \code{saveFreq}
    defines the frequency of the saving
    process. The \code{ClusterLongData} is saved every \code{saveFreq}
    clustering calculations. The object is saved in the file
    \code{objectName.Rdata} in the curent folder.
  }
  \item{maxIt}{\code{[numeric]}: Set a limit to the number of iteration if
    convergence is not reached.
  }
  \item{imputationMethod}{\code{[character]}: the calculation of quality
    criterion can not be done if some value are
    missing. \code{imputationMethod} define the method use to impute the
    missing value. It should be one of "LI-Bissectrice" (the default)
    "LOCF","LOCB","LI-Global","LI-Local" or "LI-LOCBF".
    See \code{\link{imputation}} for detail.
  }
  \item{distanceName}{\code{[character]}: name of the
    \code{distance} used by k-means. If the \code{distanceName} is one of
    "manhattan", "euclidean", "minkowski", "maximum", "canberra" or
    "binary", a compiled optimized version specificaly design for
    trajectories version is used. Otherwise, the function define in
    the slot \code{distance} is used.
  }
  \item{power}{\code{[numeric]}: If \code{distanceName="minkowski"}, this define
    the power that will be used.
  }
  \item{distance}{\code{[numeric <- function(trajA,trajB)]}: function that computes the
    distance between two trajectories. If no function is specified, the Euclidian
    distance with Gower adjustment (to deal with missing value) is
    used.
  }
  \item{centerMethod}{\code{[numeric <-
      function(vector(numeric))]}: k-means algorithm computes the centers of
    each cluster. It is possible to personalize the definition of
    "center" by defining a function "centerMethod". This function should
    take a vector of numeric as argument and return a single numeric -the
    center of the vector-.
  }
  \item{startingCond}{\code{[character]}: specifies the starting
    condition. Should be one of  "maxDist", "randomAll", "randomK" or
    "allMethods". See \code{\link{partitionInitialise}} for details.
  }
  \item{distanceStartingCond}{\code{[numeric <- function(trajA,trajB)]}: some starting condition needs
    to compute the distance matrix of the
    trajectories. \code{distanceStartingCond} define the distance that will be
    use to calculate this matrix. See \code{\link{partitionInitialise}} for detail.
  }
  \item{nbCriterion}{\code{[numeric]}: set the maximum number of
    quality criterion that are display on the graph (since displaying
    a high criterion number an slow down the overall process, the
    default value is 100).
  }
}

\details{
  \code{parKml} is the constructor of object \code{\linkS4class{ParKml}}.
}
\value{
  An object \code{\linkS4class{ParKml}}.
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
### Generation of some data
cld1 <- generateArtificialLongData(c(15,15,15))

### Setting two different set of option :
(option1 <- parKml())
(option2 <- parKml(distanceName="maximum",centerMethod=function(x)median(x,na.rm=TRUE)))

### Running kml3d We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
kml3d(cld1,4,1,toPlot="both",paramKml=option1)
kml3d(cld1,4,1,toPlot="both",paramKml=option2)
}
