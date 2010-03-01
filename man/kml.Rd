\name{kml}
\alias{kml}
\alias{kml-method}
\alias{kml,ClusterLongData-method}

\title{~ Algorithm kml: K-means for Longitidinal data ~}

\description{
  \code{kml} is a new implementation of k-means for longitudinal data (or trajectories). This algorithm is able to deal with missing value and
  provides an easy way to re roll the algorithm several times, varying the starting conditions and/or the number of clusters looked for.

  Here is the description of the algorithm. For an overview of the package, see \link{kml3d-package}.
}

\usage{
kml(object,nbClusters=2:6,nbRedrawing=3,toPlot="none",paramKml=parKml(),
  criterionNames=c("calinski","test"))
}

\arguments{
  \item{object}{[ClusterLongData]: contains trajectories to clusterize
    as well as previous \code{\linkS4class{Clustering}}.
  }
  \item{nbClusters}{[vector(numeric)]: Vector containing the number of clusters
    with which \code{kml} must work. By default,
    \code{nbClusters} is \code{2:6} which indicates that \code{kml} must
    search partitions with respectively 2, then 3, ... up to 6
    clusters. Maximum number of cluster is 26.
  }
  \item{nbRedrawing}{[numeric]: Sets the number of time that k-means
    must be re-run (with different starting conditions) for each number
    of clusters.
  }
  \item{toPlot}{\code{[character]}: during computation, \code{kml} can
    display some graphes. If \code{toPlot="traj"},
    then the trajectories are plot (like with function
    \code{\link{plot,ClusterLongData}}).
    If \code{toPlot="criterion"}, the quality criterions are plot (like with function
    \code{\link{plot,ListClustering}}). If \code{toPlot="both"}, the graphic
    windows is split in two and both graphs are displayed. If "none",
    there is no graphical display.
  }
  \item{paramKml}{\code{[\linkS4class{ParKml}]}: set the option used
    by \code{kml} (like the starting condition, the imputation methods,
    the save frequency, the maximum number of iteration, , the distance
    used...) See \linkS4class{ParKml} for details.
  }
  \item{criterionNames}{\code{[character]}: Criterion that shall be
    compute ofr each \code{\linkS4class{Clustering}}.
  }
}

\details{
  \code{kml} works on object of class \code{ClusterLongData}.
  For each number \code{i} included in \code{nbClusters}, \code{kml} computes a
  \code{\link{Clustering}} with \code{i} clusters then stores it in the field
  \code{ci} of the object \code{ClusterLongData} according to its number
  of clusters \code{i}.
  The algorithm starts over as many times as it is told in \code{nbRedrawing}. By default, it is executed for 2,
  3, 4, 5 and 6 clusters 20 times each, namely 100 times.

  When a \code{Clustering} has been found, it is added to the slot
  \code{ci}. \code{ci} stores the all \code{Clustering} with
  \code{i} clusters. Inside a sublist, the
  \code{Clustering} are sorted either in the creation order. They can
  also be sort from the biggest quality criterion to
  the smallest (the best are stored first) using \code{\link{ordered,ListClustering}}.

  Note that \code{Clustering} are saved throughout the algorithm. If the user
  interrupts the execution of \code{kml}, the result is not lost. If the
  user run \code{kml} on an object then run \code{kml} again on the same object, the
  \code{Clustering} computed the second time are added to
  the ones already present in the object (unless you "clear" some
  list, see \code{object["clusters","clear"]<-value} in
  \code{\linkS4class{ClusterLongData}}).

  The possible starting conditions are "randomAll", "randomK" and
  "maxDist", as defined in \code{\link{partitionInitialise}}. In
  addition, the method "allMethods" is a shortcut that run a "maxDist", a "randomAll"
  and "randomK" for all the other re rolling.
}

\section{Optimisation}{
  Behind kml, there are two different procedures :
  \enumerate{
    \item Fast: when the parameter \code{distanceName} is set to a classical
    distance (one of "euclidean", "maximum", "manhattan", "canberra",
    "binary" or "minkowski") and \code{toPlot} is set to
    \code{"criterion"} or \code{"none"} (the default), \code{kml} call a \code{C}
    compiled (optimized) procedure.
    \item Slow: when the user defines its own (non-classical) distance or if he wants
    to see the construction of the clusters by setting \code{toPlot} to
    \code{"both"} or \code{"traj"}, \code{kml} uses a \code{R} non compiled
    programme.
  }
  The \code{C} prodecure is arround 25 times faster than the \code{R} one.

  So we advice to use the R procedure 1/ for trying some new method
  (like using a new distance) or 2/ to "see" the very first cluster
  construction, in order to check that every thing goes right. Then it's
  time to switch to the \code{C} procedure (like we do in \code{Example} section).

  If for a specific use, you need a different distance, feel free to
  contact the author.
}

\value{
  None. This function internaly a \code{\linkS4class{ClusterLongData}}
  object by adding some \code{\link{Clustering}} to it.
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





\seealso{
  Overview: \code{\link{kml3d-package}} \cr
  Classes : \code{\linkS4class{ClusterLongData}}, \code{\linkS4class{Clustering}} \cr
  Methods : \code{\link{clusterLongData}}, \code{\link{choice}}
}

\examples{
### Generation of some data
cld1 <- generateArtificialLongData(c(15,15,15))

### We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
#     We want to "see" what happen (so toPlot="both")
kml(cld1,2:5,3,toPlot="both")

### 3 seems to be the best.
#     We don't want to see again, we want to get the result as fast as possible.
#     Just, to check the overall process, we plot the criterion evolution
kml(cld1,3,10,toPlot="criterion")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


\keyword{dplot}    % Computations Related to Plotting
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
