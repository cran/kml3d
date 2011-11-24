\name{kml3d-package}
\alias{kml3d-package}
\alias{[,ParChoice-method}
\alias{[<-,ParChoice-method}
\title{~ Overview: KmL3D, K-means for joint Longitudinal data ~}

\description{
  \code{kml3d} is a new implementation of k-means for longitudinal data (or trajectories).
  Here is an overview of the package.
}
\details{
  \tabular{ll}{
    Package: \tab kml3d\cr
    Type: \tab Package\cr
    Version: \tab 0.6\cr
    Date: \tab 2011-11-24\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,graphics,rgl,misc3d\cr
    URL:\tab http://www.r-project.org\cr
    URL:\tab http://christophe.genolini.free.fr/kml\cr
  }
}


\section{Overview}{
  To cluster data, \code{kml3d} go through three steps, each of which
  is associated to some functions:

  \enumerate{
    \item Data preparation

    \item Building "optimal" clusterization.

    \item Exporting results

    \item Visualizing and exporting 3D object
  }
}


\section{1. Data preparation}{
  \code{kml} works on object of class \code{\linkS4class{ClusterLongData}}.
  Data preparation therefore simply consists in transforming data into an object \code{ClusterLongData}.
  This can be done via function
  \code{\link[=clusterLongData]{clusterLongData}}
  (\code{\link[=clusterLongData]{cld}} in short) or
  \code{\link[=as.clusterLongData]{as.clusterLongData}}
  (\code{\link[=as.clusterLongData]{as.cld}} in short).
  The formers lets the user build some data from scratch, the latters
  converts a \code{data.frame} or an \code{array} in \code{\linkS4class{ClusterLongData}}.

  Working on several variables mesured on different scales can give to
  much weight to one of the dimention. So the function \code{\link{scale}} normalizes data.

  Instead of working on real data, one can also work on artificial
  data. Such data can be created with \code{\link{generateArtificialLongData}}
  (\code{\link{gald}} in short).

}

\section{2. Building "optimal" clustering}{
  Once an object of class \code{\linkS4class{ClusterLongData}} has been created, the algorithm
  \code{\link{kml}} can be run.

  Starting with a \code{ClusterLongData}, \code{kml} built several \code{\linkS4class{Clustering}}.
  A object of class \code{Clustering} is a partition of trajectories
  into subgroups. It also contains some information like the
  percentage of trajectories contained in each group or some quality critetion (like the Calinski &
  Harabasz).\cr

  \code{kml} is a "hill-climbing" algorithm. The specificity of this
  kind of algorithm is that it always converges towards a maximum, but
  one cannot know whether it is a local or a global maximum. It offers
  no guarantee of optimality.

  To maximize one's chances of getting a quality \code{Clustering},
  it is better to execute the hill climbing algorithm several times,
  then to choose the best solution. By default, \code{kml} executes the hill climbing algorithm 20 times
  and chooses the \code{Clustering} maximising the Calinski and Harabatz
  criterion.

  Likewise, it is not possible to know \emph{beforehand} the optimum number of clusters.
  On the other hand, \emph{afterwards}, it is possible to calculate
  clues that will enable us to choose.

  In the end, \code{kml} tests by default 2, 3, 4, 5 et 6 clusters, 20 times each.
}

\section{3. Exporting results}{
  When \code{\link{kml}} has constructed some
  \code{Clustering}, the user can examine them one by one and choose
  to export some. This can be done via function
  \code{\link{choice}}. \code{choice} opens a graphic windows showing
  various information including the trajectories cluterized by a specific
  \code{Clustering}.

  When some \code{Clustering} has been selected (the user can select
  more than 1), it is possible to
  save them. The clusters are therefore exported towards the file
  \code{nom-cluster.csv}. Criteria are exported towards
  \code{nom-criteres.csv}. The graphs are exported according to their
  extension.
}

\section{4. Visualizing and exporting 3D object}{
  \code{kml3d} also propose tools to visualize the trajectories in
  3D. \code{\link{plot3d}} using the library \code{rgl} to plot two
  variables according to time (either the all set of trajectories, or
  just the mean trajectories). Then the user can make the
  graphical representation turn using the mouse. \code{\link{plot3dPdf}} build an
  \code{\link[misc3d:makeTriangles]{Triangles}} object. These kind of
  object can be include in a \code{pdf} file using
  \code{\link{saveTrianglesAsASY}} and the software
  \code{asymptote}. Once again, it is possible to make the image in the
  pdf file move using the mouse -so the reader gets real 3D-.
}



\section{How to get help?}{
  For those who are not familiar with S4 programming:
  In S4 programming, each function can be adapted for some specific
  arguments.
  \itemize{
    \item{To get help on a function (for example \code{plot}), use:
      \code{?(plot)}.}
    \item{To get help on a function adapted to its argument (for example \code{plot}
      on argument \code{LongData}), used: \code{?"plot,LongData"}.}
  }
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
### 1. Data Preparation
myCld <- generateArtificialLongData(c(15,15,15))

### 2. Building "optimal" clusterization (with only 3 redrawings)
kml(myCld,nbRedrawing=3)

### 3. Exporting results
try(choice(myCld))

### 4. Visualizing in 3D
plot3d(myCld)
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
\keyword{models}   % Statistical Models



