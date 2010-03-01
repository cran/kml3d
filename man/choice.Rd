\name{choice}
\alias{choice}
\alias{choice-methods}
\alias{choice,ClusterLongData-method}

\title{ ~ Function: choice ~}
\description{
  \code{choice} lets the user choose some \code{Clustering} he wants to export.
}
\usage{
choice(object, typeGraph = "bmp",...)
}

\arguments{
  \item{object}{\code{[ClusterLongData]}: Object containing the trajectories and all the clusterizations found by \code{\link{kml}},
    from whom the user want to export some \code{\linkS4class{Clustering}}.}

  \item{typeGraph}{\code{[character]} for every selected \code{clustering}, \code{choice} export some graphs. \code{typeGraph} set
    the format that will be used. Possible formats are \code{\link{bmp}},
    \code{\link{jpeg}}, \code{\link{png}} and \code{\link{tiff}}.}

  \item{\dots}{Parameters for the function \code{\link{bmp}}, \code{\link{jpeg}}, \code{\link{png}} or \code{\link{tiff}}.}
}


\details{
  \code{choice} is a function that lets the user see the \code{\link{Clustering}} found by \code{\link{kml}}.
  At first, \code{choice} opens a graphics window. On the left side are
  qualities criterion of all the \code{Clustering} contained in \code{Object}.
  One \code{Clustering} is 'active', it is the one marked by a black dot.
  On the right side, the trajectories of \code{object} are drawn, according to the active \code{Clustering}.

  From there, \code{choice} offers numerous options.
  \itemize{
    \item{Arrow}{Change the active \code{Clustering}.}
    \item{Space}{Select/unselect a \code{Clustering} (the selected
      \code{Clustering} are surrounded by a circle).}
    \item{Return}{Export the selected \code{Clustering}, then
      quit the function \code{choice}.}
    \item{'e'}{Switch between the different display of the trajectories
      and the criterion.}
    \item{'d'}{Change the quality criterion.}
    \item{'c'}{Order the \code{Clustering} according the the actual quality criterion.}
    \item{'r'}{Change the trajectories style.}
    \item{'t'}{Change the mean trajectories style.}
    \item{'g/t'}{Decrease / increase the size of the mean trajectories symbols.}
    \item{'h/y'}{Decrease / increase the number of symbol on the means trajectories.}
  }

  When 'return' is pressed, the selected \code{Clustering} are
  exported.
  Exporting is done using a specific name that is \code{objectName-CnumberOfCluster-OrderInTheSublist}.
  For each \code{Clustering}, four files/graphes are created:
  \itemize{
    \item{name-Clusters.csv}{Table with two columns. The first is the
      identifier of each individual; the second holds the cluster's
      affectation of the individual.}
    \item{name-Detail.csv}{Information about the
      clusterization (qualities criterion, the percentage of individual
      in each cluster, detail about the algorithm used,...)}
    \item{name-TrajMean.csv}{Coordonates of the mean trajectories of each
      clusters.}
    \item{name-Traj.ext}{Graph (of type 'ext') representing the
      trajectories. All the parameters set during the visualization (color
      of the trajectories, symbols used, mean color, ...) are used here.}
  }

  In addition, two graphs are created:
  \itemize{
    \item{objectName-criterionActif.ext}{Graph (of type 'ext') representing the
      actif quality criterion.}
    \item{objectName-criterionAll.ext}{Graph (of type 'ext') representing the
      all the quality criterion, standardized (see \code{\link{plot,ListClustering}}).}
  }
}

\value{No value are return; Several files and graphes are created in the
  curent folder, see 'details'.}


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
### Creation of articficial data
cld1 <- gald(c(15,15,15))

### Clusterisation
kml(cld1,nbRedrawing=3)

### Selection of the clusterization we want
#     (linux does not support getGraphicsEvent...
try(choice(cld1))
}

\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
