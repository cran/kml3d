\name{exportClustering}
\Rdversion{1.1}
\alias{exportClustering}
\alias{exportClustering,ClusterLongData,numeric}
\alias{exportClustering,ClusterLongData,numeric-method}

\title{
~ Function: exportClustering ~
}
\description{
This function save the information of a
\code{\linkS4class{Clustering}} object in 3 files and a graph.
}
\usage{
\S4method{exportClustering}{ClusterLongData,numeric}(object, y, nameObject,typeGraph = "bmp",
paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(object["nbVar"]), ...)
}

\arguments{
  \item{object}{\code{[ClusterizLongData]}: object containing the \code{LongData} and the \code{Clustering}
    that have to be saved
  }
  \item{y}{\code{[couple(numeric)]}: indicate which \code{Clustering}
    shall be exported. The first numeric \code{y[1]} is the cluster number, the
    second \code{y[2]} is the rank of clustering in the list
    \code{y[1]}.
  }
  \item{nameObject}{\code{[character]}: prefix use to name the files and
    the graph.
  }
  \item{typeGraph}{\code{[character]}: indicates the type of the graph that shall
    be exported. See \link[grDevices]{savePlot} for avalaible options.
  }
  \item{paramTraj}{\code{[parLongData]}: Set the graphical parameters
    used to plot the trajectories of the \code{LongData}. See \code{\linkS4class{ParLongData}}
    for details.
  }
  \item{paramMean}{\code{[parLongData]}: Set the graphical parameters
    used to plot the mean trajectories of each clusters \code{LongData}
    (only when \code{y} is non missing). See \code{\linkS4class{ParLongData}}
    for details.
  }
  \item{paramWindows}{\code{[ParWindows]}: Set the graphical display of
    the windows. See \code{\linkS4class{ParWindows}} for details.
  }
  \item{\dots}{Parameters for the function \code{\link{bmp}},
    \code{\link{jpeg}}, \code{\link{png}} or \code{\link{tiff}}.
  }
}

\details{
  \code{exportClustering} export information about a \code{Clustering}
  in files. Four files/graphes are create:
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
  \code{exportClustering} is mainly used by the function \code{\link{choice}}.
}

\value{
Three files and a graph.
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
#############
### Creating a ClusterizLongData object, with 3 cluterings (5 clusters each)
myCld <- gald(c(15,15,15))
kml(myCld,5,3)

### Exporting the second clustering in pdf format
exportClustering(myCld,y=c(5,2),nameObject="myCld-5-2",typeGraph="bmp")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
