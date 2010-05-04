\name{LongData-class}
\docType{class}
\alias{LongData}
\alias{LongData-class}
\alias{[,LongData-method}
%\alias{[<-,LongData-method}
\alias{show,LongData-method}
%\alias{addClusterization,LongData-method}
%\alias{addPartition,LongData-method}
%\alias{choice,LongData-method}
%\alias{getCalinskiCriterion,LongData-method}
%\alias{getClusterizList,LongData,missing,missing-method}
%\alias{getClusterizList,LongData,numeric,missing-method}
%\alias{getClusterizList,LongData,numeric,numeric-method}
%\alias{KmL,LongData-method}
%\alias{plot,LongData,ANY-method}
%\alias{print,LongData-method}

\title{~ Class: LongData ~}
\description{\code{LongData} is an objet containing the longitudinal
  data (the individual trajectories) and some associate value (like time, individual
  identifiant,...)
}

%\section{Objects from the Class}{
%  Object \code{LongData} can be created either directly by calling
%  the fonction \code{\link{longData}} or form an existing structure
%  (\code{data.frame} or \code{matrix}) by using
%  \code{\link{as.longData}}.
%}

\section{Slots}{
  \describe{
    \item{\code{idAll}}{\code{[vector(character)]}: Single identifier
      for each of the longData (each individual). Usefull for exporting clusters.}
    \item{\code{idFewNA}}{\code{[vector(character)]}: Restriction of
      \code{idAll} to the trajectories that does not have 'too many' missing
      value. See \code{maxNA} for details.}
    \item{\code{time}}{\code{[numeric]}: Time at which measures are made.}
    \item{\code{varNames}}{\code{[vector(character)]}: Names of the variable measured.}
    \item{\code{traj}}{\code{[array(numeric)]}: Contains
      the longitudianl data. Each horizontal plan (first dimension) corresponds to the trajectories of an
      individual. Vertical plans (second dimension) refer to the time at which measures
      are made. Transversal plans (the third dimension) are for variables.}
    \item{\code{dimTraj}}{\code{[vector3(numeric)]}: size of the array
      \code{traj} (ie \code{c(length(idFewNA),length(time),length(varNames))}).}
    \item{\code{maxNA}}{\code{[numeric]} or \code{[vector(numeric)]}:
      Individual whose trajectories contain more missing value than
      \code{maxNA} are exclude from \code{traj} and will no be use in
      the analysis. Their identifier is preserved in \code{idAll} but
      not in \code{idFewNA}. When \code{maxNA} is a single number, it is
      used for all the variables.}
    \item{\code{reverse}}{\code{[matrix(numeric)]}: contain the
      mean (first line) and the standard deviation (second line) used to
      normalize the data. Usefull to restaure the original data after a
      scaling operation.}
  }
}

\section{Construction}{
  Object \code{LongData} can be created by calling
  the fonction \code{\link{longData}} (build from
  scratch). \code{\link{as.longData}} can also
  turn a \code{data.frame} or a \code{array} into a \code{LongData}.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field \code{xxx}.}
%    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.}
    \item{\code{show}}{Display a short version of the object.}
    \item{\code{print}}{Display the full version of the object.}
    \item{\code{\link[=plot]{plot}}}{Display the LongData, one
      graph for each variable.}
    \item{\code{\link[=plot3d]{plot3d}}}{Display two
      variables of the LongData in 3D.}
    \item{\code{\link{imputation}}}{Impute the
      trajectories.}
    \item{\code{\link{scale}}}{Scale the trajectories.
      Using the default value normalize each variable.}
%    \item{\code{\link{restaureRealData}}}{Reverse any scaling and
%      restaure the originale trajectories.}
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
### Building longData
traj <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2))

myLongData <- longData(
    traj=traj,
    idAll=as.character(c(100,102,103)),
    time=c(1,2,4,8,15),
    varNames=c("P","A"),
    maxNA=3
)

### Show
myLongData

### Get
myLongData['varNames']

### Plot
plot(myLongData)
}

\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
