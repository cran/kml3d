\name{as.clusterLongData}
\alias{as.cld}
\alias{as.cld,data.frame}
\alias{as.cld,data.frame-method}
\alias{as.cld,array}
\alias{as.cld,array-method}
\alias{as.cld,LongData}
\alias{as.cld,LongData-method}
\alias{as.clusterLongData}
\alias{as.clusterLongData,data.frame}
\alias{as.clusterLongData,data.frame-method}
\alias{as.clusterLongData,array}
\alias{as.clusterLongData,array-method}
\alias{as.clusterLongData,LongData}
\alias{as.clusterLongData,LongData-method}

\title{~ Function: as.clusterizLongData (or as.cld) ~}

\description{
  \code{as.clusterizLongData} (or \code{as.cld}) turns a
  \code{data.frame}, an \code{array} or a \code{\linkS4class{LongData}} into
  an object of class \code{\linkS4class{ClusterLongData}}.
}

\usage{
\S4method{as.clusterLongData}{data.frame}(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski")
\S4method{as.cld}{data.frame}(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski")

\S4method{as.clusterLongData}{array}(data,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski")
\S4method{as.cld}{array}(data,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski")

\S4method{as.clusterLongData}{LongData}(data,criterionActif="calinski")
\S4method{as.cld}{LongData}(data,criterionActif="calinski")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{[data.frame]}, \code{[array]} or
    \code{[\linkS4class{LongData}]}:
    structure containning the longitudinal data. If \code{data} is a
    \code{matrix} or a \code{data.frame}, it should coutain the
    trajectories.
  }

  \item{idAll}{\code{[vector(character)]}: single identifier for each trajectory (ie each individual).
    For a \code{data.frame}, the default \code{idAll} is the first column ;
    For an \code{array}, the default \code{idAll} is the vector \code{1:dim(data)[1]}.
  }

  \item{varNames}{\code{[vector(numeric)]}: names of the variable.
    For a \code{data.frame}, the default value is the names of the list provide to \code{timeDataFrame}.
    For an \code{array}, the default value is \code{c(V1,V2,V3,...]}.
  }

  \item{time}{\code{[vector(numeric)]}: column number in which longitudinal data can be found.
    For a \code{data.frame}, the default value is the length of the first argument of \code{timeDataFrame}.
    For an \code{array}, the default value is \code{1:dim(data)[2]}.
  }

  \item{timeDataFrame}{\code{[list(character=numeric,...)]}: (for \code{data.frame} only)
    For each variable, \code{timeDataFrame} provide the column numbers that contain the trajectories.
    See example for details.
  }

  \item{maxNA}{\code{[numeric]} or \code{[vector(numeric)]}:
    Define, for each variable, the maximum number of missing values that a trajectories shall contain.
    Individual whose trajectory contains more missing value than
    \code{maxNA} are exclude from \code{traj} and will no be use in
    the analysis. When \code{maxNA} is a single number, it is
    diplicated for all the variables.
  }

  \item{criterionActif}{\code{[character]}: criterion name
    that will be used by fonctions that need a single criterion
    (like \link[=plot,ClusterLongData-method]{plot} or \link{ordered}).
  }
}

\details{
  \code{as.clusterizLongData} (or \code{as.cld}) turns a \code{data.frame} into
  an object of class \code{\linkS4class{ClusterLongData}} according to
  its arguments.
}

\value{
  An object of class \code{\linkS4class{ClusterLongData}}.
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
#################
### data.frame
myDataFrame <- data.frame(id=1:10,V1=rnorm(10),C1=rpois(10,1),V2=rnorm(10,1),C1=rpois(10,2),V3=rnorm(10,2),C2=rpois(10,2))
as.cld(myDataFrame,timeDataFrame=list(V1=c(2,4,6),v2=c(3,5,7)))

### Some time can be missing (like V12 in the following):
myDataFrame2 <- data.frame(id=11:12,Va11=c(1,4),Pe11=c(2,5),Pe12=c(3,5),Va13=c(2,3),Pe13=c(1,3))
as.cld(myDataFrame2,time=c(11,12,13),timeDataFrame=list(Va=c(2,NA,5),Pe=c(3,4,6)))


#################
### array
myArray <- array(rnorm(40),dim=c(5,4,2))
as.cld(myArray)
as.cld(myArray,idAll=c("I142","I156","I173","I181a","I181b"),time=c(6,12,24,48),varNames=c("Var1","Var2"),maxNA=0,criterionActif="ray")


#################
### LongData
myLongData <- as.longData(myArray)
as.cld(myLongData)
}
