\name{ParLongData-class}
\docType{class}
\alias{ParLongData}
\alias{ParLongData-class}
\alias{[,ParLongData-method}
\alias{[<-,ParLongData-method}
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
%\alias{show,LongData-method}

\title{~ Class: ParLongData ~}
\description{\code{ParLongData} is an objet containing some graphical
  parameter used to plot \code{\linkS4class{LongData}} object and / or mean trajectories. They work as
  define in \code{par}.
}

\section{Slots}{
  \describe{
    \item{\code{type}}{\code{[character]}: Type of the plot should be
      drawn ('p' for point, 'l' for line, 'b' for both, 'c' line appart, 'o'
      for overplot, 'h' for histogram, 's' and 'S' for steps, 'n' for no
      ploting)}
    \item{\code{col}}{\code{[character]}: A specification for the
      default plotting color. Vector of values are accepted.}
    \item{\code{pch}}{\code{[numeric]} or \code{[character]}: Either an integer
          specifying a symbol or a single character to be used as the default in plotting
          points.  See 'points' for possible values and their
	  interpretation.}
    \item{\code{pchPeriod}}{\code{[numeric]}: (for mean trajectories
      only) Fix the number of point that should be plot. Usefull to
      plot points on trajectories with a lot of mesurement (see examples
      in \code{\link[=plot,LongData-method]{plot}} for \code{LongData} for details).}
    \item{\code{cex}}{\code{[numeric]}: A numerical value giving the
      amount by which plotting text and symbols should be magnified relative
      to the default.}
    \item{\code{xlab}}{\code{[character]}: A title for the x axis.}
    \item{\code{ylab}}{\code{[character]}: A title for the y axis.}
  }
}

\section{Construction}{
  Object \code{ParLongData} can be created by three functions:
  \enumerate{
    \item \code{parLongData} create an object from scratch ;
    \item \code{parTraj} create an object whose default value are the
    one used when ploting some longitudinal data ;
    \item \code{parMean} create an object whose default value are the
    one used when ploting some mean trajectories.
  }
}

\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field \code{xxx}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.}
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
   ### Building ParLongData
   parMyData <- parLongData(type="n",col=3,pch="1",pchPeriod=20,cex=1,xlab="Time",ylab="Size")

   ### Get
   parMyData['col']

   ### Set
   parMyData['cex'] <- 3
   print(parMyData)
}
\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
