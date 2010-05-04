\name{parWindows}
\alias{parWindows}

\title{
  ~ Function: parWindows ~
}
\description{
  \code{parWindows} is the constructor of object \code{\linkS4class{ParWindows}}.
}
\usage{
parWindows(nbCol, nbRow, addLegend,closeScreen)
}
\arguments{
  \item{nbCol}{\code{[numeric]}: Number of column of the screen matrix.}
  \item{nbRow}{\code{[numeric]}: Number of row of the screen matrix.}
  \item{addLegend}{\code{[logical]}: Shall a legend be added on the graph?}
  \item{closeScreen}{\code{[logical]}: Some function (like
    \code{\link{choice}}) need to add details on a graph. This option let
    them call a plot function that will not call a \code{close.screen}
    on exit, so the graph will be modifiable.}
}

\details{
  \code{parWindows} is the constructor of object \code{\linkS4class{ParWindows}}.
   Given a number of rows and colonnes, it computes the \code{screenMatrix} that
   is use by \link[graphics]{split.screen} for plot object
  \code{\linkS4class{ClusterLongData}}. If \code{addLegend} is true, an
  extra space is added on the top of the graphes to print the legend.
}
\value{
An object of class \code{\linkS4class{ParWindows}}.
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
### Building ParWindows
(paramWin <- parWindows(3,2,FALSE,TRUE))

### Get
figsScreen <- paramWin['screenMatrix']

### Usage
listScreen <- split.screen(figsScreen)
screen(listScreen[1])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[3])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[5])
plot(-5:5/10,(-5:5)^2/10,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20+1.25,ylim=c(0,6),type="l",lwd=3)
close.screen(all.screens=TRUE)

### :-)
}

