\name{windowsCut}
\alias{windowsCut}

\title{
  ~ Function: windowsCut ~
}
\description{
\code{windowsCut} prepare an object \code{\linkS4class{ParWindows}}
  according to its arguments.
}
\usage{
windowsCut(x, addLegend = TRUE,closeScreen=TRUE)
}
\arguments{
  \item{x}{\code{[numeric]} or \code{[couple(numeric)]}: \code{x} is
    used to calculate the fields \code{nbCol} and \code{nbRow} of the
    object \code{ParWindows}. If \code{x} is a couple, then \code{x[1]} is \code{nbRow}
    and \code{x[2]} is \code{nbCol}. If \code{x} is a single number, it is the number of variables that
    should be plot. The \code{nbCol} and \code{nbRow} parameters are calculate
    consequently.
  }
  \item{addLegend}{\code{[logical]}: If \code{addLegend}is true, an
    extra space is reserved on the top of the screen to print the
    legend.
  }
  \item{closeScreen}{\code{[logical]}: Some function (like
    \code{\link{choice}}) need to add details on a graph. This option let
    them call a plot function that will not call a \code{close.screen}
    on exit, so the graph will be modifiable.
  }
}

\details{
  If \code{x} is a number of variable, the column and row number are
  estimate according to the formula
  \code{nbCol <- ceiling(sqrt(x))} and \code{nbRow <- ceiling(x/nbCol)}.
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

\value{
  An object of class \code{\linkS4class{ParWindows}}.
}

\examples{
  ### Simple cut
  windowsCut(5)

  ### With no space for legend
  windowsCut(5,FALSE)
}

