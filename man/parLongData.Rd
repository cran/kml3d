\name{parLongData}
\alias{parLongData}
\alias{parTraj}
\alias{parMean}


\title{
 ~ Functions: parLongData, parTraj and parMean~
}
\description{
\code{parLongData}, \code{parTraj} and \code{parMean} are constructors for the class
\code{\linkS4class{ParLongData}}.
}

\usage{
parLongData(type, col, pch, pchPeriod, cex, xlab, ylab)
parTraj(type = "l", col = "black", pch = "1", pchPeriod = 0, cex = 1, xlab = "Time", ylab = "")
parMean(type = "b", col = "clusters", pch = "letters", pchPeriod = 1, cex = 1.2, xlab = "Time", ylab = "")
}

\arguments{
  \item{type}{\code{[character]}: Set type of the plot should be
    drawn ('p' for point, 'l' for line, 'b' for both, 'c' line appart, 'o'
    for overplot, 'h' for histogram, 's' and 'S' for steps, 'n' for no
    ploting)}
  \item{col}{\code{[character]}: Set the plotting color. Vector of
    values are accepted. The special value 'clusters' can be use to color
    each trajectories according to its clusters.}
  \item{pch}{\code{[numeric]} or \code{[character]}: Either an integer
    specifying a symbol or a single character to be used as the default in plotting
    points.}
  \item{pchPeriod}{\code{[numeric]}: (for mean trajectories
    only) Fix the number of point that should be plot. Usefull to
    plot points on trajectories with a lot of mesurement (see examples
    in \code{\link[=plot,LongData-method]{plot}} for \code{LongData} for details).}
  \item{cex}{\code{[numeric]}: Set the
    amount by which plotting text and symbols should be magnified relative
    to the default.}
  \item{xlab}{\code{[character]}: Title for the x axis.}
  \item{ylab}{\code{[character]}: Title for the y axis.}
}
\details{
\code{parLongData} is the basic constructor of the class
\code{\linkS4class{ParLongData}}.

\code{parTraj} create an object whose default value are the
one used when ploting some longitudinal data ;

\code{parMean} create an object whose default value are the
one used when ploting some mean trajectories.
}

\value{
  An object of class \code{\linkS4class{ParLongData}}
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
##################
### Construction of LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
ld2 <- as.longData(array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(120*8*2,0,3),dim=c(120,8,2)))

### Example with default value
plot(ld2)
plot(ld2,paramTraj=parTraj())

### Example with default values for mean trajectories
plot(ld2,paramTraj=parMean())

### Example with default value except for the color
plot(ld2,paramTraj=parTraj(col="blue"))
}
\keyword{methods}

