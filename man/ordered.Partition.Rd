\name{ordered(Partition)}
\Rdversion{1.1}
\alias{ordered,Partition}
\alias{ordered,Partition-method}
%\alias{partition,missing,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: ordered for Partition ~ }
\description{
Given a \code{Partition}, \code{ordered} order its clusters letters
according to the effectif of each clusters. It is meanly a "label
changing". (The function \code{ordered} for \code{ListClustering}
is describe \link[=ordered,ListClustering-method]{here}).
}

\usage{
\S4method{ordered}{Partition}(x)%,keepEmpty=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[Partition]}: Partition to be ordered.}
%  \item{keepEmpty}{\code{[logical]}: If
%    \code{TRUE}, some clusters may be empty ; is \code{FALSE}, empty
%    clusters are removed.}
%  \item{\dots}{Not used, for S4 compatibility only.}
}
\details{
Given a \code{Partition}, \code{ordered} order its clusters letters
according to the effectif of each clusters. So A will be the cluster
with be biggest frequency, then B, then C...
}
\value{
  An object of class \code{\linkS4class{Partition}}.
}

\seealso{\linkS4class{Partition}}


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
### Building a partition
smallPart <- partition(clusters=c("A","D","A","C","C","C"),nbClusters=4)

### Same partition but ordered, without the empty cluster
ordered(smallPart)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{cluster}  % Clustering
\keyword{methods}
