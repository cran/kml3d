\name{imputation}
\alias{imputation}
\alias{imputation,array-method}
\alias{imputation,LongData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: imputation ~ }
\description{
\code{imputation} is a function that offer different methods to impute
missing value of a \code{\linkS4class{LongData}}.
}
\usage{
imputation(object, method, imputRange,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{[LongData]} or \code{[array]} : object containing
    the trajectories to impute.}
  \item{method}{\code{[character]}: Name of the imputation method (see
    detail). The default value is \code{LI-Bissectrice}.}
  \item{imputRange}{\code{[matrix]}: Specify the imputing range of each
    variable. The first line gives the minimum, the second gives the
    maximum. When an imputing range is specified, any value imputed out of the
    range will be bring back to a possible value (its minimum or its
    maximum). When the imputing range is unspecified, the current range of the
    variable is considered. See example for details.}
  \item{...}{For compatibility only.}
}

\details{
  \code{imputation} is a function that impute missing value of trajectories.
  Several imputation methods are available. For each method, the
  imputation has to deal with three kind of missing value :
  at start of the trajectorie (first
  values are missing), at the end (last values are
  missing) or in the middle (the missing value have surround by
  non-missing value). Here is a description of each methods (for all of
  them, an example is provided in the \code{Examples} section):

  \itemize{
    \item{'LOCF' (Last Occurence Carried Forward)}{
      \itemize{
	\item{Missing in the middle / at the end:}{ the previous non-missing value is
	  dipplicated forward.}
	\item{Missing at start:}{ the first non-missing occurence is
	  dupplicated backward (like LOCB).}
      }
    }
    \item{'LOCB' (Last Occurence Carried Backward)}{
      \itemize{
	\item{Missing at start / in the middle:}{ the next non-missing value is
	  dipplicated backward.}
	\item{Missing at the end:}{ the previous non-missing
	  occurence is dupplicated forward (LOCF).}
      }
    }
    \item{'LI-Global' (linear interpolation, global slope)}{
      \itemize{
	\item{Missing in the middle:}{ the value imediatly
	  surounding the missing are join by a line.}
	\item{Missing at start / at the end:}{ the line joining the first and last non-missing value
	  is considered (this line is the everage progression of the actual
	  individual trajectoire). Missing-value at start and at the end are chose on
	  this line.}
      }
    }
    \item{'LI-Local' (linear interpolation, local slope)}{
      \itemize{
	\item{Missing in the middle:}{ like
	  \code{LI-Global}, the value imediatly
	  surounding the missing are join by a line;}
	\item{Missing at start:}{ they are
	  chosen to prolongate the line going through the first non
	  missing and second non missing value.}
	\item{Missing at the end:}{ They are chosen to prolongate the line going through the last
	  missing value and the penultimate value.}
      }
    }
    \item{'LI-Bissectrice', (linear interpolation, mixed between global and local)}{
      \itemize{
	\item{Missing in the middle:}{ like
	  \code{LI-Global}, the value imediatly
	  surounding the missing are join by a line.}
	\item{Missing at start / at end:}{ since LI-Global is not
	  sensitive to local variation and LI-Local is too much sensitive to
	  abnormal value, LI-Mixed offer a medium solution by considering the
	  bissectrice of Global and Local solution. Point are chosen on
	  the bissectrices.}
      }
    }
    \item{'LI-LOCBF', (linear interpolation, LOCB and LOCF)}{
      \itemize{
	\item{Missing in the middle:}{ like
	  \code{LI-Global}, values imediatly
	  surounding the missing are join by a line.}
	\item{Missing at start:}{ imputed by LOCB.}
	\item{Missing at end:}{ imputed by LOCF.}
      }
    }
  }
}
\value{
  If \code{object} is an \code{array}, an imputed \code{array} (with no missing
  values) in returned.

  If \code{object} is a \code{LongData}, \code{imputation} directly
  modify the internal value of the \code{LongData}. In this
  case, no value is return.

}

\seealso{
  \code{\linkS4class{LongData}}
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
### Comparison of the method on a single trajectories

arrayMissing <- array(c(NA,NA,1,NA,6,NA,NA,-1,0,NA,NA),dim=c(1,13,1))
par(ask=TRUE)
plot(arrayMissing,col="red",lwd=3,type="b",ylim=c(-4,8),
   xlab="Example of trajectory with missing values")


plot(imputation(arrayMissing,"LOCF"),col="black",ylim=c(-4,8),xlab="LOCF",lwd=1,type="b")
lines(arrayMissing,col="red",lwd=3,type="b")

plot(imputation(arrayMissing,"LOCB"),col="black",ylim=c(-4,8),xlab="LOCB",lwd=1,type="b")
lines(arrayMissing,col="red",lwd=3,type="b")

plot(imputation(arrayMissing,"LI-Global"),col="black",ylim=c(-4,8),xlab="LI-Global",lwd=1,type="b")
lines(arrayMissing,col="red",lwd=3,type="b")

plot(imputation(arrayMissing,"LI-Local"),col="black",ylim=c(-4,8),xlab="LI-Local",lwd=1,type="b")
lines(arrayMissing,col="red",lwd=3,type="b")

plot(imputation(arrayMissing,"LI-Bissectrice"),col="black",ylim=c(-4,8),xlab="LI-Bissectrice",lwd=1,type="b")
lines(arrayMissing,col="red",lwd=3,type="b")

plot(imputation(arrayMissing,"LI-LOCBF"),col="black",ylim=c(-4,8),xlab="LI-LOCBF",lwd=1,type="b")
lines(arrayMissing,col="red",lwd=3,type="b")

##################
### Real example on array

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
tra2 <- array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(120*8*2,0,3),dim=c(120,8,2))
for(i in 1:640){tra2[floor(runif(1,1,120)),floor(runif(1,1,9)),floor(runif(1,1,3))]<-NA}

(tra2)
(tra2imp <- imputation(tra2))

##################
### Real example on LongData

ld2 <- as.longData(tra2)
plot3d(ld2)
imputation(ld2)
plot3d(ld2)

###### EXAMPLE WITH RANGE ######
par(ask=FALSE)
}

\keyword{###### EXAMPLE WITH RANGE ######}
\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{NA}
\keyword{methods}
