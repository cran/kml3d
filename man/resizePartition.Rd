\name{resizePartition}
\alias{resizePartition}

\title{
  ~ Function: resizePartition ~
}
\description{
  Give a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}} whose length fit with \code{idAll}, this function
  makes the \code{Partition} fit with \code{idFewNA}.
}
\usage{
resizePartition(xLongData, yPartition)
}

\arguments{
  \item{xLongData}{\code{[LongData]}: longitudinal data}
  \item{yPartition}{\code{[Partition]}: partition of the longitudinal
    data (partition that needs to be resized).}
}

\details{
  Give a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}} whose length fit with \code{idAll}, this function
  makes the \code{Partition} fit with \code{idFewNA} by keeping only the
  value  corresponding to individual present in \code{idFewNA}.
}
\value{
  Object \code{\linkS4class{Partition}}.
}
