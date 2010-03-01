\name{plot3dPdf,LongData}
\Rdversion{1.1}
\alias{plot3dPdf,LongData-method}
\alias{plot3dPdf,LongData,missing-method}
\alias{plot3dPdf,LongData,Partition-method}

\title{
  ~ Function: plot3dPdf for LongData ~
}
\description{
  \code{\linkS4class{LongData}} and a \code{\linkS4class{Partition}}, this
  function create Triangle objects representing the 3D plot of two
  variables of the main trajectories.
}
\usage{
\S4method{plot3dPdf}{LongData,missing}(x,y,varY=1,varZ=2)
\S4method{plot3dPdf}{LongData,Partition}(x,y,varY=1,varZ=2)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containing the trajectories to plot.}
  \item{y}{\code{[Partition]}: \code{\linkS4class{Partition}} defining
    clusters.}
  \item{varY}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the first variable to display. 1 by default.}
  \item{varZ}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the second variable to display. 2 by default.}
}
\details{
  Create Triangle objects representing the 3D plot of the main
  trajectories of a \code{\linkS4class{ClusterLongData}}.

  The three functions \code{\link{plot3dPdf}}, \code{\link{saveTrianglesAsASY}} and \code{\link{makeLatexFile}}
  are design to export a 3D graph to a Pdf file. The process is the following:

  \enumerate{
    \item \code{\link{plot3dPdf}}: Create a scene, that is a collection of Triangle object that
    represent a 3D images.
    \item \code{\link{saveTrianglesAsASY}}: Export the scene in an '.asy' file.
    \item '.azy' can not be include in LaTeX file. LaTeX can read only '.pre' file. So the next step is to use
    \code{asymptote} to convert '.asy' tp '.pre'. This is done by the command \code{asy -inlineimage -tex pdflatex
      scene.azy}.
    \item The previous step did produce a file \code{scene+0.prc} that can be include in a LaTeX file.
    \code{\link{makeLatexFile}} create a LaTeX file that is directly compilable (using \code{pdfLatex}).
    It produce a pdf file that contain the 3D object.
  }
}

\value{
  A Triangle object.
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
  ### Generating the data
  myLd <- gald(c(5,5,5),clusterLD=FALSE)
  part <- partition(rep(1:3,5))

  ### Creation of the scene
  scene <- plot3dPdf(myLd,part)
  drawScene.rgl(scene)

  ### Export in '.azy' file
  saveTrianglesAsASY(scene)

  ### Creation of a '.prc' file
  # Open a console window, then run
  # asy -inlineimage -tex pdflatex scene.azy

  ### Creation of the LaTeX main document
  makeLatexFile()

  ### Creation of the '.pdf'
  # Open a console window, then run
  # pdfLatex main.tex
}
