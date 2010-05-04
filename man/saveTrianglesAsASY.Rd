\name{saveTrianglesAsASY}
\alias{saveTrianglesAsASY}

\title{
  ~ Function: saveTrianglesAsASY ~
}
\description{
  Export a \code{Triangle} object to an '.azy' file.
}
\usage{
saveTrianglesAsASY(scene, filename = "scene.asy")
}

\arguments{
  \item{scene}{\code{[Triangle]}: Object representing the graph to plot.}
  \item{filename}{\code{[character]}: Name of exported file.}
}
\details{
  Export a \code{Triangle} object to an '.asy' file.
}
\value{
  An '.asy' file, in the current directory.
}


\references{\url{http://www.stat.uiowa.edu/~luke/R/misc3d/misc3d-pdf/}}

\section{Author(s)}{
  Luke Tierney \cr
  Chair, Statistics and Actuarial Science \cr
  Ralph E. Wareham Professor of Mathematical Sciences\cr
  University of Iowa \cr
}

\examples{
  ### Generating the data
  myCld <- gald(c(5,5,5))
  kml(myCld,3,1)

  ### Creation of the scene
  scene <- plot3dPdf(myCld)
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

