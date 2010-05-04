\name{makeLatexFile}
\alias{makeLatexFile}

\title{
  ~ Function: makeLatexFile ~
}
\description{
Create a LaTeX document that inclusde 3D objects into PDF documents.
}
\usage{
makeLatexFile(filename = "main.tex", asyToInclude = "scene+0.prc")
}

\arguments{
  \item{filename}{Name of the LaTeX file}
  \item{asyToInclude}{Name of the file holding the 3D graph to include.}
}

\details{
Create a LaTeX document that inclusde 3D objects into PDF documents with PDF-1.5/1.6 compatibility.
}
\value{
A LaTeX file, in the current directory.
}

\seealso{
\link[misc3d:makeTriangles]{makeTriangles}
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
  myCld <- gald(c(5,5,5))
  kml(myCld,3:4,1)

  ### Creation of the scene
  scene <- plot3dPdf(myCld)
  drawScene.rgl(scene)

  ### Export in '.asy' file
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


