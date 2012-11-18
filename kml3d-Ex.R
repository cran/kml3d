pkgname <- "kml3d"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('kml3d')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ClusterLongData3d-class")
### * ClusterLongData3d-class

flush(stderr()); flush(stdout())

### Name: ClusterLongData3d-class
### Title: ~ Class: ClusterLongData3d ~
### Aliases: ClusterLongData3d-class [,ClusterLongData3d-method
###   [<-,ClusterLongData3d-method show,ClusterLongData3d-method
### Keywords: classes

### ** Examples

### Building longData
traj <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2))

myCld <- clusterLongData3d(
    traj=traj,
    idAll=as.character(c(100,102,103)),
    time=c(1,2,4,8,15),
    varNames=c("P","A"),
    maxNA=3
)

### Show
myCld

### Get
myCld['varNames']

### Set
myCld['criterionActif']<-"Davies.Bouldin"

### Plot
plot(myCld)




cleanEx()
nameEx("KmL3D-package")
### * KmL3D-package

flush(stderr()); flush(stdout())

### Name: kml3d-package
### Title: ~ Overview: KmL3D, K-means for joint Longitudinal data ~
### Aliases: kml3d-package [,ParChoice-method [<-,ParChoice-method
### Keywords: package dplot iplot chron spatial classif cluster
###   nonparametric ts robust models

### ** Examples

### 1. Data Preparation
data(pregnandiol)
names(pregnandiol)
cld3dPregTemp <- cld3d(pregnandiol,timeInData=list(preg=1:30*2+1,temp=1:30*2))

### 2. Building "optimal" clusteration (with only 2 redrawings)
###    Real analysis needs at least 20 redrawings
kml3d(cld3dPregTemp,nbRedrawing=2,toPlot="both")

### 3. Exporting results
try(choice(cld3dPregTemp))

### 4. Visualizing in 3D
plot3d(cld3dPregTemp,4,parTraj=parTRAJ(type="n"))



cleanEx()
nameEx("affectIndiv3d")
### * affectIndiv3d

flush(stderr()); flush(stdout())

### Name: affectIndiv3d
### Title: ~ Function: affectIndiv3d ~
### Aliases: affectIndiv3d

### ** Examples

#######################
### affectIndiv

### Some trajectories
traj <- gald3d()["traj"]

### 4 clusters centers
center <- traj[runif(4,1,nrow(traj)),,]

### Affectation of each individual
part <- affectIndiv3d(traj,center)


#################
### K-means simulation (4 steps)
plot(clusterLongData3d(traj),partition(part))
for (i in 1:4){
    center <- calculTrajMean3d(traj,part)
    part <- affectIndiv3d(traj,center)
    plot(clusterLongData3d(traj),partition(part))
}



cleanEx()
nameEx("calculTrajMean3d")
### * calculTrajMean3d

flush(stderr()); flush(stdout())

### Name: calculTrajMean3d
### Title: ~ Function: calculTrajMean3d ~
### Aliases: calculTrajMean3d

### ** Examples

#######################
### calculTrajMean3d

### Some LongitudinalData3d
traj <- gald3d()["traj"]

### A partition
part <- floor(runif(150,1,5))
plot(clusterLongData3d(traj),partition(part))

### Clusters center
(center <- calculTrajMean3d(traj,part))


#################
### K-means simulation (4 steps)
plot(clusterLongData3d(traj),partition(part))
for (i in 1:4){
    part <- affectIndiv3d(traj,center)
    center <- calculTrajMean3d(traj,part)
    plot(clusterLongData3d(traj),partition(part))
}



cleanEx()
nameEx("choice")
### * choice

flush(stderr()); flush(stdout())

### Name: choice
### Title: ~ Function: choice ~
### Aliases: choice choice-methods choice,ClusterLongData3d-method
### Keywords: iplot chron spatial classif cluster nonparametric ts

### ** Examples

### Creation of articficial data
cld1 <- gald3d(20)

### Clusterisation (real analysis needs at least 20 redrawings, not 2)
kml3d(cld1,nbRedrawing=2,toPlot='both')

### Selection of the clustering we want
#     (note that "try" is for compatibility with CRAN only,
#     you probably can use "choice(cld1)")
try(choice(cld1))



cleanEx()
nameEx("clusterLongData3d")
### * clusterLongData3d

flush(stderr()); flush(stdout())

### Name: clusterLongData3d
### Title: ~ Function: clusterLongData3d (or cld3d) ~
### Aliases: cld3d clusterLongData3d
###   clusterLongData3d,ANY,ANY,ANY,ANY,ANY,ANY
###   clusterLongData3d,ANY,ANY,ANY,ANY,ANY,ANY-method
###   clusterLongData3d,missing,missing,missing,missing,missing,missing
###   clusterLongData3d,missing,missing,missing,missing,missing,missing-method

### ** Examples

###############
### Building an array
tr1n <- array(c(1,2,NA, 1,4,NA, 6,1,8, 10,NA,2, 3,NA,NA,
                4,NA,5,  6,3,4, 3,4,4, 4,NA,NA, 5,5,4),
            dim=c(3,5,2))


###############
### clusterLongData

### With maxNA=3
clusterLongData3d(traj=tr1n,
    idAll=as.character(c(100,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=3
)

### With maxNA=2
### Individual 104 is exclude
clusterLongData3d(traj=tr1n,
    idAll=as.character(c(100,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=2
)




cleanEx()
nameEx("dist3d")
### * dist3d

flush(stderr()); flush(stdout())

### Name: dist3d
### Title: ~ Function: dist3d ~
### Aliases: dist3d

### ** Examples

  ### Generate artificial data
  myCld <- gald3d()

  ### Distance between individual 1 and 3 (there are in the same group)
  dist3d(myCld['traj'][1,,],myCld['traj'][3,,])

  ### Distance between individual 1 and 51 (there are in two different groups)
  dist3d(myCld['traj'][1,,],myCld['traj'][51,,])



cleanEx()
nameEx("generateArtificialLongData3d")
### * generateArtificialLongData3d

flush(stderr()); flush(stdout())

### Name: generateArtificialLongData3d
### Title: ~ Function: generateArtificialLongData3d (or gald3d) ~
### Aliases: gald3d generateArtificialLongData3d
### Keywords: datagen cluster ts

### ** Examples

#####################
### Default example

ex1 <- generateArtificialLongData3d()
plot3d(ex1,parTraj=parTRAJ(type="l"))
part1 <- partition(rep(1:3,each=50))
plot3d(ex1,part1,parTraj=parTRAJ(type="l"))

#####################
### 4 lines with unbalanced groups

ex2 <- generateArtificialLongData3d(
  nbEachClusters=c(5,10,20,40),
  meanTrajectories=list(
     function(t)c(t,t^3/100),
     function(t)c(0,t),
     function(t)c(t,t),
     function(t)c(0,t^3/100)
  ),
  residualVariation = function(t){c(rnorm(1,0,1),rnorm(1,0,1))}
)
part2 <- partition(rep(1:4,time=c(5,10,20,40)))
plot3d(ex2,part2)



cleanEx()
nameEx("kml3d")
### * kml3d

flush(stderr()); flush(stdout())

### Name: kml3d
### Title: ~ Algorithm kml3d: K-means for Joint Longitidinal data ~
### Aliases: kml3d kml3d-method kml3d,ClusterLongData-method
### Keywords: dplot chron spatial classif cluster nonparametric ts robust

### ** Examples

### Generation of some data
cld1 <- generateArtificialLongData3d(15)

### We suspect 2, 3, 4 or 5 clusters, we want 3 redrawing.
###   We want to "see" what happen (so toPlot="both")
kml3d(cld1,2:5,3,toPlot="both")

### 3 seems to be the best.
###   We don't want to see again, we want to get the result as fast as possible.
###   Just, to check the overall process, we plot the criterion evolution
kml3d(cld1,3,10,toPlot="criterion")



cleanEx()
nameEx("parKml3d")
### * parKml3d

flush(stderr()); flush(stdout())

### Name: parKml3d
### Title: ~ Function: parKml3d ~
### Aliases: parKml3d

### ** Examples

### Generation of some data
cld1 <- generateArtificialLongData3d(c(15,15,15))

### Setting two different set of option :
(option1 <- parKml3d())
(option2 <- parKml3d(centerMethod=function(x)median(x,na.rm=TRUE)))

### Running kml. Formaly, the second exemple is 'k-median'
kml3d(cld1,4,1,toPlot="both",parAlgo=option1)
kml3d(cld1,4,1,toPlot="both",parAlgo=option2)



cleanEx()
nameEx("plot")
### * plot

flush(stderr()); flush(stdout())

### Name: plot,ClusterLongData3d
### Title: ~ Function: plot for ClusterLongData3d ~
### Aliases: plot plot,ClusterLongData3d plot,ClusterLongData3d,method
###   plot,ClusterLongData3d,missing-method
###   plot,ClusterLongData3d,numeric-method
###   plot,ClusterLongData3d,Partition-method
### Keywords: dplot iplot chron spatial classif cluster ts

### ** Examples

##################
### Construction of the data

myCld <- gald3d()
part <- partition(rep(1:3,each=50))

### Basic plotting
plot(myCld)
plot(myCld,part)


##################
### Changing graphical parameters 'par'

### No letters on the mean trajectories
plot(myCld,part,parMean=parMEAN(type="l"))

### Only one letter on the mean trajectories
plot(myCld,part,parMean=parMEAN(pchPeriod=Inf))

### Color individual according to its clusters (col="clusters")
plot(myCld,part,parTraj=parTRAJ(col="clusters"))

### Mean without individual
plot(myCld,part,parTraj=parTRAJ(type="n"))


### No mean trajectories (type="n")
### Color individual according to its clusters (col="clusters")
plot(myCld,part,parTraj=parTRAJ(col="clusters"),parMean=parMEAN(type="n"))

### Only few trajectories
plot(myCld,part,nbSample=10,parTraj=parTRAJ(col='clusters'),parMean=parMEAN(type="n"))



cleanEx()
nameEx("plot3d")
### * plot3d

flush(stderr()); flush(stdout())

### Name: plot3d,ClusterLongData3d
### Title: ~ Function: plot3d for ClusterLongData3d ~
### Aliases: plot3d plot3d,ClusterLongData3d-method
###   plot3d,ClusterLongData3d,missing-method
###   plot3d,ClusterLongData3d,numeric-method
###   plot3d,ClusterLongData3d,Partition-method
### Keywords: package ts aplot

### ** Examples

##################
### Real example on array

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)%%3-1) * t
g <- function(id,t)(id%%2+1)*t
h <- function(id,t)(id%%4-0.5)*(20-t)
myCld <- clusterLongData3d(array(cbind(outer(id2,time,f),outer(id2,time,g),outer(id2,time,h))+rnorm(120*8*3,0,3),dim=c(120,8,3)))
part <- partition(rep(1:6,20))

### Basic plot
plot(myCld,part)

### plot3d, variable 1 and 2
plot3d(myCld,part)

### plot3d, variable 1 and 3
plot3d(myCld,part,varZ=3)
plot3d(myCld,parTraj=parTRAJ(col="red"))



cleanEx()
nameEx("plot3dPdf")
### * plot3dPdf

flush(stderr()); flush(stdout())

### Name: plot3dPdf
### Title: ~ Function: plot3dPdf for ClusterLongData3d ~
### Aliases: plot3dPdf plot3dPdf,ClusterLongData3d-method
###   plot3dPdf,ClusterLongData3d,missing-method
###   plot3dPdf,ClusterLongData3d,numeric-method

### ** Examples

  ### Generating the data
  myCld3d <- gald3d(c(5,5,5))
  kml3d(myCld3d,3:4,1)

  ### Creation of the scene
  scene <- plot3dPdf(myCld3d)
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



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
