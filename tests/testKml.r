source("../R/kml.r")
cleanProg(.partitionInitialise,,,1) # min
point <- matrix(c(0,0, 0,1, -1,0, 0,-1, 1,0),5,byrow=TRUE)
points <- rbind(point,t(t(point)+c(10,0)),t(t(point)+c(5,6)))
points <- rbind(points,t(t(points)+c(30,0)),t(t(points)+c(15,20)),t(-t(point)+c(20,10)))

paInit <- partitionInitialise(2,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(4,nrow(points),as.matrix(dist(points)),method="maxDist")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="pg")

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="randomK")
plot(points)
lines(points[!is.na(paInit["clusters"]),],col=2,type="p")

paInit <- partitionInitialise(3,nrow(points),as.matrix(dist(points)),method="randomAll")
plot(points,col=as.integer(paInit["clusters"]))



pa <- partitionInitialise(3,200,method="randomK")

plot(ld3,pa)

pa <- partitionInitialise(3,200,method="randomAll")
plot(ld3,pa)

pa <- partitionInitialise(3,200,method="maxDist",matDist3d(ld3["traj"]))
plot(ld3,pa)

pa <- partitionInitialise(3,30,method="randomK")
plot(ld4,pa)

pa <- partitionInitialise(3,30,method="randomAll")
plot(ld4,pa)

pa <- partitionInitialise(3,30,method="maxDist",matDist3d(ld4["traj"]))
plot(ld4,pa)


cleanProg(calculTrajMean,,,2) # meanNA tapply
cent2a <- calculTrajMean(ld2["traj"],p2a['clusters'])
cent2b <- calculTrajMean(ld2["traj"],p2b['clusters'])
cent2c <- calculTrajMean(ld2["traj"],p2c['clusters'],medianNA)

#cent3aC <- calculTrajMean(ld3["traj"],p3a['clusters'])
#cent3bC <- calculTrajMean(ld3["traj"],p3b['clusters'])
#cent3cC <- calculTrajMean(ld3["traj"],p3c['clusters'])
#cent3dC <- calculTrajMean(ld3["traj"],p3d['clusters'])
#cent3eC <- calculTrajMean(ld3["traj"],p3e['clusters'])
#cent3fC <- calculTrajMean(ld3["traj"],p3f['clusters'])

cleanProg(affectIndiv,,,1) # dist3d (dans les arguments)
aC <- affectIndiv(ld2["traj"],cent2a)
bC <- affectIndiv(ld2["traj"],cent2b)
cC <- affectIndiv(ld2["traj"],cent2c)


cleanProg(kmlSlow)
partInit <- partitionInitialise(3,193,method="maxDist",matDist3d(ld3["traj"]))
system.time(kmlSlow(ld3['traj'],partInit))
system.time(kmlSlow(ld3['traj'],partInit,toPlot="none"))

kml(cld3)
#cleanProg(kmlFast)
cleanProg(expandStartingCond)
expandStartingCond(startingCond="allMethods",3,"randomK")
expandStartingCond(startingCond="allMethods",3,"maxDist")
expandStartingCond(startingCond="randomK",3,"maxDist")
expandStartingCond(startingCond="randomK",3,"randomK")

cleanProg(cutScreen)
cleanProg(fastOrSlow,,,1) #DISTANCE_METHODS
cleanProg(.clusterLongData.kml)

cld3 <- as.clusterLongData(dn3,time=1:6,timeDataFrame=list(cred=3:8,creq=9:14,croq=c(24:28,NA)))
kml(cld3,toPlot="both")
choice(cld3)
cleanProg(choiceChangeParam,,,1) # choiceChangeParam


my <- gald(
           nbEachClusters=c(20,20,20,20,20),
           functionClusters=list(
           function(t){return(c(0,0))},
           function(t){return(c(10,10))},
           function(t){return(c(0,10))},
           function(t){return(c(10-t,10-t))},
           function(t){return(c(10,10-t))}
           )
           )
kml(my)

my2 <- gald(
           nbEachClusters=c(150,150,150,150,150),
           functionClusters=list(
           function(t){return(c(0,0))},
           function(t){return(c(10,10))},
           function(t){return(c(0,10))},
           function(t){return(c(10-t,10-t))},
           function(t){return(c(10,10-t))}
           )
           )
kml(my2)


mi <- gald(
           nbEachClusters=c(20,20,20,20,20),
           functionClusters=list(
           function(t){return(c(0,0))},
           function(t){return(c(10,10))},
           function(t){return(c(0,10))},
           function(t){return(c(t,t))},
           function(t){return(c(10,10-t))}
           )
           )
kml(mi)

mi2 <- gald(
           nbEachClusters=c(150,30,40,25,15),
           functionClusters=list(
           function(t){return(c(0,0))},
           function(t){return(c(10,10))},
           function(t){return(c(0,10))},
           function(t){return(c(t,t))},
           function(t){return(c(10,10-t))}
           )
           )
kml(mi2)


ma <- gald(time=5:15,
           nbEachClusters=c(30,30,30),
           functionClusters=list(
           function(t){return(c(0,dnorm(10-t)*30))},
           function(t){return(c(dnorm(10-t)*30,0))},
           function(t){return(c(dnorm(10-t)*30,dnorm(10-t)*30))}
           )
           )
pa <- partition(rep(1:3,each=30))
plot(ma,pa)
kml(ma)




