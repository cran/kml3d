source("../R/clusterLongData.r")

cat("\n####################################################################
######################## Test ClusterLongData ######################
####################################################################\n")

cleanProg(.ClusterLongData.validity,,,0)

### Constructeurs
cleanProg(clusterLongData)
cleanProg(.ClusterLongData.constructor)

new("ClusterLongData")
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))
#new("LongData",traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)),time=c(2,4,8),varName="T")

tr1 <- tr1n <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2),
            dimnames=list(c(101,102,104),c("T1","T2","T4","T8","T16"),c("P","A"))
            )
tr1n[1,2,1] <- NA; tr1n[2,4,2] <- NA; tr1n[3,1,2] <- NA; tr1n[3,3,2] <- NA;

new("ClusterLongData",
    traj=tr1,
    idAll=as.character(c(100,102)),
    idFewNA=as.character(c(101,102,104)),
    time=c(1,2,4,8,16),
    varNames=c("P","A"),
    maxNA=3
    )


tr2 <- array(c(1,2,3, 1,4,3, 6,1,8, 10,1,2,
              6,1,8, 10,1,2, 1,3,2, 4,2,5,
              1,3,2, 4,2,5, 6,3,4, 3,4,4,
              4,7,6, 5,5,4,  4,7,6, 5,5,4),
            dim=c(4,3,4),
            dimnames=list(c("I1","I2","I3","I4"),c("T1","T2","T4"),c("P","A","E","R"))
            )
new("ClusterLongData",
    traj=tr2,
    idFewNA=c("I1","I2","I3","I4"),
    idAll=c("I1","I2","I3","I4"),
    time=c(1,2,4),
    varNames=c("P","A","E","R"),
    maxNA=2
    )


tr3n <- array(c(1,NA,NA, 1,4,3,
              NA,1,8, 10,NA,2,
              4,NA,6, NA,5,4),
            dim=c(3,3,2),
            dimnames=list(c("I1","I2","I3"),c("T1","T2","T4"),c("P","A"))
            )
new("ClusterLongData",
    traj=tr3n,
    idAll=c("I1","I2","I3"),
    idFewNA=c("I1","I2","I3"),
    time=c(1,2,4),
    varNames=c("P","A"),
    maxNA=2
    )


tr4n <- array(c(NA,NA,2,
               NA,NA,1,
               NA,NA,NA,
               NA,3,2,
               2,NA,1,
               1,2,1,

               3,NA,NA,
               4,NA,6,
               5,2,4,
               2,NA,NA,
               NA,4,2,
               1,NA,2),
            dim=c(3,6,2),
            dimnames=list(c("T1","T2","T4"),c(101,102,103,105,106,107),c("P","A"))
            )
tr4n <- aperm(tr4n,c(2,1,3))

new("ClusterLongData",
    traj=tr4n,
    idAll=as.character(c(101,102,103,105,106,107)),
    idFewNA=as.character(c(101,102,103,105,106,107)),
    time=c(1,2,4),
    varNames=c("P","A"),
    maxNA=c(1,1)
    )


cat("\n###################################################################
####################### Test ClusterLongData ######################
########################### Constructeur ##########################
###################################################################\n")

cleanProg(.ClusterLongData.constructor,,,1) #all

clusterLongData()
#longData(traj=array(c(1,2,3,1,4,6,1,8,10),dim=c(3,3)))

clusterLongData(traj=tr1,idAll=as.character(c(101,102,104)),time=c(1,2,4,8,16),varNames=c("P","A"),maxNA=3)
cld(traj=tr2,idAll=as.character(c(1,2,3,4)),time=c(1,2,4),varNames=c("P","A","E","R"),maxNA=2)
clusterLongData(traj=tr3n,idAll=as.character(c(1,2,3)),time=c(1,2,4),varNames=c("P","A"),maxNA=2)
cld(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=2)

### Vérification de l'exclusion des manquantes
clusterLongData(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=1)
clusterLongData(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=2)
clusterLongData(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=c(1,1))
clusterLongData(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=c(2,2))
clusterLongData(traj=tr4n,idAll=c(1,2,3,4,5,6)+100,time=c(1,2,4),varNames=c("P","A"),maxNA=c(2,1))


as.cld(ld1)
as.clusterLongData(data=tr1,idAll=as.character(c(101,102,104)),time=c(1,2,4,8,16),varNames=c("P","A"),maxNA=3)
as.clusterLongData(data=tr1)



### Base de données
cleanProg(as.clusterLongData,,,0)
cleanProg(.ClusterLongData.show,,,0)

cld0 <- clusterLongData()
cld1 <- clusterLongData(traj=tr1,idAll=c(101,102,104),time=c(1,2,4,8,16),varNames=c("Pa","Av"),maxNA=3)
cld1b <- as.cld(ld1)
identical(cld1,cld1b)
cld1n <- clusterLongData(traj=tr1n,idAll=c(101,102,104),time=c(1,2,4,8,16),varNames=c("Pa","Av"),maxNA=3)

data <- read.csv2("example.csv")
cld2 <- as.clusterLongData(data,timeDataFrame=list(A=c(2,4),P=c(5,7)),time=2:3,varNames=c("Av","Pe"))
cld2 <- as.clusterLongData(data,timeDataFrame=list(A=c(2,4),P=c(5,7)),time=2:3)
try(cld2 <- as.clusterLongData(data,timeDataFrame=list(c(2,4),c(5,7)),time=2:3))
cld2 <- as.clusterLongData(data,timeDataFrame=list(A=c(2,4),P=c(5,7)),time=2:3,varNames=c("Av","Pe"))
cld2n <- as.clusterLongData(data,timeDataFrame=list(A=c(2,NA,4),P=c(NA,5,7)),time=2:4)
cld2 <- as.clusterLongData(data,timeDataFrame=list(V21=c(2,3,4),V4=c(5,6,7)),time=c(11,13,14))


dn3 <- read.csv("DatasetKML.csv")[1:200,,]
cld3n <- as.clusterLongData(dn3,time=1:6,timeDataFrame=list(cred=3:8,creq=9:14,croq=c(24:28,NA)))
cld3 <- cld3n
plot(cld3)
imputation(cld3)
plot(cld3)
#for(i in 1:2400){dn3[floor(runif(1,1,244)),floor(runif(1,2,29))]<-NA}
#ld3n <- as.clusterLongData(dn3,timeCol=list(T=2:28),timeReal=list(0:26))

cleanProg(.ClusterLongData.get,,,1) # CLUSTER_NAMES
cld3['add'] <- c3a
plot(cld3)
plot(cld3,2)
cld3['add'] <- c3b
cld3['add'] <- c3c
cld3['add'] <- c3d
cld3['add'] <- c3e
cld3['add'] <- c3an
cld3['add'] <- c3bn
cld3['add'] <- c3cn
cld3['add'] <- c3dn
cld3['add'] <- c3en



#as(cld3,"ListClustering")<-lcl3
#as(cld3n,"ListClustering")<-lcl3

tr4 <- array(rnorm(30*5*12),dim=c(30,5,12))
cld4 <- as.clusterLongData(data=tr4)

cld7n <- as.cld(ld7n)
cld7n['add'] <- c7an
cld7n['add'] <- c7bn
cld7n['add'] <- c7cn
cld7n['add'] <- c7dn
cld7n['add'] <- c7en
cld7n['add'] <- c7fn



cleanProg(.plot3d.clusterLongData.num)
cleanProg(.plot3d.clusterLongData.missingY)
cleanProg(.plot.clusterLongData.any)
cleanProg(.plot.clusterLongData.missingY)
cleanProg(.plotAll)

plot(cld3,2)
plot(cld3,c(2,2))
plot(cld3,c(2,1))
plot(cld3,c(3,1))
plot(cld3,c(3,2))
plot(cld3,"c4")
plot(cld3,c("c4",1))




cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++ Fin Test ClusterLongData +++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
