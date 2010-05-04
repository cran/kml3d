### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################### Class ClustLongData ######################
############################## Creation ############################
####################################################################\n")

.ClusterLongData.validity <- function(object){
    validObject(as(object,"LongData"))
    validObject(as(object,"ListClustering"))
    return(TRUE)
}
cat("### Definition ###\n")
# id       : identifiant of the individual (or lines).
# time     : real time
# varNames : nom of the variable (single now, several in the futur)
# value    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)
setClass(
    Class="ClusterLongData",
    contains=c("LongData","ListClustering"),
    validity=.ClusterLongData.validity
)

setMethod("clusterLongData",signature=c("missing","missing","missing","missing","missing","missing"),
    function(traj,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski"){new("ClusterLongData")}
)

### Valable pour traj = matrix
.ClusterLongData.constructor <- function(traj,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski"){
    traj <- longData(traj=traj,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA)
    traj <- as(traj,"ClusterLongData")
    traj["criterionActif"] <- criterionActif
    return(traj)
}
setMethod("clusterLongData",signature=c("ANY","ANY","ANY","ANY","ANY","ANY"),.ClusterLongData.constructor)
cld <- clusterLongData

setGeneric("as.clusterLongData",function(data,...){standardGeneric("as.clusterLongData")})

setMethod("as.clusterLongData","LongData",
    function(data,criterionActif="calinski"){
        data <- as(data,"ClusterLongData")
        data["criterionActif"] <- criterionActif
        return(data)
    }
)

setMethod("as.clusterLongData","data.frame",
    function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski"){
        if(missing(idAll)){idAll <- data[,1]}else{}
        if(missing(varNames)){varNames <- names(timeDataFrame)}else{}
        if(missing(time)){time <- 1:length(timeDataFrame[[1]])}else{}
        matr <- as.matrix(data[,na.omit(unlist(timeDataFrame))])
        traj <- array(matr[,rank(unlist(timeDataFrame),na.last="keep")],c(length(idAll),length(time),length(varNames)))
        return(clusterLongData(traj=traj,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA,criterionActif=criterionActif))
    }
)

setMethod("as.clusterLongData","array",
    function(data,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski"){
        if(missing(idAll)){idAll <- 1:dim(data)[1]}else{}
        if(missing(varNames)){varNames <- paste("V",1:dim(data)[3],sep="")}else{}
        if(missing(time)){time <- 1:dim(data)[2]}else{}
        return(clusterLongData(traj=data,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA,criterionActif=criterionActif))
    }
)


as.cld <- as.clusterLongData

#as.cld <- as.clusterLongData <- function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski"){
#    if(class(data)!="LongData"){
#        data <- as.longData(data=data,idAll=idAll,time=time,timeDataFrame=timeDataFrame,varNames=varNames,maxNA=maxNA)
#    }else{}
#    cLongData <- as(data,"ClusterLongData")
#    cLongData["criterionActif"] <- criterionActif
#    return(cLongData)
#}

#as.cld <- as.clusterLongData <- function(data,...){
#    if(class(data)!="LongData"){
#        data <- as.longData(data=data,...)
#    }else{}
#    cLongData <- as(data,"ClusterLongData")
#    cLongData["criterionActif"] <- "calinski"
#    return(cLongData)
#}



#.ClusterLongData.show <- function(object){
#    show(as(object,"LongData"))
#    cat("/n")
#    show(as(object,"ListClustering"))
#}


.ClusterLongData.show <- function(object){
    cat("   ~~~ Class: ClusterLongData ~~~")
    cat("\n      ~ Sub-Class: LongData ~ ")
    .longData.show(as(object,"LongData"))
    cat("\n    ~ Sub-Class: ListClustering ~ ")
    .ListClustering.show(as(object,"ListClustering"))
}
setMethod("show","ClusterLongData",.ClusterLongData.show)


cat("### Getteur ###\n")
.ClusterLongData.get <- function(x,i,j,drop){
    if(i%in%c("criterionActif","criterionPossibles","initializationMethod","sorted","criterionValues","criterionValuesAsMatrix",CLUSTER_NAMES,2:26)){
        x <- as(x,"ListClustering")
    }else{
        x <- as(x,"LongData")
    }
    return(x[i,j])
}
setMethod("[","ClusterLongData",.ClusterLongData.get)




cat("### Setteur ###\n")
### Héritage direct de ListClustering puisque set n'est pas défini pour LongData


cat("\n####################################################################
######################### Class ClustLongData ######################
############################### Autres #############################
####################################################################\n")

### On a un cld et un num, on plot le longData et la Partition qui va avec.
.plot3d.clusterLongData.num <- function(x,y,varY=1,varZ=2,paramTraj=parTraj(type="n"),paramMean=parMean(),nbSample=200){
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[y[1]][[y[2]]]
    plot3d(x=as(x,"LongData"),y=yPartition,varY=varY,varZ=varZ,paramTraj=paramTraj,paramMean=paramMean,nbSample=nbSample)
    return(invisible())
}
setMethod("plot3d",signature=c("ClusterLongData","numeric"),.plot3d.clusterLongData.num)


### Si y est manquant :
###  - soit il est calculable et on le calcul puis on appelle plot.ClusterLongData
###  - soit il n'est pas calculable et on appelle plot.LongData.num
.plot3d.clusterLongData.missingY <- function(x,y,varY=1,varZ=2,paramTraj=parTraj(),paramMean=parMean(),nbSample=200){
    if(all(is.tna(x["criterionValues"]))){
        plot3d(x=as(x,"LongData"),varY=varY,varZ=varZ,paramTraj=paramTraj,nbSample=nbSample)
        return(invisible())
    }else{
        allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
        y <- as.integer(substr(names(which.max(allCrit)),2,3))
    }
#    .plot3d.clusterLongData.num(x,y,varY=varY,varZ=varZ,paramTraj=paramTraj,paramMean=paramMean,nbSample=nbSample)
    plot3d(x,y,varY=varY,varZ=varZ,paramTraj=paramTraj,paramMean=paramMean,nbSample=nbSample)
    return(invisible())
}
setMethod("plot3d",signature=c("ClusterLongData","missing"),.plot3d.clusterLongData.missingY)




.plot3dPdf.clusterLongData.num <- function(x,y,varY=1,varZ=2){
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[y[1]][[y[2]]]
    return(plot3dPdf(x=as(x,"LongData"),y=yPartition,varY=varY,varZ=varZ))
}
setMethod("plot3dPdf",signature=c("ClusterLongData","numeric"),.plot3dPdf.clusterLongData.num)


.plot3dPdf.clusterLongData.missingY <- function(x,y,varY=1,varZ=2){
    if(all(is.tna(x["criterionValues"]))){
        stop("[plot3dPdf]: y is missing with no possibility to compute it (no available criterion")
    }else{}
    allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
    y <- as.integer(substr(names(which.max(allCrit)),2,3))
    return(plot3dPdf(x,y,varY=varY,varZ=varZ))
}
setMethod("plot3dPdf",signature=c("ClusterLongData","missing"),.plot3dPdf.clusterLongData.missingY)




### On a un cld et un num, on plot le longData et la Partition qui va avec.
.plot.clusterLongData.any <- function(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200){
    if(class(y[1])=="character"){
        y[1] <- substr(y[1],2,3)
        y <- as.numeric(y)
    }else{}
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[y[1]][[y[2]]]
    plot(x=as(x,"LongData"),y=yPartition,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
    return(invisible())
}
#setMethod("plot",signature=c("ClusterLongData","ANY"),.clusterLongData.num.plot)


### Si y est manquant :
###  - soit il est calculable et on le calcul puis on appelle plot.ClusterLongData
###  - soit il n'est pas calculable et on appelle plot.LongData.num
.plot.clusterLongData.missingY <- function(x,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200){
    if(all(is.tna(x["criterionValues"]))){
        plot(x=as(x,"LongData"),paramTraj=paramTraj,paramWindows=paramWindows,nbSample=nbSample)
    }else{
        allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
        y <- as.integer(substr(names(which.max(allCrit)),2,3))
        .plot.clusterLongData.any(x,y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
    }
    return(invisible())
}

##setMethod("plot",signature=c("ClusterLongData","missing"),.clusterLongData.plot)
.plotAll <- function(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200,toPlot=c("both"),nbCriterion=100){
    switch(EXP=toPlot,
           "both"={
               listScreen <- split.screen(matrix(c(0,0.3,0.3,1,0,0,1,1),2))
               screen(listScreen[2])
               paramSubWindows <- paramWindows
               paramSubWindows['closeScreen']<-TRUE
               if(missing(y)){
                   .plot.clusterLongData.missingY(x,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramSubWindows,nbSample=nbSample)
               }else{
                   .plot.clusterLongData.any(x,y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramSubWindows,nbSample=nbSample)
               }
               screen(listScreen[1])
               ## ??? Liste des arguments a vérifier
               plot(as(x,"ListClustering"),nbCriterion=nbCriterion)

               if(paramWindows['closeScreen']){
                   close.screen(listScreen)
                   return(invisible())
               }else{
                   return(listScreen)
               }
           },
           "traj"={
               if(missing(y)){
                   .plot.clusterLongData.missingY(x,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
               }else{
                   .plot.clusterLongData.any(x,y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
               }
           },
           "criterion"={
               plot(as(x,"ListClustering"),criterion=x['criterionActif'],nbCriterion=nbCriterion)
           }
    )
}
setMethod("plot",signature=c("ClusterLongData","missing"),.plotAll)
setMethod("plot",signature=c("ClusterLongData","ANY"),.plotAll)

cat("\n--------------------------------------------------------------------
------------------------- Class ClustLongData ----------------------
--------------------------------- Fin ------------------------------
--------------------------------------------------------------------\n")
