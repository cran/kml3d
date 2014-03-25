## clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################## Class ClustLongData3d #####################
############################## Creation ############################
####################################################################\n")

.ClusterLongData3d.validity <- function(object){
    validObject(as(object,"LongData3d"))
    validObject(as(object,"ListPartition"))
    return(TRUE)
}
cat("### Definition ###\n")
# id       : identifiant of the individual (or lines).
# time     : real time
# varNames : nom of the variable (single now, several in the futur)
# value    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)
setClass(
    Class="ClusterLongData3d",
    contains=c("LongData3d","ListPartition"),
    validity=.ClusterLongData3d.validity
)

setMethod("clusterLongData3d",signature=c("missing","missing","missing","missing","missing","missing"),
    function(traj,idAll,time,timeInData,varNames,maxNA){new("ClusterLongData3d")}
)



### Data.frame ou array en 3D
.clusterLongData3d.constructor <- function(traj,idAll,time,timeInData,varNames,maxNA){
    ## First part : set all the parameters
    if(is.data.frame(traj)){
        if(missing(idAll)){
            idAll <- traj[,1]
        }else{}
        matr <- as.matrix(traj[,sort(na.omit(unlist(timeInData)))])
        lengthTime <- length(timeInData[[1]])
        nbVar <- length(timeInData)
        traj <- array(matr[,rank(unlist(timeInData),na.last="keep")],c(nrow(traj),lengthTime,nbVar))
    }else{
        if(is.array(traj)){
            if(missing(idAll)){
                idAll <- paste("i",1:nrow(traj),sep="")
            }else{}
            if(!missing(timeInData)){
                traj <- traj[,timeInData,,drop=FALSE]
            }else{}
            lengthTime <- dim(traj)[2]
            nbVar <- dim(traj)[3]
        }else{
            stop("[ClusterLongData3d:constructor]: 'traj' should be either a data.frame or an array")
        }
    }
    if(missing(maxNA)){maxNA <- lengthTime-2}else{}
    if(length(maxNA)==1){maxNA <- rep(maxNA,nbVar)}else{}
    if(missing(varNames)){
        if(!missing(timeInData)){
            if(!is.null(names(timeInData))){
                varNames <- names(timeInData)
            }else{
                varNames <- paste("V",1:nbVar,sep="")
            }
        }else{
            varNames <- paste("V",1:nbVar,sep="")
        }
    }else{}
    if(missing(time)){time <- 1:lengthTime}else{}

    ## Second part : all the arguments are non-missing, the object can be build.

    keepId <- apply(t(apply(traj,c(1,3),function(x){sum(is.na(x))}))<=maxNA,2,all)

    traj <- traj[keepId,,,drop=FALSE]
    idFewNA <- idAll[keepId]
    dimnames(traj) <- list(idFewNA,paste("t",time,sep=""),varNames)
    reverse <- matrix(c(0,1),2,length(varNames),dimnames=list(c("mean","sd"),varNames))
    return(new("ClusterLongData3d",
        idAll=as.character(idAll),
        idFewNA=as.character(idFewNA),
        time=time,
        varNames=varNames,
        traj=traj,
        dimTraj=dim(traj),
        maxNA=maxNA,
        reverse=reverse)
    )
}


setMethod("clusterLongData3d",signature=c("ANY","ANY","ANY","ANY","ANY","ANY"),.clusterLongData3d.constructor)

cld3d <- clusterLongData3d



## ### Valable pour traj = matrix
## .ClusterLongData.constructor <- function(traj,idAll,time,varNames,maxNA=length(time)-2){
##     clustLD <- as(longData(...),"ClusterLongData")
##     clustLD["criterionActif"] <- "Calinski.Harabatz"
##     return(clustLD)
## }
## setMethod("clusterLongData",signature=c("ANY","ANY","ANY","ANY","ANY"),.ClusterLongData.constructor)
## cld <- clusterLongData

## setGeneric("as.clusterLongData",function(data,...){standardGeneric("as.clusterLongData")})

## setMethod("as.clusterLongData","LongData",
##     function(data,criterionActif="calinski"){
##         data <- as(data,"ClusterLongData")
##         data["criterionActif"] <- criterionActif
##         return(data)
##     }
## )

## setMethod("as.clusterLongData","data.frame",
##     function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2,criterionActif="calinski"){
##         if(missing(idAll)){idAll <- data[,1]}else{}
##         if(missing(varNames)){varNames <- names(timeDataFrame)}else{}
##         if(missing(time)){time <- 1:length(timeDataFrame[[1]])}else{}
##         matr <- as.matrix(data[,na.omit(unlist(timeDataFrame))])
##         traj <- array(matr[,rank(unlist(timeDataFrame),na.last="keep")],c(length(idAll),length(time),length(varNames)))
##         return(clusterLongData(traj=traj,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA,criterionActif=criterionActif))
##     }
## )

## setMethod("as.clusterLongData","array",
##     function(data,idAll,time,varNames,maxNA=length(time)-2,criterionActif="calinski"){
##         if(missing(idAll)){idAll <- 1:dim(data)[1]}else{}
##         if(missing(varNames)){varNames <- paste("V",1:dim(data)[3],sep="")}else{}
##         if(missing(time)){time <- 1:dim(data)[2]}else{}
##         return(clusterLongData(traj=data,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA,criterionActif=criterionActif))
##     }
## )


## as.cld <- as.clusterLongData

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
#    show(as(object,"ListPartition"))
#}


.ClusterLongData3d.show <- function(object){
    cat("   ~~~ Class: ClusterLongData3d ~~~")
    cat("\n      ~ Sub-Class: LongData3d ~ ")
    showLongData3d(as(object,"LongData3d"))
    cat("\n    ~ Sub-Class: ListPartition ~ ")
    showListPartition(as(object,"ListPartition"))
}
setMethod("show","ClusterLongData3d",.ClusterLongData3d.show)



cat("### Getteur ###\n")
setMethod(
  "[",
  signature=signature(x="ClusterLongData3d", i="character", j="ANY",drop="ANY"),
  definition=function (x, i, j="missing", ..., drop = TRUE){
    .local <- function (x, i, j, drop){
      if (is.numeric(i)) {
        stop("[ClusterLongData3d:getteur]: to get a clusters list, use ['ci']")
      }else{}
      if (i %in% c("criterionValues", "criterionValuesAsMatrix")){
        j <- x['criterionActif']
      }else{}
      if (i %in% c(CRITERION_NAMES, "criterionActif", CLUSTER_NAMES,
                   "criterionValues", "criterionValuesAsMatrix", "sorted",
                   "initializationMethod")) {
        x <- as(x, "ListPartition")
      }else{
        x <- as(x, "LongData3d")
      }
      return(x[i, j])
    }
    .local(x, i, j, ..., drop)
  }
)

## .ClusterLongData3d.get <- function(x,i,j,drop){
##     if(is.numeric(i)){
##         stop("[ClusterLongData3d:getteur]: to get a clusters list, use ['ci']")
##     }else{}
##     if(i%in%c(CRITERION_NAMES,"criterionActif",CLUSTER_NAMES,"criterionValues","criterionValuesAsMatrix","sorted","initializationMethod")){
##         x <- as(x,"ListPartition")
##     }else{
##         x <- as(x,"LongData3d")
##     }
##     return(x[i,j])
## }
## setMethod("[","ClusterLongData3d",.ClusterLongData3d.get)

#getCluster <- function(xCld,nbCluster,clusterRank,asInteger=FALSE){
#    return(xCld[paste("c",nbCluster,sep="")][[clusterRank]][ifelse(asInteger,"clustersAsInteger","clusters")])
#}



cat("### Setteur ###\n")
### Héritage direct de ListPartition puisque set n'est pas défini pour LongData
### ATTENTION !!! Normalement, il faudrait vérifier que la partition est de la BONNE TAILLE !!!

## .ClusterLongData3d.set <- function(x,i,j,...,value){
##     if(i=="add"){
##         if(length(value["clusters"])!=x["nbIdFewNA"]){
##             stop("[ClusterLongData3d:set] the lenght of the Partition should be the same than 'idFewNA'")
##         }else{}
##     }
##     callNextMethod(x, i, j,..., value=value)
## }
## setReplaceMethod("[","ClusterLongData3d",.ClusterLongData3d.set)

setMethod(
  f="[<-",
  signature=signature(x="ClusterLongData3d", i="character", j="missing",value="missing"),
  definition=function (x, i, j="missing", ..., value){
    if (i == "add") {
      if (length(value["clusters"]) != length(x["idFewNA"])) {
        stop("[ClusterLongData3d:set] the lenght of the Partition should be the same than 'idFewNA'")
      }else{}
    }
    callNextMethod(x, i, j=j, ..., value = value)
  }
)


cat("\n####################################################################
######################## Class ClustLongData3d #####################
############################### Autres #############################
####################################################################\n")

### On a un cld et un num, on plot le longData et la Partition qui va avec.
.plot3d.clusterLongData3d.num <- function(x,y,varY=1,varZ=2,parTraj=parTRAJ(),parMean=parMEAN(),nbSample=200,...){
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[paste('c',y[1],sep="")][[y[2]]]
    plotTraj3d(x=as(x,"LongData3d"),y=yPartition,varY=varY,varZ=varZ,parTraj=parTraj,parMean=parMean,nbSample=nbSample,...)
    return(invisible())
}
setMethod("plot3d",signature=c("ClusterLongData3d","numeric"),.plot3d.clusterLongData3d.num)


### Si y est manquant :
###  - soit il est calculable et on le calcul puis on appelle plot.ClusterLongData3d
###  - soit il n'est pas calculable et on appelle plot.LongData3d.num
.plot3d.clusterLongData3d.missingY <- function(x,y,varY=1,varZ=2,parTraj=parTRAJ(),parMean=parMEAN(),nbSample=200,...){
    if(all(is.tna(x["criterionValues"]))){
        plotTraj3d(x=as(x,"LongData3d"),varY=varY,varZ=varZ,parTraj=parTraj,nbSample=nbSample,...)
        return(invisible())
    }else{
        allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
        y <- as.integer(substr(names(which.max(allCrit)),2,3))
    }
    .plot3d.clusterLongData3d.num(x,y,varY=varY,varZ=varZ,parTraj=parTraj,parMean=parMean,nbSample=nbSample,...)
### ICI ###    plot3d(x,y,varY=varY,varZ=varZ,parTraj=parTraj,parMean=parMean,nbSample=nbSample,...)
    return(invisible())
}
setMethod("plot3d",signature=c("ClusterLongData3d","missing"),.plot3d.clusterLongData3d.missingY)
setMethod("plot3d",signature=c("ClusterLongData3d","Partition"),function(x,y,...){plotTraj3d(x,y,...)})




.plot3dPdf.clusterLongData3d.num <- function(x,y,varY=1,varZ=2){
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[paste('c',y[1],sep="")][[y[2]]]
    return(plot3dPdf(x=as(x,"LongData3d"),y=yPartition,varY=varY,varZ=varZ))
}
setMethod("plot3dPdf",signature=c("ClusterLongData3d","numeric"),.plot3dPdf.clusterLongData3d.num)


.plot3dPdf.clusterLongData3d.missingY <- function(x,y,varY=1,varZ=2){
    if(all(is.tna(x["criterionValues"]))){
        stop("[plot3dPdf]: y is missing with no possibility to compute it (no available criterion")
    }else{}
    allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
    y <- as.integer(substr(names(which.max(allCrit)),2,3))
    return(plot3dPdf(x,y,varY=varY,varZ=varZ))
}
setMethod("plot3dPdf",signature=c("ClusterLongData3d","missing"),.plot3dPdf.clusterLongData3d.missingY)




### On a un cld et un num, on plot le longData3d et la Partition qui va avec.
.plot.clusterLongData3d.num <- function(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar']),nbSample=200,...){
#    if(class(y[1])=="character"){
 #       y[1] <- substr(y[1],2,3)
  #      y <- as.numeric(y)
   # }else{}
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[paste('c',y[1],sep="")][[y[2]]]
    plotTraj(x=as(x,"LongData3d"),y=yPartition,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...) ### ICI ###
    return(invisible())
}
#setMethod("plot",signature=c("ClusterLongData3d","ANY"),.clusterLongData3d.num.plot)


### Si y est manquant :
###  - soit il est calculable et on le calcul puis on appelle plot.ClusterLongData3d
###  - soit il n'est pas calculable et on appelle plot.LongData3d.num
.plot.clusterLongData3d.missingY <- function(x,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar']),nbSample=200,...){
    if(all(is.tna(x["criterionValues"]))){
        plotTraj(x=as(x,"LongData3d"),parTraj=parTraj,parWin=parWin,nbSample=nbSample,...) ### ICI ###
    }else{
        allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
        y <- as.integer(substr(names(which.max(allCrit)),2,3))
        .plot.clusterLongData3d.num(x,y,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
    }
    return(invisible())
}

##setMethod("plot",signature=c("ClusterLongData3d","missing"),.clusterLongData3d.plot)
.plotAll <- function(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar']),nbSample=200,toPlot=c("both"),nbCriterion=100,...){
    switch(EXPR=toPlot,
           "both"={
               listScreen <- split.screen(matrix(c(0,0.3,0.3,1,0,0,1,1),2))
               screen(listScreen[2])
               parSubWindows <- parWin
               parSubWindows['closeScreen']<-TRUE
               if(missing(y)){
                   .plot.clusterLongData3d.missingY(x,parTraj=parTraj,parMean=parMean,parWin=parSubWindows,nbSample=nbSample,...)
               }else{
                   .plot.clusterLongData3d.num(x,y,parTraj=parTraj,parMean=parMean,parWin=parSubWindows,nbSample=nbSample,...)
               }
               screen(listScreen[1])
               ## ??? Liste des arguments a vérifier
               plotCriterion(as(x,"ListPartition"),nbCriterion=nbCriterion)

               if(parWin['closeScreen']){
                   close.screen(listScreen)
                   return(invisible())
               }else{
                   return(listScreen)
               }
           },
           "traj"={
               if(missing(y)){
                   .plot.clusterLongData3d.missingY(x,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
               }else{
                   .plot.clusterLongData3d.num(x,y,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
               }
           },
           "criterion"={
               plotCriterion(as(x,"ListPartition"),criterion=x['criterionActif'],nbCriterion=nbCriterion)
           }
    )
}
setMethod("plot",signature=c("ClusterLongData3d","missing"),.plotAll)
setMethod("plot",signature=c("ClusterLongData3d","numeric"),.plotAll)
setMethod("plot",signature=c("ClusterLongData3d","Partition"),function(x,y,...){plotTraj(x,y,...)})


gald3d <- generateArtificialLongData3d <- function(
    nbEachClusters=50,time=0:10,varNames=c("V","T"),
    meanTrajectories=list(function(t){c(0,0)},function(t){c(10,10)},function(t){c(10-t,10-t)}),
    personalVariation=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    residualVariation=function(t){c(rnorm(1,0,2),rnorm(1,0,2))},
    decimal=2,percentOfMissing=0#,clusterLongData=TRUE
){
    nbClusters <- length(meanTrajectories)
    if(length(nbEachClusters)==1){nbEachClusters <- rep(nbEachClusters,nbClusters)}else{}
    if(is.numeric(personalVariation)){eval(parse(text=paste("personalVariation <- function(t){c(rnorm(1,0,",personalVariation,"),rnorm(1,0,",personalVariation,"))}",sep="")))}else{}
    if(length(personalVariation)==1){personalVariation <- rep(list(personalVariation),nbClusters)}else{}
    if(is.numeric(residualVariation)){eval(parse(text=paste("residualVariation <- function(t){c(rnorm(1,0,",residualVariation,"),rnorm(1,0,",residualVariation,"))}",sep="")))}else{}
    if(length(residualVariation)==1){residualVariation <- rep(list(residualVariation),nbClusters)}else{}
    if(length(percentOfMissing)==1){percentOfMissing <- rep(percentOfMissing,nbClusters)}else{}
    nbTime <- length(time)
    nbVar <- length(varNames)
    idAll <- paste("i",1:(sum(nbEachClusters)),sep="")
    indivInCluster <- rep(1:nbClusters,times=nbEachClusters)

    traj <- array(NA,dim=c(sum(nbEachClusters),nbTime,nbVar),dimnames=c(idAll,paste("t",time,sep=""),varNames))
    for (iIndiv in 1:nrow(traj)){
        traj[iIndiv,,] <- t(sapply(time,meanTrajectories[[indivInCluster[iIndiv]]])+personalVariation[[indivInCluster[iIndiv]]](0)+sapply(time,residualVariation[[indivInCluster[iIndiv]]]))
    }
    traj <- round(traj,digits=decimal)

    for (iCluster in 1:nbClusters){
        nbVal <- nbTime*nbEachClusters[iCluster]
        while(sum(is.na(traj[indivInCluster==iCluster,,]))/nbVal < percentOfMissing[iCluster]){
            randL <- floor(runif(1,cumsum(c(0,nbEachClusters))[iCluster]+1,cumsum(nbEachClusters)[iCluster]+1))
            randC <- floor(runif(1,1,nbTime+1))
            randV <- floor(runif(1,1,nbVar+1))
            if(sum(!is.na(traj[randL,,randV]))>1){traj[randL,randC,randV]<-NA}else{}
        }
    }
#    if(clusterLongData){return(as.clusterLongData(traj,idAll=id,time=time,varNames=varNames))}else{
    return(clusterLongData3d(traj,idAll=idAll,time=time,varNames=varNames))
}




cat("\n--------------------------------------------------------------------
------------------------ Class ClustLongData3d ---------------------
--------------------------------- Fin ------------------------------
--------------------------------------------------------------------\n")
