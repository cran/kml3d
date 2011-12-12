cat("\n####################################################################
################################ kml ###############################
############################# Creation #############################
####################################################################\n")


.partitionInitialise <- function(nbClusters,lengthPart,method="randomK",matrixDist){
    switch(method,
        "randomK"={
            part <- rep(NA,lengthPart)
            seeds <- sample(lengthPart,nbClusters)
            part[seeds] <- 1:nbClusters
        },
        "randomAll"={
            part <- floor(runif(lengthPart,1,nbClusters+1))       # Chaque individu recoit une affectation
            seeds <- sample(lengthPart,nbClusters)                # Puis on choisit k individus pour éviter les clusters vides.
            part[seeds] <- 1:nbClusters
        },
        "maxDist"={
            part <- rep(NA,lengthPart)
            seeds <- which(matrixDist==max(matrixDist,na.rm=TRUE),arr.ind=TRUE)[1,]
            while(length(seeds)<nbClusters){
                matrixDist[,seeds] <- 0
	        seeds <- c(seeds,
                    which.max(
                        apply(matrixDist[seeds,],2,min)
                    )[1]
                )
            }
            part[seeds] <- 1:nbClusters
        },
        stop("[PartitionInitialize] invalid initialization methods")
    )
    return(partition(clusters=part,nbClusters=nbClusters))
}
setMethod("partitionInitialise",signature=c("numeric","numeric"),.partitionInitialise)





### ATTENTION : calculCenterGeneralized travaillait avec traj + Partition ;
### Maintenant, il travaille avec traj + part.
#calculCenterGeneralized <- calculTrajMean <- function(traj,part,centerMethod=meanNA){
#    trajMean <- apply(traj, c(2,3), tapply, part, centerMethod)
#    return(trajMean)
#}

### CalculMean : même chose mais en C.


### On suppose que si un centre est NA, il est en dernière ligne de clustersCenter
affectIndiv <- function(traj,clustersCenter,distance=dist3d){
#    if (distance %in% METHODS){distanceFun <- ,method=distance))}}else{distanceFun <- distance}
    nbId <- nrow(traj)
    clusterAffectation <- rep(1,nbId)
    distActuel <- apply(traj,1,function(x){distance(x,clustersCenter[1,,])})
    ##   print(distActuel)
    for(iNbClusters in 2:nrow(clustersCenter)){
        distToMean <- apply(traj,1,function(x){distance(x,clustersCenter[iNbClusters,,])})
 #       print(distToMean)
        cond <- distToMean<distActuel
        cond[is.na(cond)] <- FALSE # Car si cond==NA, c'est que distToMean==NA et donc on ne change pas l'affectation.
        clusterAffectation <- ifelse(cond,rep(iNbClusters,nbId),clusterAffectation)
        distActuel <- ifelse(distToMean<distActuel,distToMean,distActuel)
    }
    return(partition(clusterAffectation,nrow(clustersCenter)))
}

### affectIndiv : même chose, mais en C
kmlSlow <- function(traj,clusterAffectation,toPlot="traj",paramKml=parKml()){
#    if (distance %in% METHODS){distanceFun <- function(x,y){return(dist(t(cbind(x,y)),method=distance))}}else{distanceFun <- distance}
 #   print(distanceFun)
    kmlCenterMethod=paramKml['centerMethod']
    kmlDistance=paramKml['distance']

    exClusterAffectation <- partition()
    if(toPlot%in%c("traj","both")){
        plot(as.longData(traj),clusterAffectation)
    }else{}
    for(iterations in 1:paramKml['maxIt']){
        clustersCenter <- calculTrajMean(traj=traj,part=clusterAffectation['clusters'],centerMethod=kmlCenterMethod)
        clusterAffectation <- affectIndiv(traj=traj,clustersCenter=clustersCenter,distance=kmlDistance)
        if(identical(clusterAffectation,exClusterAffectation)){
            return(list(clusterAffectation=clusterAffectation,convergenceTime=iterations))
        }else{
            exClusterAffectation <- clusterAffectation
        }
        if(toPlot%in%c("traj","both")){
            plot(as.longData(traj),clusterAffectation)
        }else{}
    }
    return(list=c(clusterAffectation=clusterAffectation,convergenceTime=Inf))
}

#kmlFast <- function(traj,clusterAffectation,toPlot="none",paramKml=parKml()){
#    if (distance %in% METHODS){distanceFun <- function(x,y){return(dist(t(cbind(x,y)),method=distance))}}else{distanceFun <- distance}
#   print(distanceFun)
#    kmlCenterMethod=paramKml['centerMethod']
#    kmlDistance=paramKml['distance'

#    exClusterAffectation <- partition()
#    if(toPlot=="traj"){
#        plot(as.longData(traj),clusterAffectation)
#    }else{}
#    for(iterations in 1:paramKml['maxIt']){
#        clustersCenter <- calculCenterGeneralized(traj=traj,part=clusterAffectation['clusters'],centerMethod=kmlCenterMethod)
#        clusterAffectation <- affectIndivGeneralized(traj=traj,clustersCenter=clustersCenter,distance=kmlDistance)
#        if(identical(clusterAffectation,exClusterAffectation)){
#            return(list(clusterAffectation=clusterAffectation,convergenceTime=iterations))
#        }else{
#            exClusterAffectation <- clusterAffectation
#        }
#        if(toPlot=="traj"){
#            plot(as.longData(traj),clusterAffectation)
#        }else{}
#    }
#    return(list=c(clusterAffectation=clusterAffectation,convergenceTime=Inf))
#}


expandStartingCond <- function(startingCond,nbRedrawing,methodUsed){
    if(length(startingCond)==1){
        if(startingCond=="allMethods"){
            if("maxDist"%in%methodUsed){
                startingCond <- c("randomAll",rep("randomK",nbRedrawing))[1:nbRedrawing]
            }else{
                startingCond <- c("maxDist","randomAll",rep("randomK",nbRedrawing))[1:nbRedrawing]
            }
        }else{
            startingCond <- rep(startingCond,nbRedrawing)
        }
    }else{}
    return(startingCond)
}



cutScreen <- function(toPlot){
    return(switch(EXP=toPlot,
          "both"={split.screen(matrix(c(0.3,0,1,0.3,0,0,1,1),2))},
          "traj"={split.screen(matrix(c(0,0,1,1,0,0,1,1),2))},
          "criterion"={split.screen(matrix(c(0,0,1,1,0,0,1,1),2))},
          "none"=NULL)
    )
}



#exCutScreen <- function(toPlot){
#        if(toPlot=="both"){
#        listScreen <- split.screen(matrix(c(0.3,0,1,0.3,0,0,1,1),2))
#    }else{
#        if(
#        listScreen <- split.screen(c(1,1))
#    }
#    return(listScreen)
#}

fastOrSlow <- function(toPlot,distName){
    if(toPlot%in%c("both","traj") | !(distName%in%DISTANCE_METHODS)){
        cat(" ~ Slow KmL ~\n")
        fast <- FALSE
    }else{
        cat(" ~ Fast KmL ~\n")
        fast <- TRUE
    }
    return(fast)
}

.clusterLongData.kml3d <- function(object,nbClusters=2:6,nbRedrawing=20,toPlot="none",paramKml=parKml(),criterionNames=c("calinski","ray","davies","random")){
    nameObject<-deparse(substitute(object))
    on.exit(if(toPlot!="none"){close.screen(listScreen)}else{})

    nbIdFewNA <- object["nbIdFewNA"]
    convergenceTime <- 0
    traj <- object["traj"]
    nbTime <- length(object["time"])
    saveCld <-0

    ################
    ## listScreen[1] (à droite) est pour les traj.
    listScreen <- cutScreen(toPlot)
    if(toPlot%in%c("both","criterion")){
        screen(listScreen[2])
        plot(as(object,"ListClustering"),nbCriterion=paramKml['nbCriterion'])
    }else{}

    ################
    ## Starting conditions
    startingCond <- expandStartingCond(paramKml['startingCond'],nbRedrawing,object["initializationMethod"])
    object["initializationMethod"] <- unique(c(object["initializationMethod"],startingCond))
    if(startingCond[1]=="maxDist"){matDistance <- matDist3d(object["traj"])}else{}

    ################
    ## Fast or Slow, according to distance and to toPlot
    fast <- fastOrSlow(toPlot,paramKml['distanceName'])
    fast <- FALSE

    for(iRedraw in 1:nbRedrawing){
        for(iNbClusters in nbClusters){
            saveCld <- saveCld+1
            clustersInit <- partitionInitialise(nbClusters=iNbClusters,lengthPart=nbIdFewNA,method=startingCond[iRedraw],matrixDist=matDistance)
            clust <- rep(NA,nbIdFewNA)
            if(fast){
 #               resultKml <- .C("kml",as.double(t(trajNoNA)),iNbInd=as.integer(nbId),iNbTime=as.integer(nbTime),
  #                              iNbCluster=as.integer(iNbClusters),maxIt=as.integer(maxIt),
   #                             distance=as.integer(distInt),power=as.numeric(power),vClusterAffectation1=as.integer(clustersInit["clusters"]),
    #                            convergenceTime=as.integer(convergenceTime),
     #                           NAOK=TRUE,PACKAGE="kml")[c(8,9)]
      #          clust[noNA] <- resultKml[[1]]
            }else{
                if(toPlot%in%c("both","traj")){screen(listScreen[1])}else{}
                resultKml <- kmlSlow(traj=traj,clusterAffectation=clustersInit,toPlot=toPlot,paramKml=paramKml)
                ## clust <- resultKml[[1]]["clusters"]
            }
            yPartition <- ordered(partition(nbClusters=iNbClusters,clusters=resultKml[[1]]["clusters"]))

            if(yPartition['nbClusters']>1){
                object["add"] <- clustering(xLongData=as(object,"LongData"),yPartition=yPartition,convergenceTime=resultKml[[2]],multiplicity=1,
                                            criterionNames=criterionNames,
                                            algorithm=c(algo="kmeans",startCond=startingCond[iRedraw],imputation=paramKml['imputationMethod']))
            }else{}

            assign(nameObject,object,envir=parent.frame())
            cat("*")
            if(saveCld>=5){#paramKml['saveFreq']){
                save(list=nameObject,file=paste(nameObject,".Rdata",sep=""))
                saveCld <- 0
                cat("\n")
            }else{}
            if(toPlot=="both"){
                screen(listScreen[2])
                plot(as(object,"ListClustering"),nbCriterion=paramKml['nbCriterion'])
            }else{
                if(toPlot=="criterion"){
                    plot(as(object,"ListClustering"),nbCriterion=paramKml['nbCriterion'])
                }else{}
            }
        }
    }

    ordered(object)
    assign(nameObject,object,envir=parent.frame())
    save(list=nameObject,file=paste(nameObject,".Rdata",sep=""))
    ## La fenetre graphique est fermée grace a 'on.exit' défini en début de fonction
    return(invisible())
}
setMethod("kml3d","ClusterLongData",.clusterLongData.kml3d)

#            if(toPlot=="both"){
 #               screen(listScreen[2])
  #              plot(as(object,"ListClustering"),nbCriterion=paramKml['nbCriterion'])
   #         }else{
    #            if(toPlot=="criterion"){
     #               screen(listScreen[1])
      #              plot(as(object,"ListClustering"),nbCriterion=paramKml['nbCriterion'])
       #         }else{}
        #    }else{}



.Clustering.export <- function(object,y,nameObject,typeGraph="bmp",paramTraj=parTraj(),paramMean=parMean(),
                                   paramWindows=windowsCut(object['nbVar']),...){
#    col="clusters",type="l",
#    col.mean="clusters",type.mean="b",main="",cex=1,
#    pch.mean="letters",pch.time=NA,...#,legends=TRUE,...
#){
    part <- object[y[1]][[y[2]]]

    dataFrame <- data.frame(id=object["idAll"],clusters=NA)
    dataFrame$clusters[dataFrame$id%in%object['idFewNA']] <- part["clusters"]
    write.csv2(dataFrame,file=paste(nameObject,"-Clusters.csv",sep=""),row.names=FALSE)

    detail <- c(part["nbClusters"],part["percentEachCluster"],part["criterionValues"],
                part["algorithm"],part["convergenceTime"])
    names(detail) <-  c("nbClusters",paste("percent",LETTERS[1:part["nbClusters"]]),part["criterionNames"],
                       "algorithmUsed","startingCondition","imputationMethod","convergenceTime")
    write.csv2(detail,file=paste(nameObject,"-Details.csv",sep=""),row.names=TRUE)

    trajMean <- data.frame(calculTrajMean(object['traj'],part['clusters']))
    write.csv2(trajMean,file=paste(nameObject,"-TrajMean.csv",sep=""),row.names=TRUE)

    eval(parse(text=paste(typeGraph,"(filename='",nameObject,"-Traj.",typeGraph,"',...)",sep="")))
    plot(as(object,"LongData"),part,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows)
    dev.off()
        #lty=lty,lty.mean=lty.mean,pch=pch,pch.mean=pch.mean,pch.time=pch.time,
        #xlab=xlab,ylab=ylab,ylim=ylim,cex.mean=cex.mean,legends=legends,sizeMin=sizeMin,...)
#    savePlot(filename=paste(nameObject,"-Traj",sep=""),type=typeGraph)
    return(invisible())
}
setMethod("exportClustering",signature=c("ClusterLongData","numeric"),.Clustering.export)





choiceChangeParam <- function(paramChoice){
    xy <- paramChoice['xy']

    texte <- paste("     ~ Choice : menu ~
 - 'Arrow' : change partition
 - 'Space' : select/unselect a partition
 -    e    : change the display (",paramChoice['toPlot'],")
 -    d    : change actif criterion (",paramChoice['critPossible'][paramChoice['critRank']],")
 -    c    : sort according to the actif criterion (",paramChoice['critSorted'][paramChoice['critRank']],"),
 -    r    : change the trajectories's style (type=",CHOICE_STYLE[['typeTraj']][paramChoice['styleTrajRank']],
                                            "; col=",CHOICE_STYLE[['colTraj']][paramChoice['styleTrajRank']],")
 -    f    : change the means trajectories' style (type=",CHOICE_STYLE[['typeMean']][paramChoice['styleMeanRank']],
                                    "; col=",CHOICE_STYLE[['colMean']][paramChoice['styleMeanRank']],
                                    "; pch=",CHOICE_STYLE[['pchMean']][paramChoice['styleMeanRank']],")
 -   g/t   : change the symbol size (",paramChoice['cex'],")
 -   y/h   : change the number of symbols (freq=1/",1+paramChoice['pchPeriod'],")
     ~ 'Return' when its done ~\n",sep="")

    choix <- getGraphicsEvent(texte,onKeybd=function(key){return(key)})
    switch(EXP=choix,
           "Up"    = {
               if(xy[1]>1){
                   paramChoice['toDo'] <- "xy"
                   xy[2]<-1
                   xy[1]<-xy[1]-1
                   paramChoice['xy']<-xy
#                       if(is.na(critMatrix[y[1],1])){
 #                          y[1] <- y[1]+1-which.min(is.na(critMatrix[,1][y[1]:1]))
  #                         if(is.na(critMatrix[y[1],1])){
   #                            y[1] <- y[1]-1+which.min(is.na(critMatrix[,1][y[1]:52]))
    #                       }else{}
     #                  }else{}
               }else{paramChoice['toDo'] <- ""}
           },
           "Down"  = {
               if(xy[1]<nrow(paramChoice['critMatrix'])){
                   paramChoice['toDo'] <- "xy"
                   xy[2]<-1
                   xy[1]<-xy[1]+1
                   paramChoice['xy']<-xy
#                       if(is.na(critMatrix[y[1],1])){
 #                          y[1] <- y[1]-1+which.min(is.na(critMatrix[,1][y[1]:52]))
  #                         if(is.na(critMatrix[y[1],1])){
   #                            y[1] <- y[1]+1-which.min(is.na(critMatrix[,1][y[1]:1]))
    #                       }else{}
     #                  }else{}
               }else{paramChoice['toDo'] <- ""}
           },
           "Right" = {
               paramChoice['toDo'] <- "xy"
               if(xy[2]<ncol(paramChoice['critMatrix']) && !is.na(paramChoice['critMatrix'][xy[1],xy[2]+1])){
                   paramChoice['toDo'] <- "xy"
                   xy[2]<-xy[2]+1
                   paramChoice['xy']<-xy
               }else{paramChoice['toDo'] <- ""}
           },
           "Left"  = {
               paramChoice['toDo'] <- "xy"
               if(xy[2]>1){
                   paramChoice['toDo'] <- "xy"
                   xy[2]<-xy[2]-1
                   paramChoice['xy']<-xy
               }else{paramChoice['toDo'] <- ""}

           },

           "ctrl-J" = {
               paramChoice['toDo'] <- "EXIT"
           },
           " "      = {
               paramChoice['toDo'] <- ""
               if(list(xy) %in% paramChoice['selectedPart']){
                   paramChoice['selectedPart'] <- paramChoice['selectedPart'][!(paramChoice['selectedPart'] %in% list(xy))]
               }else{
                   paramChoice['selectedPart'] <- c(paramChoice['selectedPart'],list(xy))
               }
           },
           "e" = {
               paramChoice['toDo'] <- ""
               paramChoice['toPlot'] <- ifelse(paramChoice['toPlot']=="both","traj",
                                               ifelse(paramChoice['toPlot']=="traj","criterion","both")
                                               )
           },
           "r" = {
               paramChoice['toDo'] <- "parTraj"
               paramChoice['styleTrajRank'] <- paramChoice['styleTrajRank']%%3+1
           },
           "f" = {
               paramChoice['toDo'] <- "parMean"
               paramChoice['styleMeanRank'] <- paramChoice['styleMeanRank']%%7+1
           },
           "t" = {
               paramChoice['toDo'] <- "parMean"
               paramChoice['cex'] <- paramChoice['cex']+0.1
           },
           "g" = {
               paramChoice['toDo'] <- "parMean"
               paramChoice['cex'] <- paramChoice['cex']-0.1
           },
           "h" = {
               paramChoice['toDo'] <- "parMean"
               paramChoice['pchPeriod'] <- ceiling(paramChoice['pchPeriod']*1.05+0.05)
#               if(paramChoice['pchPeriod']>paramChoice['nbTime']){
 #                  paramChoice['pchPeriod'] <- paramChoice['nbTime']}else{}
           },
           "y" = {
               paramChoice['toDo'] <- "parMean"
               paramChoice['pchPeriod'] <- floor(paramChoice['pchPeriod']/1.05)
               if(paramChoice['pchPeriod']<0){paramChoice['pchPeriod'] <- 0}else{}
           },
           "d" = {
               paramChoice['toDo'] <- "changeCriterion"
               paramChoice['critRank'] <- paramChoice['critRank']%%length(paramChoice['critPossible'])+1
#               object['criterionActif'] <- paramChoice['critPossible'][paramChoice['critRank']]
 #              ordered(object)
#               critMatrix <- object["criterionValues",paramChoice['critPossible'][paramChoice['critRank']]]
 #              lengthList <- max(sapply(critMatrix , length))
  #             critMatrix <- t(sapply(critMatrix , function(x) c(x,rep(NA,lengthList-length(x)))))
   #            paramChoice['critMatrix'] <- critMatrix
           },
           "c" = {
               paramChoice['toDo'] <- "order"
           },
           default={}

           )
    return(paramChoice)
}
#paramChoice <- parChoice()
#cleanProg(choiceChangeParam,,,2) # CHOICE_STYLE length



partPermut <- function(selectedPart,matPermut){
    onePermut <- function(xy){
        xy[2] <- which(matPermut[xy[1],]%in%xy[2])
        return(xy)
    }
    return(lapply(selectedPart,onePermut))
}

cat("### Method: 'choice' pour clusterizLongData ###\n")
.clusterLongData.choice <- function(object,typeGraph="bmp",...){
    nameObject <- deparse(substitute(object))

   # pchTime <- object["time"]
#    pchFreq <- length(pch.time)

  #  size <- 1
 #   nbTime <- object["nbTime"]

    ## Fonction qui 'cercle' les selected
    pointCal <- function(z){points(z[2],critMatrix[z[1],z[2]],lwd=3,cex=3)}

    nbVar <- object['nbVar']
    critMatrix <- object["criterionValuesAsMatrix"]
    critMatrixRowName <- match(rownames(critMatrix),CLUSTER_NAMES)+1

    y <- as.numeric(c(which.max(critMatrix[,1]),1))
    paramTraj <- parTraj()
    paramMean <- parMean()
#    calSelected <- list()
    paramChoice <- parChoice(xy=y,nbTime=object['nbTime'],critMatrix=critMatrix,selectedPart=list(),critPossible=object['criterionPossibles'],critSorted=object['criterionPossibles']%in%object['criterionActif'])
    if(!object['sorted']){paramChoice['critSorted']<-rep(FALSE,length(object['criterionPossibles']))}else{}

    listScreen <- plot(object,c(critMatrixRowName[y[1]],y[2]),toPlot="both",paramWindows=windowsCut(nbVar,TRUE,FALSE))
    if(paramChoice['toPlot']=="both"){points(y[2],critMatrix[y[1],y[2]],pch=19,lwd=5)}else{}
    close.screen(listScreen)

    while(TRUE){
    #   print(paramChoice['selectedPart'])
        paramChoice <- choiceChangeParam(paramChoice)
        switch(EXP=paramChoice['toDo'],
               "xy"={y <- paramChoice['xy']},
               "parTraj"={paramTraj <- parTraj(type=CHOICE_STYLE[['typeTraj']][paramChoice['styleTrajRank']],
                                               col=CHOICE_STYLE[['colTraj']][paramChoice['styleTrajRank']])
               },
               "parMean"={paramMean <- parMean(type=CHOICE_STYLE[['typeMean']][paramChoice['styleMeanRank']],
                                               col=CHOICE_STYLE[['colMean']][paramChoice['styleMeanRank']],
                                               pch=CHOICE_STYLE[['pchMean']][paramChoice['styleMeanRank']],
                                               pchPeriod=paramChoice['pchPeriod'],
                                               cex=paramChoice['cex'])
               },
               "changeCriterion"={
                   object['criterionActif'] <- paramChoice['critPossible'][paramChoice['critRank']]
                   paramChoice['critMatrix'] <- critMatrix <- object["criterionValuesAsMatrix"]
               },
               "order"={
                   matPermut <- ordered(object)
                   paramChoice['critMatrix'] <- critMatrix <- object["criterionValuesAsMatrix"]
                   paramChoice['selectedPart'] <- partPermut(paramChoice['selectedPart'],matPermut)
                   y <- paramChoice['xy'] <- unlist(partPermut(list(paramChoice['xy']),matPermut))
               },
               "EXIT"={break;}
        )


        listScreen <- plot(object,c(critMatrixRowName[y[1]],y[2]),toPlot=paramChoice['toPlot'],paramTraj=paramTraj,paramMean=paramMean,
                           paramWindows=windowsCut(nbVar,TRUE,FALSE))
        if(paramChoice['toPlot']=="both"){
            points(y[2],critMatrix[y[1],y[2]],pch=19,lwd=5)
            lapply(paramChoice['selectedPart'],pointCal)
        }else{}
        close.screen(listScreen)
    }

    nameObject <- paste(nameObject,"-C",y[1],"-",y[2],sep="")
    if(length(paramChoice['selectedPart'])!=0){
        for(iY in paramChoice['selectedPart']){
            exportClustering(object=object,y=c(critMatrixRowName[iY[1]],iY[2]),
                             nameObject=paste(nameObject,"-C",iY[1],"-",iY[2],sep=""),
                             typeGraph=typeGraph,paramTraj=paramTraj,
                             paramMean=paramMean,paramWindows=windowsCut(nbVar),...)
        }
        eval(parse(text=paste(typeGraph,"(filename='",nameObject,"-criterionActif.",typeGraph,"',...)",sep="")))
            plot(object,toPlot="criterion")
        dev.off()
        eval(parse(text=paste(typeGraph,"(filename='",nameObject,"-criterionAll.",typeGraph,"',...)",sep="")))
            plot(as(object,"ListClustering"),nbCriterion=1,criterion=object['criterionPossibles'])
        dev.off()
    }


#    lapply(paramChoice['selectedPart'],exportSelected)
#                               col=colTrajPossible[styleTraj],type=typeTrajPossible[styleTraj],
 #                              col.mean=colMeanPossible[styleMeanTraj],type.mean=typeMeanPossible[styleMeanTraj],main="",cex=size,
  #                             pch.mean=pchMeanPossible[styleMeanTraj],pchTime=pchTime,
   #                            col.sub=colTrajPossible[styleTraj],type.sub=typeTrajPossible[styleTraj],
    #                           col.mean.sub=colMeanPossible[styleMeanTraj],type.mean.sub=typeMeanPossible[styleMeanTraj],main.sub="")
}
setMethod("choice",signature=c("ClusterLongData"),.clusterLongData.choice)



cat("\n-------------------------------------------------------------------
------------------------------- kml -------------------------------
------------------------------- Fin -------------------------------
-------------------------------------------------------------------\n")
