cat("\n####################################################################
########################## Class LongData ##########################
############################# Creation #############################
####################################################################\n")

### Pas de trajectoire totalement vide => maxNA<length(time)

cat("### Definition ###\n")
.LongData.validity <- function(object){
#    cat("**** validity LongData ****\n")
    if(length(object@idAll)==0&length(object@time)==0&length(object@varNames)==0&length(object@traj)==0){
    }else{
        if(any(c(length(object@idAll)==0,length(object@time)==0,length(object@varNames)==0,length(object@traj)==0))){
            stop("[LongData:validity]: at least one slot is empty")}else{}
        if(length(object@idFewNA)!=dim(object@traj)[1]){
            stop("[LongData:validity]: The number of id does not fit with the number of trajectories
  [LongData:validity]: length(object@idFewNA)=",length(object@idFewNA)," dim(object@traj)[1]=",dim(object@traj)[1])}else{}
        if(length(object@time)!=dim(object@traj)[2]){
            stop("[LongData:validity]: The number of time does not fit with the length of trajectories
  [LongData:validity]: length(object@time)=",length(object@time)," dim(object@traj)[2]",dim(object@traj)[2])}else{}
        if(length(object@varNames)!=dim(object@traj)[3]){
            stop("[LongData:validity]: The number of variable does not fit with the width ot trajectories
  [LongData:validity]: length(object@varNames)=",length(object@varNames)," dim(object@traj)[3]=",dim(object@traj)[3])}else{}
        if(any(is.na(object@time))){
            stop("[LongData:validity]: There is some unknow times
  [LongData:validity]: is.na(object@time)=",is.na(object@time))}else{}
        if(!identical(object@time,sort(object@time))){
            stop("[LongData:validity]: time is not in the right order
  [LongData:validity]: object@time=",object@time)}else{}
        if(any(duplicated(object@time))){
            stop("[LongData:validity]: Some time are duplicate
  [LongData:validity]: duplicated(object@time)=",duplicated(object@time))}else{}
        if(any(is.na(object@idAll))){
            stop("[LongData:validity]: Some idAll are NA
  [LongData:validity]: is.na(object@idAll)=",is.na(object@idAll))}else{}
        if(any(duplicated(object@idAll))){
            stop("[LongData:validity]: Some idAll are duplicate
  [LongData:validity]: duplicated(object@idAll)=",duplicated(object@idAll))}else{}
        if(any(dimnames(object@traj)[[1]]!=object@idFewNA,
               dimnames(object@traj)[[2]]!=paste("T",object@time,sep=""),
               dimnames(object@traj)[[3]]!=object@varNames)){
            stop("[LongData:validity]: dimnames of traj is not correct
  [LongData:validity]: dimnames(object@traj)",dimnames(object@traj),"
  [LongData:validity]: object@idFewNA=",object@idFewNA,"
  [LongData:validity]: paste('T',object@time,sep='')=",paste("T",object@time,sep=""),"
  [LongData:validity]: object@varNames=",object@varNames)}else{}
        if(max(object@maxNA)>=length(object@time)){
            stop("[LongData:validity]: some maxNA are too high (trajectories with only NA are not trajectories)
  [LongData:validity]: object@maxNA=",object@maxNA," length(object@time)=",length(object@time))}else{}
    }
}

setClass(
    Class="LongData",
    representation=representation(
        idAll="character",
        idFewNA="character",
        time="numeric",
        varNames="character",
        traj="array",
        dimTraj="numeric",
        maxNA="numeric",
        reverse="matrix"
    ),
    prototype=prototype(
        idAll=character(),
        idFewNA=character(),
        time=numeric(),
        varNames=character(),
        traj=array(dim=c(0,0,0)),
        dimTraj=numeric(),
        maxNA=numeric(),
        reverse=matrix(NA,2)
    ),
    validity=.LongData.validity
)


cat("\n###################################################################
########################## Class LongData #########################
########################### Constructeur ##########################
###################################################################\n")

setMethod("longData",signature=c("missing","missing","missing","missing","missing"),
    function(traj,idAll,time,varNames,maxNA=length(time)-2){new("LongData")}
)
### Pour un array mais en donnant tous les détails
.LongData.constructor <- function(traj,idAll,time,varNames,maxNA=length(time)-2){
    if(length(maxNA)==1){maxNA <- rep(maxNA,length(varNames))}else{}

    ## X1 <- apply(traj,c(1,3),function(x){sum(is.na(x))}) compte le nombre de NA par indiv et par variable
    ## X2 <- t(X1)<=maxNA pour chaque ligne (ie chaque variable), indique TRUE si le nombre de NA est plus petit que le maxNA correspondant
    ## apply(X2,2,all) vérifie que la condition est bonne pour toutes les variables.
    keepId <- apply(t(apply(traj,c(1,3),function(x){sum(is.na(x))}))<=maxNA,2,all)

    ## Si on permet l'excusion globale, la formule est :
    ## keepId <- apply(traj,1,function(x)(sum(is.na(x))<=maxNA))
    traj <- traj[keepId,,,drop=FALSE]
    idFewNA <- idAll[keepId]
    dimnames(traj) <- list(idFewNA,paste("T",time,sep=""),varNames)
    reverse <- matrix(c(0,1),2,length(varNames),dimnames=list(c("mean","sd"),varNames))
    new("LongData",
        idAll=as.character(idAll),
        idFewNA=as.character(idFewNA),
        time=time,
        varNames=varNames,
        traj=traj,
        dimTraj=dim(traj),
        maxNA=maxNA,
        reverse=reverse)
}
setMethod("longData",signature=c("ANY","ANY","ANY","ANY","ANY"),.LongData.constructor)



cat("### Conversion d'un data.frame en longData ###\n")

setGeneric("as.longData",function(data,...){standardGeneric("as.longData")})

#as.longData <- function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2){UseMethod("as.longData")}

setMethod("as.longData","data.frame",
    function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2){
        if(missing(idAll)){idAll <- data[,1]}else{}
        if(missing(varNames)){varNames <- names(timeDataFrame)}else{}
        if(missing(time)){time <- 1:length(timeDataFrame[[1]])}else{}
        matr <- as.matrix(data[,na.omit(unlist(timeDataFrame))])
        traj <- array(matr[,rank(unlist(timeDataFrame),na.last="keep")],c(length(idAll),length(time),length(varNames)))
        return(longData(traj=traj,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA))
    }
)

setMethod("as.longData","array",
    function(data,idAll,time,varNames,maxNA=length(time)-2){
        if(missing(idAll)){idAll <- 1:dim(data)[1]}else{}
        if(missing(varNames)){varNames <- paste("V",1:dim(data)[3],sep="")}else{}
        if(missing(time)){time <- 1:dim(data)[2]}else{}
        return(longData(traj=data,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA))
    }
)


cat("\n###################################################################
########################## Class LongData #########################
############################ Accesseurs ###########################
###################################################################\n")

cat("### Getteur ###\n")
.longData.get <- function(x,i,j,drop){
    switch(EXPR=i,
           "idAll"={return(x@idAll)},
           "idFewNA"={return(x@idFewNA)},
           "varNames"={return(x@varNames)},
           "time"={return(x@time)},
           "traj"={return(x@traj)},
           "dimTraj"={return(x@dimTraj)},
           "nbIdFewNA"={return(x@dimTraj[1])},
           "nbTime"={return(x@dimTraj[2])},
           "nbVar"={return(x@dimTraj[3])},
           "maxNA"={return(x@maxNA)},
           "reverse"={return(x@reverse)},
           stop("[LongData:get]:",i," is not a 'LongData' slot")
    )
}
setMethod("[","LongData",.longData.get)


### A priori, on n'a jamais besoin de modifier un LongData après sa création.
### ATTENTION : le set de ClusterLongData hérite directement de ListClustering
###    puisque set n'est pas défini pour LongData. Si on ajoute un set pour LongData,
###    il faut corriger le set de ClusterLongData

#cat("### Setteur ###\n")
#setMethod("[<-","LongData",
#    function(x,i,j,value){
#        switch(EXPR=i,
#            "id"={x@id<-as.character(value)},
#            "varNames"={
#                x@varNames<-value
#                dimname(traj)[[3]] <- values
#            },
#            "time"={x@time<-value},
#            "traj"={x@traj<-value},
#            "other"={x@other<-value},
#            stop("[LongData:getteur]: this is not a LongData slot")
#        )
#        dimnames(x@traj) <- list(x@id,paste(x@varName,x@time,sep=""))
#        validObject(x)
#        return(x)
#    }
#)



cat("\n###################################################################
########################## Class LongData #########################
############################# Affichage ###########################
###################################################################\n")

cat("### Method: 'show' pour LongData ###\n")
.longData.show <- function(object){
    cat("\n~ idAll       = [",length(object@idAll),"] ",sep="");catShort(object@idAll)
    cat("\n~ idFewNA     = [",object['nbIdFewNA'],"] ",sep="");catShort(object@idFewNA)
    cat("\n~ varNames    = [",object['nbVar'],"] ",sep="");catShort(object@varNames)
    cat("\n~ time        = [",object['nbTime'],"] ",sep="");catShort(object@time)
    cat("\n~ maxNA       = [",object['nbVar'],"] ",sep="");catShort(object@maxNA)
    cat("\n~ reverse     = [2x",object['nbVar'],"]",sep="");
    cat("\n    - mean    =",object['reverse'][1,])
    cat("\n    - SD      =",object['reverse'][2,])
    cat("\n\n~ traj = [",object['nbIdFewNA'],"x",object['nbTime'],"x",object['nbVar'],"] (limited to 5x10x3)  :\n",sep="")
    if(length(object@idFewNA)!=0){
        for(iVar in 1:min(3,length(object@varNames))){
            cat("\n",object@varNames[iVar],":\n")
            if(ncol(object@traj)>10){
                trajToShow <- as.data.frame(object@traj[,1:10,iVar])
                trajToShow$more <- "..."
            }else{
                trajToShow <- as.data.frame(object@traj[,,iVar])
            }
            if(nrow(object@traj)>5){
                print(trajToShow[1:5,])
                cat("... ...\n")
            }else{
                print(trajToShow)
            }
        }
    }else{cat("   <no trajectories>\n")}
    return(invisible(object))
}
setMethod("show","LongData",
    definition=function(object){
        cat("\n   ~~~ Class: LongData ~~~")
        .longData.show(object)
    }
)


cat("### Method: 'print' pour LongData ###\n")
.longData.print <- function(x){
    object <- x
    cat("\n   ~~~ Class: LongData ~~~")
    cat("\n~ Class :",class(object))
    cat("\n\n~ traj = [",object['nbIdFewNA'],"x",object['nbTime'],"x",object['nbVar'],"] (limited to 5x10x3)  :\n",sep="")
    print(object['traj'])
    cat("\n\n~ idAll = [",length(object@idAll),"]\n",sep="");print(object@idAll)
    cat("\n~ idFewNA = [",object['nbIdFewNA'],"]\n",sep="");print(object@idFewNA)
    cat("\n~ varNames = [",object['nbVar'],"]\n",sep="");print(object@varNames)
    cat("\n~ time = [",object['nbTime'],"]\n",sep="");print(object@time)
    cat("\n~ maxNA = [",object['nbVar'],"]\n",sep="");print(object@maxNA)
    cat("\n~ reverse mean =\n");print(object['reverse'][1,])
    cat("\n~ reverse SD =\n");print(object['reverse'][2,])
    return(invisible(object))
}
setMethod("print","LongData",.longData.print)





### Calcule les trajectoires moyennes de chaque clusters.
### A noter, traj est un array et part est un vecteur
### NE FONCTIONNE PAS POUR LES PARTITIONS A UN SEUL CLUSTER
calculTrajMean <- function(traj,part,centerMethod=function(x){mean(x,na.rm=TRUE)}){#,nbClusters,nbTime,nbVar){
#    if(length(table(part))==1){
  #      trajMean <- array(apply(traj, c(2,3), tapply, part, meanNA),dim=c(1,dim(traj)[2:3]))
 #   }else{
    trajMean <- apply(traj, c(2,3), tapply, part, centerMethod)
   # }
    return(trajMean)
}

cat("\n###################################################################
########################## Class LongData #########################
############################ imputation ###########################
###################################################################\n")


cat("### LOCF & LOCB ###\n")

trajImput.LOCB.begin <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.LOCB.begin]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{}
    firstNoNA <- min(which(!is.na(oneTraj)))
    oneTraj[1:firstNoNA]<-oneTraj[firstNoNA]
    return(oneTraj)
}

trajImput.LOCF.end <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.LOCB.end]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{}
    lastNoNA <- max(which(!is.na(oneTraj)))
    oneTraj[lastNoNA:length(oneTraj)]<-oneTraj[lastNoNA]
    return(oneTraj)
}

trajImput.LOCB.middle <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.LOCB.middle]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{
        if(is.na(oneTraj[length(oneTraj)])){stop("[LongData:trajImput.LOCB.middle]: Last value is NA. Impute last value first.\n")}else{}
    }
    while(any(is.na(oneTraj))){
        oneTraj[max(which(is.na(oneTraj)))]<-oneTraj[max(which(is.na(oneTraj)))+1]
    }
    return(oneTraj)
}

trajImput.LOCF.middle <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.LOCF.middle]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{
        if(is.na(oneTraj[1])){stop("[LongData:trajImput.LOCF.middle]: First value is NA. Impute first value first.\n")}else{}
    }
    while(any(is.na(oneTraj))){
        oneTraj[min(which(is.na(oneTraj)))]<-oneTraj[min(which(is.na(oneTraj)))-1]
    }
    return(oneTraj)
}

trajImput.LOCB <- function(oneTraj){
    return(trajImput.LOCB.middle(trajImput.LOCF.end(oneTraj)))
}

trajImput.LOCF <- function(oneTraj){
    return(trajImput.LOCF.middle(trajImput.LOCB.begin(oneTraj)))
}




###################################################################
###################### Interpolation Lineraire ####################
###################################################################

cat("### Linear interpolation ###\n")

###############
### Linear interpolation Middle

trajImput.interpoLin.middle <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.interpoLin.middle]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{}
    if(is.na(oneTraj[1])|is.na(oneTraj[length(oneTraj)])){
        stop("[LongData:trajImput.interpoLin.middle]: First or last value is NA; impute first and last values first\n")
    }else{}

    while(any(is.na(oneTraj))){
        NAinfM <- min(which(is.na(oneTraj)))-1
        NAsupM <- min(which(!is.na( oneTraj[-(1:NAinfM)] ))) + NAinfM
        oneTraj[NAinfM:NAsupM] <- seq(from=oneTraj[NAinfM],to=oneTraj[NAsupM],length.out=NAsupM-NAinfM+1)
    }
    return(oneTraj)
}



###############
### Linear interpolation 2 : global slope

trajImput.globalSlope.beginEnd <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.globalSlope.beginEnd]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{}
    if(sum(!is.na(oneTraj))==1){
        warning("[LongData:trajImput.globalSlope.beginEnd]: There is only one non-missing value on this line.
  [LongData:trajImput.globalSlope.beginEnd]: LOCF and LOCB are used instead interpolation.\n")
        return(rep(oneTraj[!is.na(oneTraj)],length(oneTraj)))
    }else{}

    lengthTraj <- length(oneTraj)
    firstNoNA <- min(which(!is.na(oneTraj)))
    lastNoNA <- max(which(!is.na(oneTraj)))

    a <- (oneTraj[firstNoNA]-oneTraj[lastNoNA])/(firstNoNA-lastNoNA)
    b <- oneTraj[lastNoNA] - a*lastNoNA
    indNA <- c(1:firstNoNA,lastNoNA:lengthTraj)
    oneTraj[indNA]<-a*indNA+b
    return(oneTraj)
}


trajImput.linInterGlobal <- function(oneTraj){
    return(trajImput.interpoLin.middle(trajImput.globalSlope.beginEnd(oneTraj)))
}



###############
### Linear interpolation 3 : Local

trajImput.localSlope.beginEnd <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.localSlope.beginEnd]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{}
    if(sum(!is.na(oneTraj))==1){
        warning("[LongData:trajImput.localSlope.beginEnd]: There is only one non-missing value on this line.
  [LongData:trajImput.localSlope.beginEnd]: LOCF and LOCB are used instead interpolation.\n")
        return(rep(oneTraj[!is.na(oneTraj)],length(oneTraj)))
    }else{}

    firstNoNA <- min(which(!is.na(oneTraj)))
    secondNoNA <- min(which(!is.na(oneTraj[-firstNoNA])))+1

    a <- (oneTraj[firstNoNA]-oneTraj[secondNoNA])/(firstNoNA-secondNoNA)
    b <- oneTraj[secondNoNA] - a*secondNoNA
    indNA <- 1:firstNoNA
    oneTraj[indNA]<-a*indNA+b

    lengthTraj <- length(oneTraj)
    lastNoNA <- max(which(!is.na(oneTraj)))
    penultimateNoNA <- max(which(!is.na(oneTraj[-lastNoNA])))

    a <- (oneTraj[penultimateNoNA]-oneTraj[lastNoNA])/(penultimateNoNA-lastNoNA)
    b <- oneTraj[lastNoNA] - a*lastNoNA
    indNA <- lastNoNA:lengthTraj
    oneTraj[indNA]<-a*indNA+b
    return(oneTraj)
}

trajImput.linInterLocal <- function(oneTraj){
    return(trajImput.interpoLin.middle(trajImput.localSlope.beginEnd(oneTraj)))
}



###############
### Linear interpolation 4 : LOCF

trajImput.linInterLOCF <- function(oneTraj){
    return(trajImput.interpoLin.middle(trajImput.LOCF.end(trajImput.LOCB.begin(oneTraj))))
}


###############
### Linear interpolation 1

#trajImput.bissectrice.beginEnd <- function(oneTraj){
#    if(all(is.na(oneTraj))){
#        warning("[LongData:trajImput.bissectrice.beginEnd]: There is only NA on this line, impossible to impute\n")
#        return(oneTraj)
#    }else{}
#    if(sum(!is.na(oneTraj))==1){
#        warning("[LongData:trajImput.bissectrice.beginEnd]: There is only one non-missing value on this line.
#  [LongData:trajImput.bissectrice.beginEnd]: LOCF and LOCB are used instead interpolation.\n")
#        return(rep(oneTraj[!is.na(oneTraj)],length(oneTraj)))
#    }else{}
#    lengthTraj <- length(oneTraj)
#    firstNoNA <- min(which(!is.na(oneTraj)))
#    lastNoNA <- max(which(!is.na(oneTraj)))
#    secondNoNA <- min(which(!is.na(oneTraj[-firstNoNA])))+1
#    penultimateNoNA <- max(which(!is.na(oneTraj[-lastNoNA])))
#
#    a <- ((oneTraj[firstNoNA]-oneTraj[secondNoNA])/(firstNoNA-secondNoNA) + (oneTraj[firstNoNA]-oneTraj[lastNoNA])/(firstNoNA-lastNoNA))/2
#    b <- oneTraj[firstNoNA] - a*firstNoNA
#    indNA <- 1:firstNoNA
#    oneTraj[indNA]<-a*indNA+b
#
#    a <- ((oneTraj[penultimateNoNA]-oneTraj[lastNoNA])/(penultimateNoNA-lastNoNA) + (oneTraj[firstNoNA]-oneTraj[lastNoNA])/(firstNoNA-lastNoNA))/2
#    b <- oneTraj[lastNoNA] - a*lastNoNA
#    indNA <- lastNoNA:lengthTraj
#    oneTraj[indNA]<-a*indNA+b
#    return(oneTraj)
#}

## Bissectrice issue de A :
## formule on http://forums.futura-sciences.com/mathematiques-superieur/39936-equation-dune-bissectrice.html#post2823519
bissectrice <- function(xA,yA,xB,yB,xC,yC){
#    plot(c(xC,xA,xB),c(yC,yA,yB),xlim=c(-5,5),ylim=c(-5,5),type="l")
    dAB <- as.numeric(dist(rbind(c(xA,yA),c(xB,yB))))
    dAC <- as.numeric(dist(rbind(c(xA,yA),c(xC,yC))))

    beta <- (yC-yA)*dAB-(yB-yA)*dAC ## beta peut etre nulle si A B et C sont allignés. Dans ce cas, la bissectrice est la perpendiculaire

    if(abs(beta)<1e-15){
        a <- (yA-yC)/(xA-xC)
    }else{
        a <- -((xC-xA)*dAB-(xB-xA)*dAC)/beta
    }
    b <- yA - a*xA
#    lines(c(-100,100),c(-100,100)*a+b,col=2)
    return(list(a=a,b=b))
}


trajImput.bissectrice.beginEnd <- function(oneTraj){
    if(all(is.na(oneTraj))){
        warning("[LongData:trajImput.bissectrice.beginEnd]: There is only NA on this line, impossible to impute\n")
        return(oneTraj)
    }else{}
    if(sum(!is.na(oneTraj))==1){
        warning("[LongData:trajImput.bissectrice.beginEnd]: There is only one non-missing value on this line.
  [LongData:trajImput.bissectrice.beginEnd]: LOCF and LOCB are used instead interpolation.\n")
        return(rep(oneTraj[!is.na(oneTraj)],length(oneTraj)))
    }else{}

    lengthTraj <- length(oneTraj)
    firstNoNA <- min(which(!is.na(oneTraj)))
    lastNoNA <- max(which(!is.na(oneTraj)))
    secondNoNA <- min(which(!is.na(oneTraj[-firstNoNA])))+1
    penultimateNoNA <- max(which(!is.na(oneTraj[-lastNoNA])))

    bissec <- bissectrice(firstNoNA,oneTraj[firstNoNA],
                          lastNoNA,oneTraj[lastNoNA],
                          secondNoNA,oneTraj[secondNoNA])
    indNA <- 1:firstNoNA
    oneTraj[indNA]<-bissec$a*indNA+bissec$b

    bissec <- bissectrice(lastNoNA,oneTraj[lastNoNA],
                          firstNoNA,oneTraj[firstNoNA],
                          penultimateNoNA,oneTraj[penultimateNoNA])
    indNA <- lastNoNA:lengthTraj
    oneTraj[indNA]<-bissec$a*indNA+bissec$b
    return(oneTraj)
}

trajImput.linInterBissectrice <- function(oneTraj){
    return(trajImput.interpoLin.middle(trajImput.bissectrice.beginEnd(oneTraj)))
}


### A terme, il faudrait envisager la création d'une classe varDefinitition qui contiendrait :
### 'varNames','min','max','integer','listOfValue'... Pour l'instant, range suffit !

.trajImputArray <- function(object,method,imputRange,...){
    ### Method without partition
    if(missing(method)){method <- "LI-Bissectrice"}else{}
    if(missing(imputRange)){imputRange <- apply(object,3,rangeNA)}else{}
    object <- switch(EXPR=method,
        "LOCF"={aperm(apply(object,c(1,3),trajImput.LOCF),perm=c(2,1,3))},
        "LOCB"={aperm(apply(object,c(1,3),trajImput.LOCB),perm=c(2,1,3))},
        "LI-Bissectrice"={aperm(apply(object,c(1,3),trajImput.linInterBissectrice),perm=c(2,1,3))},
        "LI-Global"={aperm(apply(object,c(1,3),trajImput.linInterGlobal),perm=c(2,1,3))},
        "LI-Local"={aperm(apply(object,c(1,3),trajImput.linInterLocal),perm=c(2,1,3))},
        "LI-LOCBF"={aperm(apply(object,c(1,3),trajImput.linInterLOCF),perm=c(2,1,3))},
        {warning("[LongData:trajImputMatrix]: Unknow imputation method\n")}
    )
    for(i in 1:dim(object)[3]){
        object[,,i][object[,,i]<imputRange[1,i]] <- imputRange[1,i]
        object[,,i][object[,,i]>imputRange[2,i]] <- imputRange[2,i]
    }
    return(object)
}
setMethod(f="imputation",
    signature=c(object="array",method="ANY"),
    definition=.trajImputArray
)


.trajImputLongData <- function(object,method,imputRange,...){
    ### Method without partition
    if(missing(method)){method <- "LI-Bissectrice"}else{}
    if(missing(imputRange)){imputRange <- apply(object['traj'],3,rangeNA)}else{}
    nameObject<-deparse(substitute(object))
    object@traj <- imputation(object['traj'],method=method,imputRange=imputRange)
    assign(nameObject,object,envir=parent.frame())
    return(invisible())
}
setMethod(f="imputation",
    signature=c(object="LongData",method="ANY"),
    definition=.trajImputLongData
)



.longData.scale <- function(x,center=TRUE,scale=TRUE){
    nameObject<-deparse(substitute(x))
    traj <- x@traj
    if(center){center <- apply(traj,3,meanNA)}else{}
    if(scale){scale <- apply(traj,3,sdNA)}else{}

    for (i in 1:x@dimTraj[3]){
        traj[,,i] <- (traj[,,i]-center[i])/scale[i]
    }
    x@reverse[1,] <- x@reverse[1,] + center*x@reverse[2,]
    x@reverse[2,] <- x@reverse[2,] * scale
    x@traj <- traj
    assign(nameObject,x,envir=parent.frame())
    return(invisible())
}

setMethod(f="scale",
    signature=c(x="LongData"),
    definition=.longData.scale
)


#.longData.scale2 <- function(x,center,scale){
#    traj <- x['traj']
#    if(missing(center)){center <- apply(traj,3,meanNA)}else{}
#    if(missing(scale)){scale <- apply(traj,3,sdNA)}else{}
#    traj <- sweep(traj,3,center)
#    traj <- sweep(traj,3,scale,FUN="/")
#    x@reverse[1,] <- center
#    x@reverse[2,] <- scale
#    x@traj <- traj
#    assign(nameObject,object,envir=parent.frame())
#    return(invisible())
#    x
#}
#setMethod(f="scale2",
#    signature=c(x="LongData"),
#    definition=.longData.scale2
#)


.longData.restaureRealData <- function(object){
    nameObject<-deparse(substitute(object))
    traj <- object@traj

    for (i in 1:object@dimTraj[3]){
        traj[,,i] <- traj[,,i]*object@reverse[2,i] + object@reverse[1,i]
    }
    object@reverse[1,] <- 0
    object@reverse[2,] <- 1
    object@traj <- traj
    assign(nameObject,object,envir=parent.frame())
    return(invisible())
}
setMethod(f="restaureRealData",
    signature=c(object="LongData"),
    definition=.longData.restaureRealData
)

gald <- generateArtificialLongData <- function(
    nbEachClusters=c(50,50,50),time=0:10,varNames=c("V1","V2"),
    functionClusters=list(function(t){c(0,0)},function(t){c(10,10)},function(t){c(10-t,10-t)}),
    functionNoise=function(t){c(rnorm(1,0,3),rnorm(1,0,3))},
    decimal=2,percentOfMissing=0,clusterLD=TRUE
){
    nbClusters <- length(nbEachClusters)
    if(length(functionClusters)==1){functionClusters <- rep(list(functionClusters),nbClusters)}else{}
    if(length(functionNoise)==1){functionNoise <- rep(list(functionNoise),nbClusters)}else{}
    if(length(percentOfMissing)==1){percentOfMissing <- rep(percentOfMissing,nbClusters)}else{}
    nbTime <- length(time)
    nbVar <- length(varNames)
    id <- paste("I-",1:(sum(nbEachClusters)),sep="")
    indivInCluster <- rep(1:nbClusters,times=nbEachClusters)

    traj <- array(NA,dim=c(sum(nbEachClusters),nbTime,nbVar),dimnames=c(id,paste("T",time,sep=""),varNames))
    for (iIndiv in 1:nrow(traj)){
        traj[iIndiv,,] <- t(sapply(time,functionClusters[[indivInCluster[iIndiv]]])+sapply(time,functionNoise[[indivInCluster[iIndiv]]]))
    }
    traj <- round(traj,digit=decimal)

    for (iCluster in 1:nbClusters){
        nbVal <- nbTime*nbEachClusters[iCluster]
        while(sum(is.na(traj[indivInCluster==iCluster,,]))/nbVal < percentOfMissing[iCluster]){
            randL <- floor(runif(1,cumsum(c(0,nbEachClusters))[iCluster]+1,cumsum(nbEachClusters)[iCluster]+1))
            randC <- floor(runif(1,1,nbTime+1))
            randV <- floor(runif(1,1,nbVar+1))
            traj[randL,randC,randV] <- NA
        }
    }
    if(clusterLD){return(as.clusterLongData(traj,idAll=id,time=time,varNames=varNames))}else{return(as.longData(traj,idAll=id,time=time,varNames=varNames))}

}



cat("\n-------------------------------------------------------------------
-------------------------- Class LongData -------------------------
------------------------------- Fin -------------------------------
-------------------------------------------------------------------\n")

