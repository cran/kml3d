cat("\n####################################################################
######################### Class parLongData ########################
############################# Creation #############################
####################################################################\n")

### Pas de trajectoire totalement vide => maxNA<length(time)

.ParLongData.validity <- function(object){
#    cat("**** validity ParLongData <empty> ****\n")
    return(TRUE)
}

setClass(
    Class="ParLongData",
    representation=representation(
        type="character",
        col="character",
        pch="character",
        pchPeriod="numeric",
        cex="numeric",
        xlab="character",
        ylab="character"
    ),
    prototype=prototype(
        type=character(),
        col=character(),
        pch=character(),
        pchPeriod=numeric(),
        cex=numeric(),
        xlab=character(),
        ylab=character()
    ),
    validity=.ParLongData.validity
)

cat("\n###################################################################
######################## Class parLongData ########################
########################### Constructeur ##########################
###################################################################\n")

parLongData <- function(type,col,pch,pchPeriod,cex,xlab,ylab){
    if(is.numeric(col)){col<-palette()[col]}
    new("ParLongData",type=type,col=col,pch=pch,pchPeriod=pchPeriod,cex=cex,xlab=xlab,ylab=ylab)
}

parTraj <- function(type="l",col="black",pch="1",pchPeriod=0,cex=1,xlab="Time",ylab=""){
    parLongData(type=type,col=col,pch=pch,pchPeriod=pchPeriod,cex=cex,xlab=xlab,ylab=ylab)
}

parMean <- function(type="b",col="clusters",pch="letters",pchPeriod=1,cex=1.2,xlab="Time",ylab=""){
    parLongData(type=type,col=col,pch=pch,pchPeriod=pchPeriod,cex=cex,xlab=xlab,ylab=ylab)
}


cat("### Method : 'show' for ParLongData ###\n") # Si on ajoute un titre a traj, on pourra afficher 'associate traj ='
.ParLongData.show <- function(object){
    cat("   ~~~ Class: ParLongData ~~~ ")
    cat("\n ~ type       : ",object@type)
    cat("\n ~ col        : [",length(object@col),"] ",sep="");catShort(object@col)
    cat("\n ~ pch        : [",length(object@pch),"] ",sep="");catShort(object@pch)
    cat("\n ~ pchPeriod  : ",object@pchPeriod)
    cat("\n ~ cex        : ",object@cex)
    cat("\n ~ xlab       : ",object@xlab)
    cat("\n ~ ylab       : ",object@ylab,"\n")
    return(invisible(object))
}
setMethod(f="show",signature="ParLongData",definition=.ParLongData.show)



cat("### Getteur ###\n")
setMethod("[","ParLongData",
    function(x,i,j,drop){
        switch(EXPR=i,
            "type"={return(x@type)},
            "col"={return(x@col)},
            "pch"={return(x@pch)},
            "pchPeriod"={return(x@pchPeriod)},
            "cex"={return(x@cex)},
            "xlab"={return(x@xlab)},
            "ylab"={return(x@ylab)},
            stop("[ParLongData:get]: there is not such a slot in ParLongData")
        )
    }
)


cat("### Setteur ###\n")
setMethod("[<-","ParLongData",
    function(x,i,j,value){
        switch(EXPR=i,
            "type"={x@type<-value},
            "col"={x@col<-value},
            "pch"={x@pch<-value},
            "pchPeriod"={x@pchPeriod<-value},
            "cex"={x@cex<-value},
            "xlab"={x@xlab<-value},
            "ylab"={x@ylab<-value},
            stop("[ParLongData:set]: there is not such a slot in ParLongData")
        )
        validObject(x)
        return(x)
    }
)

### Prépare un ParLongData en fonction d'une partition :
###  - Si col="clusters", crée un vecteur couleur pour chaque individu
ParLongData.Partition.expand <- function(xParLongData,y){
    col <- xParLongData['col']
    if(identical(col,"clusters")){
        col <- rainbow(y['nbClusters'])[y['clustersAsInteger']]
    }else{
        if(length(col)==1){
            col <- rep(col,length(y['clusters']))
        }else{}
    }
    xParLongData['col'] <- col

    if(length(xParLongData['pch'])==1){
        xParLongData['pch'] <- switch(xParLongData['pch'],
                                      "letters"=LETTERS[1:y['nbClusters']][y['clustersAsInteger']],
                                      "symbols"=as.character(1:y['nbClusters'])[y['clustersAsInteger']],
                                      xParLongData['pch'])
    }else{}
return(xParLongData)
}
setMethod("expandParLongData",signature=c(xParLongData="ParLongData",y="Partition"),def=ParLongData.Partition.expand)


### Prépare un ParLongData en fonction d'un nombre de clusters :
###  - Si col="clusters", crée un vecteur couleur pour chaque clusters
###  - Si pch="letters" ou "symbols", crée un vecteur pch pour chauqe clusters
ParLongData.nbClusters.expand <- function(xParLongData,y){
    col <- xParLongData['col']
    if(identical(col,"clusters")){
        col <- rainbow(y)
    }else{
        if(length(col)==1){
            col <- rep(col,y)
        }else{}
    }
    xParLongData['col'] <- col

    if(length(xParLongData['pch'])==1){
        xParLongData['pch'] <- switch(xParLongData['pch'],
                                      "letters"=LETTERS[1:y],
                                      "symbols"=as.character(1:y),
                                      xParLongData['pch'])
    }else{}

    return(xParLongData)
}
setMethod("expandParLongData",signature=c(xParLongData="ParLongData",y="numeric"),def=ParLongData.nbClusters.expand)


cat("\n--------------------------------------------------------------------
----------------------- Fin Test ParLongData -----------------------
--------------------------------------------------------------------\n")
