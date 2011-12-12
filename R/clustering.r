### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
########################## Class Clustering ########################
############################## Creation ############################
####################################################################\n")



## calculCriterion <- function(traj,part,imputationMethod="LI-Bissectrice",criterionNames=c("calinski")){#,"test","test2")){
##     result <- list()
##     if("calinski" %in% criterionNames){
##         isNA <- is.na(part['clusters'])
##         clusters <- part['clustersAsInteger'][!isNA]
##         traj <- traj['traj'][!isNA,,,drop=FALSE]
##         if(any(is.na(traj))){traj <- imputation(traj,method=imputationMethod)}else{}
##         traj <- matrix(as.numeric(traj),nrow=nrow(traj))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
##         cls.attr <- cls.attrib(traj,clusters)
##         varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
##         varWithin <- wcls.matrix(traj,clusters,cls.attr$cluster.center)
##         traceBetween <- sum(diag(varBetween))
##         traceWithin <- sum(diag(varWithin))
##         calinski <- traceBetween/traceWithin*(length(clusters)-part['nbClusters'])/(part['nbClusters']-1)
##         if(is.na(calinski)){calinski<-NaN}
##         result <- c(result,calinski=calinski)
##     }else{}
##     if("test" %in% criterionNames){
##         test <- rnorm(1,-100,5)
##         result <- c(result,test=test)
##     }else{}
##     if("test2" %in% criterionNames){
##         test2 <- rnorm(1,10)
##         result <- c(result,test2=test2)
##     }else{}

##     return(result)
## }

calculCriterion <- function(traj,part,imputationMethod="LI-Bissectrice",criterionNames=c("calinski","ray","davies","random")){#,"test","test2")){
    result <- list()
    isNA <- is.na(part['clusters'])

    clusters <- part['clustersAsInteger'][!isNA]
    traj <- traj['traj'][!isNA,,,drop=FALSE]

    n <- length(clusters)
    k <- max(clusters)
    if(any(is.na(traj))){traj <- imputation(traj,method=imputationMethod)}else{}
    traj <- matrix(as.numeric(traj),nrow=nrow(traj))       # Il arrive que traj soit une matrice d'entier, et ca coincerait...

    cls.attr <- cls.attrib(traj,clusters)

    #######################
    ### Calinski & Harabatz
    ###
    ### Selon Krzysztof     : C(k)=tB/tW*(n-1)/(n-k)
    ### Selon Milligan 1985 : C(k)=[tB/(k-1)]/[tW/(n-k)]=tB/tW*(n-k)/(k-1)
    if("calinski" %in% criterionNames){
        varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
        varWithin <- wcls.matrix(traj,clusters,cls.attr$cluster.center)
        traceBetween <- sum(diag(varBetween))
        traceWithin <- sum(diag(varWithin))
        calinski <- traceBetween/traceWithin*(n-k)/(k-1)
        if(is.na(calinski)){calinski<-NaN}
        result <- c(result,calinski=calinski)
    }else{}
    #######################
    ### Ray & Turi
    ###
    ### Intra = moyenne des distances entre un point et son centre
    ### Inter = plus petite distance au carré entre les centres.
    ### R(k)=Intra/Inter
    ### Un "grand" R dénote une mauvaise partition (grand Intra et/ou petit Inter)
    ###
    if("ray" %in% criterionNames){
        rayInter <- min(dist(cls.attr[[2]]))^2
        rayIntra <- 0
        for(j in 1:k){
            distMoyI <- function(x){return(rbind(x,cls.attr[[2]][j,]))}
            rayIntra <- rayIntra + sum(apply(traj[clusters==j,,drop=FALSE],1,distMoyI))
        }
        rayIntra <- rayIntra/n
        ray <- as.numeric(rayIntra/rayInter)
        result <- c(result,ray=ray)
    }else{}
    ## if(nrow(cls.attr[[2]])==partition@nbClusters){
    ##     rayInter <- +Inf
    ##     for(i in 1:partition@nbClusters){
    ##         distTrajI <- function(x){dist(rbind(x,cls.attr[[2]][i,]))}
    ##         rayInter <- min(apply(cls.attr[[2]][-i,,drop=FALSE],1,distTrajI),rayInter)
    ##     }
    ## }else{
    ##     rayInter <- NaN
    ## }

    ## rayIntra <- 0
    ## for (i in 1:nrow(traj)){
    ##   rayIntra <- rayIntra+dist(rbind(traj[i,],cls.attr[[2]][as.integer(clusters[i]),]))^2
    ## }
    ## rayIntra <- rayIntra/nrow(traj)
    ## ray <- as.numeric(rayInter/rayIntra)

    ##################
    ### Davies Bouldin
    ###
    ### Pour chaque cluster j, on définit MoyDistInt(j) une mesure des moyennes des distances internes de j (ou un diametre)
    ### Pour deux clusters j et j', DistExt(j,j') est une distance entre les clusters (exemple distance entre les centres de gravité)
    ### La 'proximite' entre j et j' vaut Proxi(j,j')=MoyDistInt(j)+MoyDistInt(j'))/DistExt(j,j')
    ### Si j et j' sont compacts et bien séparé, alors Proxi(j,j') sera petite.
    ###
    ### Ensuite, pour un cluster j, max(Proxi(j,j') donne sa pire proximité
    ### Au final, Davies and Bouldin est la moyenne des moins bonnes proximités de tous les clusters
    ### Un "grand" D denote une mauvaise partition (des proximités élevées)
    ###

    if("davies" %in% criterionNames){

        clsScat <- cls.scatt.data(traj,as.integer(clusters))
        davies <- as.numeric(clv.Davies.Bouldin(clsScat,"average","average"))
        result <- c(result,davies=davies)
    }else{}

    if("random" %in% criterionNames){
        random <- rnorm(1,10)
        result <- c(result,random=random)
    }else{}

    return(result)
}


#calculCriterionC <- function(traj,part,criterionNames=c("calinski")){#,"test","test2")){
#    criterionValues <- numeric(length(criterionNames))
#    return(.C("kml",traj=as.numeric(traj),nbVar,nbtime,nbId,part,criterionNames,criterionValues))
#}


cat("### Definition ###\n")
.Clustering.validity <- function(object){
    validObject(as(object,"Partition"))
    return(TRUE)
}

# nbCluster : cluster number
# clusterIndex :
setClass(
   Class="Clustering",
   representation=representation(
      percentEachCluster="numeric",
      convergenceTime="numeric",
      multiplicity="numeric",
      criterionNames="character",
      criterionValues="numeric",
      postProba="matrix",
      algorithm="character"
   ),
   contain="Partition",
   prototype=prototype(
      percentEachCluster=numeric(),
      convergenceTime=numeric(),
      multiplicity=numeric(),
      criterionNames=character(),
      criterionValues=numeric(),
      postProba=matrix(,0,0),
      algorithm=character()
   ),
   validity=.Clustering.validity
)


cat("####################################################################
########################## Class Clustering ########################
############################ Constructeur ##########################
####################################################################\n")

clustering <- function(xLongData,yPartition,convergenceTime=0,multiplicity=1,criterionNames=c("calinski","ray","davies","random"),
                       algorithm=c(algo="kmeans",startCond="",imputation="LI-Bissectrice")
){
    cat("*** initialize Clustering ***\n")
    if(missing(xLongData) && missing(yPartition)){
        return(new("Clustering"))
    }else{
        if(missing(xLongData)){stop("[Clustering:initialize]: LongData is missing !")}else{}
        if(missing(yPartition)){stop("[Clustering:initialize]: Partition is missing !")}else{}
        yPartition <- ordered(resizePartition(xLongData,yPartition))
#        if(length(xLongData["idFewNA"])!=length(yPartition["clusters"])){
 #           stop("[Clustering:initialize]: The partition has not the same length than the number of trajectoire")}else{}
        tab <- as.numeric(table(yPartition["clusters"]))
        return(new("Clustering",clusters=yPartition["clusters"],nbClusters=yPartition["nbClusters"],
                   percentEachCluster=tab/sum(tab),convergenceTime=convergenceTime,multiplicity=multiplicity,criterionNames=criterionNames,
                   criterionValues=unlist(calculCriterion(xLongData,yPartition,imputationMethod=algorithm['imputation'],criterionNames)),
                   algorithm=algorithm))
    }
}



cat("\n####################################################################
########################## Test  Clustering ########################
############################# Accesseurs ###########################
####################################################################\n")

cat("### Getteur ###\n")
.clustering.get <- function(x,i,j,drop){
    switch(EXPR=i,
           "clusters"={return(x@clusters)},
           "clustersAsInteger"={return(as.integer(x@clusters))},
           "nbClusters"={return(x@nbClusters)},
           "percentEachCluster"={return(x@percentEachCluster)},
           "convergenceTime"={return(x@convergenceTime)},
           "multiplicity"={return(x@multiplicity)},
           "criterionNames"={return(x@criterionNames)},
           "criterionValues"={return(x@criterionValues)},
           "algorithm"={return(x@algorithm)},
           if(any(x@criterionNames %in% i)){
               return(x@criterionValues[x@criterionNames %in% i])
           }else{
               stop("[Clustering:getteur]: there is not slot '",i,"' in 'Clustering'")
           }
    )
}
setMethod("[","Clustering",.clustering.get)


cat("### Setteur ###\n")
.clustering.set <- function(x,i,j,value){
    switch(EXPR=i,
           "convergenceTime"={x@convergenceTime<-value},
           "multiplicity"={x@multiplicity<-value},
#               "criterionNames"={x@criterionNames<-value},
 #              "criterionValues"={x@criterionValues<-value},
           if(i %in% c("clusters","nbClusters","percentEachCluster","algorithm","criterionNames","criterionValues")){
               stop("[Clustering:setteur]: ",i," is not entend to be change by the user.")
           }else{
               stop("[Clustering:setteur]: ",i," is not a 'Clustering' slot")
           }
    )
    validObject(x)
    return(x)
}
setMethod("[<-","Clustering",.clustering.set)


cat("####################################################################
########################## Class Clustering ########################
############################## Affichage ###########################
####################################################################\n")



cat("### Method : 'show' for yPartition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Clustering.show <- function(object){
    cat("   ~~~ Class : Clustering ~~~ ")
    cat("\n ~ nbClusters         = ",object@nbClusters)
    cat("\n ~ convergenceTime    = ",object@convergenceTime)
    cat("\n ~ multiplicity       = ",object@multiplicity)
    cat("\n ~ percentEachCluster = ",formatC(object@percentEachCluster,digits=2))
    cat("\n ~ criterionNames     = ",object@criterionNames)
    cat("\n ~ criterionValues    = ",formatC(object@criterionValues,digits=2))
    cat("\n ~ algorithm          = ",object@algorithm)
    cat("\n ~ clusters   : [",length(object@clusters),"]",sep="")
    if(length(object@nbClusters)!=0){
        for (iCluster in LETTERS[1:object@nbClusters]){
            toKeep <- iCluster==object@clusters
            cat("\n    ",iCluster," : [",sum(toKeep,na.rm=TRUE),"] ",sep="")
            catShort((1:length(object@clusters))[toKeep & !is.na(toKeep)])
        }
        cat("\n   <NA> : [",sum(is.na(object@clusters)),"] ",sep="")
        catShort((1:length(object@clusters))[is.na(object@clusters)])
        cat("\n")
    }else{
        cat("\n     <empty Partition>\n")
    }
    return(invisible(object))
}
setMethod(f="show",signature="Clustering",definition=.Clustering.show)


cat("\n####################################################################
########################## Class Clustering ########################
############################### Autres #############################
####################################################################\n")



cat("\n--------------------------------------------------------------------
-------------------------- Class Clustering ------------------------
------------------------------- Autres -----------------------------
--------------------------------------------------------------------\n")
