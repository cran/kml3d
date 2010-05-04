cat("####################################################################
######################## Class ListClustering ######################
############################## Creation ############################
####################################################################\n")

cat("### Definition ###\n")

.ListClustering.validity <- function(object){
#    cat("**************** ListClustering ****************")
    return(TRUE)
}

# nbCluster : cluster number
# clusterIndex :
setClass(
   Class="ListClustering",
   representation=representation(
      criterionActif="character",
      criterionPossibles="character",
      initializationMethod="character",
      sorted="logical",
      c2="list",
      c3="list",
      c4="list",
      c5="list",
      c6="list",
      c7="list",
      c8="list",
      c9="list",
      c10="list",
      c11="list",
      c12="list",
      c13="list",
      c14="list",
      c15="list",
      c16="list",
      c17="list",
      c18="list",
      c19="list",
      c20="list",
      c21="list",
      c22="list",
      c23="list",
      c24="list",
      c25="list",
      c26="list"
   ),
   prototype=prototype(
      criterionActif=character(),
      criterionPossibles=character(),
      initializationMethod=character(),
      sorted=logical(),
      c2=list(),
      c3=list(),
      c5=list(),
      c6=list(),
      c7=list(),
      c8=list(),
      c9=list(),
      c10=list(),
      c11=list(),
      c12=list(),
      c13=list(),
      c14=list(),
      c15=list(),
      c16=list(),
      c17=list(),
      c18=list(),
      c19=list(),
      c20=list(),
      c21=list(),
      c22=list(),
      c23=list(),
      c24=list(),
      c25=list(),
      c26=list()
   )
)


cat("####################################################################
######################## Class ListClustering ######################
############################ Constructeur ##########################
####################################################################\n")

listClustering <- function(){#criterionActif=""){
    return(new("ListClustering"))#,criterionActif=""))
}




cat("\n####################################################################
######################## Test  ListClustering ######################
############################# Accesseurs ###########################
####################################################################\n")


# Si on veut rendre [<- utilisable pour partition, il faut modifier ICI
cat("### Setteur ###\n")
.listClustering.set <- function(x,i,j,value){
    switch(EXPR=i,
           "criterionActif"={
               if(length(x@criterionActif)==0){
                   x@criterionActif<-value
               }else{
                   if(x@criterionActif!=value){
                       x@criterionActif<-value
                       x@sorted<-FALSE
                   }else{}
               }
           },
           "initializationMethod"={x@initializationMethod<-value},
           "sorted"={x@sorted<-value},
           "add"={
               if(class(value)!="Clustering"){
                   stop("[ListClustering:setteur]: a ListClustering object shall contain only Clustering object.")
               }else{}
               eval(parse(text=paste('x@c',value['nbClusters'],' <- c(x@c',value['nbClusters'],',list(value))',sep="")))
               x@criterionPossibles <- unique(c(x@criterionPossibles,value['criterionNames']))
               x@sorted <- FALSE
           },
           "clear"={eval(parse(text=paste('x@',value,' <-  list()',sep="")))},
#                if(value=="all"){
 #                   x <- listClustering()#new("ListClustering",criterionActif=x@criterionActif,initializationMethod=x@initializationMethod,sorted=x@sorted)
  #              }else{
#                    eval(parse(text=paste('x@',value,' <-  list()',sep="")))
   #             }
#            },
#            if(i %in% CLUSTER_NAMES){
 #               eval(parse(text=paste('x@',i,' <- ',value,sep="")))
  #          }else{
           if( (i %in% CLUSTER_NAMES)|(i=="criterionPossibles")){
               stop("[ListClustering:setteur]: Direct acces to ",i," is not permited.")
           }else{
               stop("[ListClustering:setteur] ",i," is not a ListClustering slot.")
           }
    )
    validObject(x)
    return(x)
}

setReplaceMethod("[","ListClustering",.listClustering.set)


cat("### Getteur ###\n")
.listClustering.get <- function(x,i,j,drop){
    if(is.numeric(i) & (i<2|i>26)){
        stop("[ListClustering:getteur]: i should be in [2:26]")
    }else{}
    switch(EXPR=i,
           "criterionActif"={return(x@criterionActif)},
           "c2"={return(x@c2)},
           "c3"={return(x@c3)},
           "c4"={return(x@c4)},
           "c5"={return(x@c5)},
           "c6"={return(x@c6)},
           "c7"={return(x@c7)},
           "c8"={return(x@c8)},
           "c9"={return(x@c9)},
           "c10"={return(x@c10)},
           "c11"={return(x@c11)},
           "c12"={return(x@c12)},
           "c13"={return(x@c13)},
           "c14"={return(x@c14)},
           "c15"={return(x@c15)},
           "c16"={return(x@c16)},
           "c17"={return(x@c17)},
           "c18"={return(x@c18)},
           "c19"={return(x@c19)},
           "c20"={return(x@c20)},
           "c21"={return(x@c21)},
           "c22"={return(x@c22)},
           "c23"={return(x@c23)},
           "c24"={return(x@c24)},
           "c25"={return(x@c25)},
           "c26"={return(x@c26)},
           "criterionValues"={
               if(missing(j)){j <- x@criterionActif}else{}
               result <- list()
               for(i in CLUSTER_NAMES){
                   eval(parse(text=paste("listI <- lapply(x@",i,",function(x){x['criterionValues']['",j,"']})",sep="")))
                   if(length(listI)!=0){
                       eval(parse(text=paste("result <- c(result,",i,"=list(listI))",sep="")))
                   }else{}
               }
               return(result)
           },
           "criterionValuesAsMatrix"={
               if(missing(j)){j <- x@criterionActif}else{}
               result <- list()
               for(i in CLUSTER_NAMES){
                   eval(parse(text=paste("listI <- lapply(x@",i,",function(x){x['criterionValues']['",j,"']})",sep="")))
                   if(length(listI)!=0){
                       eval(parse(text=paste("result <- c(result,",i,"=list(listI))",sep="")))
                   }else{}
               }
               lengthList <- max(sapply(result , length))
               return(t(sapply(result , function(x) c(x,rep(NA,lengthList-length(x))))))
           },
           "criterionPossibles"={return(x@criterionPossibles)},
           "initializationMethod"={return(x@initializationMethod)},
           "sorted"={return(x@sorted)},
        #       if(i %in% CLUSTER_NAMES){
         #          eval(parse(text=paste('return(x@',i,')',sep="")))
          #     }else{
           if(i %in% x@criterionPossibles){
               return(x['criterionValues',i])
           }else{
               stop("[ListClustering:getteur]: ",i," is not a ListClustering slot")
           }
               #}
    )
}

setMethod("[","ListClustering",.listClustering.get)


cat("####################################################################
######################## Class ListClustering ######################
############################## Affichage ###########################
####################################################################\n")



cat("### Method : 'show' for yPartition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.ListClustering.show <- function(object){
    cat("\n ~ criterionActif          = ",object@criterionActif)
    cat("\n ~ criterionPossibles      = ",object@criterionPossibles)
    cat("\n ~ initializationMethod    = ",object@initializationMethod)
    cat("\n ~ sorted                  = ",object@sorted)
    cat("\n ~ criterion values (",object@criterionActif,")",sep="")
    allCrit <- object['criterionValues']
    if(length(allCrit)==0){
        cat("\n    <no Clustering>\n")
    }else{
        for(i in 1:length(allCrit)){
            cat("\n    - ",names(allCrit)[i]," : ")
            catShort(unlist(allCrit[[i]]))
        }
        cat("\n")
    }
    return(invisible(object))
}
setMethod(f="show",signature="ListClustering",
    definition=function(object){
        cat("   ~~~ Class: ListClustering ~~~ ")
        .ListClustering.show(object)
    }
)



cat("\n####################################################################
######################## Class ListClustering ######################
############################### Autres #############################
####################################################################\n")

.ListClustering.ordered <- function(x,...){
    nameObject<-deparse(substitute(x))
    criterName <- x['criterionActif']
    decrease <- ifelse(CRITERION_MIN_OR_MAX[criterName]==1,TRUE,FALSE)
    matPermut <- list()

    if(length(x['criterionActif'])==0){stop("ListClustering:ordered]: 'criterionActif' is not define")}else{}
    for(i in 2:26){
        listCriterion <- lapply(x[i],function(x){x['criterionValues'][criterName]})
        if(length(listCriterion)!=0){
            orderCi <- order(unlist(listCriterion),decreasing=decrease,na.last=TRUE)
            eval(parse(text=paste("x@c",i," <- x@c",i,"[orderCi]",sep="")))
            matPermut <- c(matPermut,list(orderCi))
        }
    }
    x@sorted <- TRUE
    assign(nameObject,x,envir=parent.frame())
    lengthList <- max(sapply(matPermut , length))
    return(t(sapply(matPermut , function(x) c(x,rep(NA,lengthList-length(x))))))
#    return(matPermut)
}
setMethod("ordered",signature="ListClustering",definition=.ListClustering.ordered)


.ListClustering.plot <- function(x, criterion=x['criterionActif'],nbCriterion = 100, standardized = TRUE){
    ##    minMax <- criterionMinOrMax(calinski=1,test=-1,test2=1)
    lengthCrit <- length(criterion)
    if(lengthCrit==1 & nbCriterion!=1){

        ## On plot uniquement un critère, une trajectoire par nombre de groupe
        allCrit <- x["criterionValues",criterion]
        if(length(allCrit)!=0){
            lengthList <- max(sapply(allCrit , length))
            allCrit <- sapply(allCrit , function(x) c(x,rep(NA,lengthList-length(x)+1)))
            lengthList <- min(lengthList,nbCriterion)
            matplot(1:lengthList,allCrit[1:lengthList,,drop=FALSE],type="b",lty=1,pch=c(2:10,letters[1:16])[CLUSTER_NAMES %in% dimnames(allCrit)[[2]]],
                    xlab="Rerolling",ylab="",main=criterion)
        }else{
            plot(1,type="n",xlab="Rerolling",ylab="",main=criterion)
        }

    }else{

        if(!identical(x['sorted'],logical())){

            ## On plot plusieurs critères, une ligne par critères, le nombre de groupe en abscisse
            if(!x['sorted'] | !(x['criterionActif']%in%criterion)){warning("[ListCriterion:plot]: the Clustering are unsorted]")}else{}
            matCrit <- matrix(NA,lengthCrit,25,dimnames=list(criterion,CLUSTER_NAMES))
            for(i in 1:lengthCrit){
                allCrit <- sapply(x["criterionValues",criterion[i]] , function(x){result <- x[[1]];names(result)<-NULL;result})
                matCrit[i,CLUSTER_NAMES%in%names(allCrit)] <- allCrit
            }

            if(standardized){
                for(i in 1:lengthCrit){
                    matCrit[i,] <- matCrit[i,]*CRITERION_MIN_OR_MAX[criterion[i]]
                    matCrit[i,] <- matCrit[i,]-min(matCrit[i,],na.rm=TRUE)
                    matCrit[i,] <- matCrit[i,]/max(matCrit[i,],na.rm=TRUE)
                }
                mainTitle <-c("Standardized criterions",paste("Sorted using '",x['criterionActif'],"'",sep=""))
            }else{
                mainTitle <- c("Non standardized criterions",paste("Sorted using '",x['criterionActif'],"'",sep=""))
            }
            xlab <- paste(1:lengthCrit,":",criterion,sep="",collapse=" ; ")
            rangeVal <- range(which(apply(matCrit,2,function(x){any(!is.na(x))})))+1
            matplot(rangeVal[1]:rangeVal[2],t(matCrit[,(rangeVal[1]:rangeVal[2])-1,drop=FALSE]),type="b",main=mainTitle,lty=1,xlab=xlab,ylab="")
        }else{
            plot(1,type="n",xlab="Rerolling",ylab="")
        }
    }
    return(invisible())
}
setMethod("plot",signature=c(x="ListClustering",y="missing"),.ListClustering.plot)


### Attention, si on regroupe en se fixant que sur la partition, on perd les differences liées aux imputations
regroup <- function(object,toOrder=TRUE){
    nameObject<-deparse(substitute(object))
    if(!object['sorted']|toOrder){ordered(object)}else{}
    for (i in 2:26){
        eval(parse(text=paste("listCi <- object['c",i,"']",sep="")))
        j <- length(listCi)
        keep <- rep(TRUE,j)

        while(j>1){
            if(identical(listCi[[j-1]]['clusters'],listCi[[j]]['clusters'])){
                keep[j] <- FALSE
                listCi[[j-1]]['multiplicity'] <- listCi[[j-1]]['multiplicity']+listCi[[j]]['multiplicity']
            }else{}
            j <- j-1
        }
        eval(parse(text=paste("object@c",i," <- listCi[keep]",sep="")))
    }
    assign(nameObject,object,envir=parent.frame())
}
cat("--------------------------------------------------------------------
------------------------ Class ListClustering ----------------------
------------------------------ Creation ----------------------------
--------------------------------------------------------------------\n")
