.ParKml.validity <- function(object){
#    cat("**** validity ParKml <empty> ****\n")
    return(TRUE)
}

setClass(
    Class="ParKml",
    representation=representation(
        saveFreq="numeric",
        maxIt="numeric",
        imputationMethod="character",
        distanceName="character",
        power="numeric",
        distance="function",
        centerMethod="function",
        startingCond="character",
        distanceStartingCond="function",
        nbCriterion="numeric"
    ),
    prototype=prototype(
        saveFreq=numeric(),
        maxIt=numeric(),
        imputationMethod=character(),
        distanceName=character(),
        power=numeric(),
        distance=function(){},
        centerMethod=function(){},
        startingCond=character(),
        distanceStartingCond=function(){},
        nbCriterion=numeric()
    ),
    validity=.ParKml.validity
)

parKml <- function(saveFreq=100,maxIt=200,imputationMethod="LI-Bissectrice",
                   distanceName="euclidean",power=2,distance=function(){},
                   centerMethod=meanNA,startingCond="allMethods",distanceStartingCond=function(x,y)dist(rbind(x,y)),nbCriterion=100){
    if(distanceName %in%DISTANCE_METHODS){distance <- function(x,y){dist3d(x,y,method=distanceName,power=power)}}else{}
    new("ParKml",saveFreq=saveFreq,maxIt=maxIt,imputationMethod=imputationMethod,
        distanceName=distanceName,power=power,distance=distance,
        centerMethod=centerMethod,startingCond=startingCond,distanceStartingCond=distanceStartingCond,nbCriterion=nbCriterion)
}


setMethod("[","ParKml",
    function(x,i,j,drop){
        switch(EXPR=i,
            "saveFreq"={return(x@saveFreq)},
            "maxIt"={return(x@maxIt)},
            "imputationMethod"={return(x@imputationMethod)},
            "distanceName"={return(x@distanceName)},
            "power"={return(x@power)},
            "distance"={return(x@distance)},
            "centerMethod"={return(x@centerMethod)},
            "startingCond"={return(x@startingCond)},
            "distanceStartingCond"={return(x@distanceStartingCond)},
            "nbCriterion"={return(x@nbCriterion)},
            stop("[ParKml:get]: there is not such a slot in ParWindows")
        )
    }
)

cat("### Method : 'show' for ParKml ###\n")
.ParKml.show <- function(object){
    cat("   ~~~ Class: ParKml ~~~ ")
    cat("\n ~ saveFreq             :",object@saveFreq)
    cat("\n ~ maxIt                :",object@maxIt)
    cat("\n ~ imputationMethod     :",object@imputationMethod)
    cat("\n ~ distanceName         :",object@distanceName)
    cat("\n ~ power                :",object@power)
    cat("\n ~ distance             : ");print(object@distance)
    cat(" ~ centerMethod         : ");print(object@centerMethod)
    cat(" ~ startingCond         :",object@startingCond)
    cat("\n ~ distanceStartingCond : ");print(object@distanceStartingCond)
    cat("\n ~ nbCriterion          :",object@nbCriterion,"\n")
    return(invisible(object))
}
setMethod(f="show",signature="ParKml",definition=.ParKml.show)
