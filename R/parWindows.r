cat("\n####################################################################
######################### Class parWindows #########################
############################# Creation #############################
####################################################################\n")

### Pas de trajectoire totalement vide => maxNA<length(time)

.ParWindows.validity <- function(object){
#    cat("**** validity ParWindows <empty> ****\n")
    return(TRUE)
}

setClass(
    Class="ParWindows",
    representation=representation(
        nbCol="numeric",
        nbRow="numeric",
        addLegend="logical",
        closeScreen="logical",
        screenMatrix="matrix"
    ),
    prototype=prototype(
        nbCol=numeric(),
        nbRow=numeric(),
        addLegend=logical(),
        closeScreen=logical(),
        screenMatrix=matrix(,0,0)
    ),
    validity=.ParWindows.validity
)

cat("\n###################################################################
######################### Class parWindows ########################
########################### Constructeur ##########################
###################################################################\n")

parWindows <- function(nbCol,nbRow,addLegend,closeScreen){
    xDivision <- seq.int(0, 1, length.out = nbCol + 1)
    yDivision <- seq.int(ifelse(addLegend,0.9,1), 0,length.out = nbRow + 1)

    screenMatrix <- matrix(c(rep.int(xDivision[-(nbCol + 1)], nbRow),
                     rep.int(xDivision[-1],nbRow),
                     rep.int(yDivision[-1], rep.int(nbCol, nbRow)),
                     rep.int(yDivision[-(nbRow + 1)], rep.int(nbCol, nbRow))
                     ),ncol = 4)
    if(addLegend){screenMatrix <- rbind(screenMatrix,c(0,1,0.5,1))}else{}
    return(new("ParWindows",nbCol=nbCol,nbRow=nbRow,addLegend=addLegend,closeScreen=closeScreen,screenMatrix=screenMatrix))
}


windowsCut <- function(x,addLegend=TRUE,closeScreen=TRUE){
    if(length(x)==1){
        nbRow <- ceiling(sqrt(x))
        nbCol <- ceiling(x/nbRow)
    }else{
        nbRow <- x[1]
        nbCol <- x[2]
    }
    return(parWindows(nbCol=nbCol,nbRow=nbRow,addLegend=addLegend,closeScreen=closeScreen))
}

cat("### Method : 'show' for ParWindows ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.ParWindows.show <- function(object){
    cat("   ~~~ Class: ParWindows ~~~ ")
    cat("\n ~ nbCol       :",object@nbCol)
    cat("\n ~ nbRow       :",object@nbRow)
    cat("\n ~ addLegend   :",object@addLegend)
    cat("\n ~ closeScreen :",object@closeScreen)
    cat("\n ~ screenMatrix\n")
    print(object@screenMatrix)
    return(invisible(object))
}
setMethod(f="show",signature="ParWindows",definition=.ParWindows.show)

cat("### Getteur ###\n")
setMethod("[","ParWindows",
    function(x,i,j,drop){
        switch(EXPR=i,
            "nbCol"={return(x@nbCol)},
            "nbRow"={return(x@nbRow)},
            "addLegend"={return(x@addLegend)},
            "closeScreen"={return(x@closeScreen)},
            "screenMatrix"={return(x@screenMatrix)},
            stop("[ParWindows:get]: there is not such a slot in ParWindows")
        )
    }
)

cat("### Setteur ###\n")
setMethod("[<-","ParWindows",
    function(x,i,j,value){
        switch(EXPR=i,
            "nbCol"={x@nbCol<-value},
            "nbRow"={x@nbRow<-value},
            "addLegend"={x@addLegend<-value},
            "closeScreen"={x@closeScreen<-value},
            "screenMatrix"={x@screenMatrix<-value},
            stop("[ParWindows:set]: there is not such a slot in ParWindows")
        )
        validObject(x)
        return(x)
    }
)


#cat("### Setteur ###\n")
#setMethod("[<-","ParWindows",
#    function(x,i,j,value){
#        switch(EXPR=i,
#            "type"={x@type<-value},
#            "col"={x@col<-value},
#            "pch"={x@pch<-value},
#            "pchPeriod"={x@pchPeriod<-value},
#            "cex"={x@cex<-value},
#            "xlab"={x@xlab<-value},
#            "ylab"={x@ylab<-value},
#            stop("[ParWindows:set]: there is not such a slot in ParWindows")
#        )
#        validObject(x)
#        return(x)
#    }
#)

cat("\n--------------------------------------------------------------------
-------------------------- Fin ParWindows --------------------------
--------------------------------------------------------------------\n")
