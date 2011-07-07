cat("\n####################################################################
############################## Function ############################
####################################################################")

cat("\n### Functions accepting NA ###")
meanNA <- function(x){mean(x,na.rm=TRUE)}
medianNA <- function(x){median(x,na.rm=TRUE)}

sdNA   <- function(x){sd(c(x),na.rm=TRUE)}
varNA   <- function(x){var(c(x),na.rm=TRUE)}
rangeNA   <- function(x){range(x,na.rm=TRUE)}

which.minNA <- function(x){
  y <- which.min(x)
  if(length(y)==0){y<-NA}
  return(y)
}

### TRUE for Truly NA : false for NaN
is.tna <- function(x){
    if(length(x)==0){
        return(TRUE)
    }else{
        if(is.list(x)){x <- unlist(x)}else{}
        return(is.na(x)&!is.nan(x))
    }
}


### Printing long line shortening them
catShort <- function(x){
    if(length(x)<=10){
        cat(x)
    }else{
        cat(x[1:10],"...")
    }
}

NAtrunc <- function(x) x[1:max(which(!is.na(x)))]

cat("\n### Constantes ###\n")
MAX_CLUSTERS <- 26
CLUSTER_NAMES <- paste("c",2:MAX_CLUSTERS,sep="")
CRITERION_MIN_OR_MAX<- c(calinski=1,test=1,test2=-1)
DISTANCE_METHODS <- c("manhattan", "euclidean", "minkowski", "maximum", "canberra", "binary")
CHOICE_STYLE <- list(
    typeTraj=c("l","l","n"),
    colTraj=c("clusters","black","black"),
    typeMean=c("b","b","b","b","l","l","n"),
    colMean=c("clusters","black","clusters","black","clusters","black","black"),
    pchMean=c("letters","letters","symbols","symbols","letters","letters","letters")
)


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++ Fin Function ++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

