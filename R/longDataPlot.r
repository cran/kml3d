
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++ Class LongData +++++++++++++++++++++++++
+++++++++++++++++++++++++++++++ plot ++++++++++++++++++++++++++++++
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

### Possibilité de passer a paramTraj une liste de parTraj ? Puis de passer une liste a un élément
### qui serait dupliquée ?

cat("### Method: 'plot' pour LongData et Partition ###\n")

### Quand la taille d'une Partition correspond a idFewNA, cette fonction ne fait rien.
### Quand la taille d'une Partition correspond a idAll, les valeurs de la partition correspondant
### aux individus présent dans idAll mais plus dans idFewNA (donc les individus qui ont trop
### de valeurs manquantes et qui ont été retiré de LongData@traj) sont supprimées.
### Ainsi, la taille de la Partition cadre avec les traj réelle et non avec les traj initiales
resizePartition <- function(xLongData,yPartition){
    if(length(yPartition['clusters'])!=xLongData['dimTraj'][1]){
        if(length(yPartition['clusters'])==length(xLongData['idAll'])){
            yPartition['clusters'] <- yPartition['clusters'][xLongData['idAll']%in%xLongData['idFewNA']]
        }else{
            stop("[LongData:LongData.Partition.plot]: the Partition and the LongData do not have the same number of individual")
        }
    }
    return(yPartition)
}


### Exclut de la matrice des trajectoires moyennes les valeurs des points qui ne doivent pas être imprimé.
### trajMeanPoint est la matrice des trajectoires moyenne.
### pchPeriod précise la période d'apparition d'un point sur une courbe.
###   - si pchPeriod=0, tous les points sont plotés
###   - si pchPeriod>1, les points sont plotés tous les 'nbClusters*pchPeriod', avec un décalage initial
calculTrajMeanPoint <- function(trajMeanPoint,nbClusters,nbTime,pchPeriod){
    period <- nbClusters*pchPeriod
    if(period>=1){
        if(period<nbTime){
            for(i in 1:nbClusters){
                toKeep <- seq(from=pchPeriod*(i-1)%%nbTime+1,to=nbTime,by=period)
                trajMeanPoint[i,(1:nbTime)[c(-toKeep)],]<-NA
            }
        }else{
            toKeep <- round(seq(from=1,to=nbTime,length.out=nbClusters))
            for(i in 1:nbClusters){
                trajMeanPoint[i,-toKeep[i],] <- NA
            }
        }
    }else{}
    return(trajMeanPoint)
}

legendCol <- function(nbVar){
    if(nbVar<6){return(nbVar)}else{
        if(nbVar %in% c(6)){return(3)}else{
            if(nbVar %in% c(7,8,11,12)){return(4)}else{
                if(nbVar %in% c(9,10,13:15)){return(5)}else{
                    if(nbVar %in% c(16:18,21:24)){return(6)}else{
                        return(7)}}}}}
}


.longData.plot <- function(x,paramTraj=parTraj(),paramWindows=windowsCut(x['nbVar'],addLegend=FALSE),nbSample=200){
    ## Duplication de la couleur (équivalent a expandParLongData)
    if(identical(paramTraj['col'],'clusters')){paramTraj['col']<-'black'}else{}
    if(length(paramTraj['col'])==1){paramTraj['col'] <- rep(paramTraj['col'],x['nbIdFewNA'])}else{}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    ## Calcul du layout
    listScreen<-split.screen(paramWindows['screenMatrix'])

    for (i in 1:x['nbVar']){
        screen(listScreen[i])
        par(mar=c(3,4,2,1))
        matplot(x['time'],t(x['traj'][toKeep,,i]),type=paramTraj['type'],col=paramTraj['col'][toKeep],lty=1,
                pch=paramTraj['pch'],cex=paramTraj['cex'],xlab="",ylab=x['varNames'][i])
    }
    if(paramWindows['closeScreen']){
        close.screen(listScreen)
        return(invisible())
    }else{
        return(listScreen)
    }
}
setMethod("plot",signature=c(x="LongData",y="missing"),def=.longData.plot)

.LongData.Partition.plot <- function(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200){
    ## ############################# Preparation ############################# ##
    nbVar <- x['nbVar']
    nbTime <- x['nbTime']
    traj <- x['traj']

    ## Gestion de la partition
    ### TROP COMPLIQUE : Il faut modifier calculTrajMean, mais aussi t(trajMean[,,i])
    #if(missing(y)){y <- partition(clusters=rep("A",length(x["idFewNA"])),nbClusters=1)}else{}

    nbClusters <- y['nbClusters']

    ## Vérification que la partition est de la bonne taille.
    y <- resizePartition(x,y)
    part <- factor(y['clusters'],levels=LETTERS[1:nbClusters])

    ## Prépare les ParLongData en fonction de la partition :
    paramTraj <- expandParLongData(paramTraj,y)
    paramMean <- expandParLongData(paramMean,nbClusters)

    ## si besoin, calcul des trajectoires moyennes et des points a placer
    if(paramMean['type']!="n"){
        trajMean <- calculTrajMean(traj,part)#,nbClusters,nbTime,nbVar)
        if( paramMean['type']%in%c("p","b","o")){# & !identical(paramMean['pch'],NA) ){
            trajMeanPoint <- calculTrajMeanPoint(trajMean,nbClusters,nbTime,paramMean['pchPeriod'])
        }else{}
    }else{}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    ## ############################# Tracé ############################# ##

    ## Calcul du layout
    listScreen<-split.screen(paramWindows['screenMatrix'])

    for (i in 1:nbVar){
        screen(listScreen[i])
        par(mar=c(3,4,2,1))
        matplot(x['time'],t(traj[toKeep,,i]),type=paramTraj['type'],col=paramTraj['col'][toKeep],lty=1,
                pch=paramTraj['pch'],cex=paramTraj['cex'],xlab="",ylab=x['varNames'][i])

        ## Tracé des moyennes avec ou sans symbols
        if(paramMean['type'] %in% c("l","b","c","o","h","s","S")){
            matlines(x['time'],t(trajMean[,,i]),col=1,lwd=8,lty=1,type="l")
            matlines(x['time'],t(trajMean[,,i]),col=paramMean['col'],lwd=4,lty=1,type="l")
        }else{}

        ## Tracé des points
        if(paramMean['type'] %in% c("b","c")){
            par(bg="white")
            matlines(x['time'],t(trajMeanPoint[,,i]),col=0,type="p",pch=19,cex=paramMean['cex']*2.5)
        }else{}
        if(paramMean['type'] %in% c("p","b","o")){
            matlines(x['time'],t(trajMeanPoint[,,i]),col=paramMean['col'],type="p",pch=paramMean['pch'],cex=paramMean['cex'])
        }else{}
    }

    ## Tracé éventuelle de la legend
    if(paramWindows['addLegend']){
        percent <- paste(": ",formatC(table(part)/length(part)*100,digit=3),"%",sep="")
        screen(listScreen[length(listScreen)],FALSE)
        legend(grconvertX(0.5,'npc'), grconvertY(1.05,'npc')^2,percent,lty=1,col=paramMean['col'],pch=paramMean['pch'],
               ncol=legendCol(nbClusters),xpd=NA,xjust=0.5,yjust=0.5,inset=-0.1)
    }
    close.screen(listScreen)
}
setMethod("plot",signature=c(x="LongData",y="Partition"),def=.LongData.Partition.plot)
#plot(ld3,p3g)





cat("###################################################################
########################## Class LongData #########################
############################## plot3d #############################
###################################################################\n")


### allVarNames contient les nom des variables presentent dans un LongData.
### variable contient soit un nom de variable, soit le numero d'une variable.
### La fonction retourne le nom ET le numéro de la variable
varNumAndName <- function(variable,allVarNames){
    if(class(variable)=="character"){
        varName <- variable
        varNum <- c(1:length(allVarNames))[allVarNames %in% varName]
        if(length(varNum)==0){stop("[LongData:plod3d]: 'variable' is not a correct variable name
  [LongData:plod3d]: variable=",varName," is not in allVarNames=",allVarNames)}else{}
    }else{
        varNum <- variable
        varName <- allVarNames[varNum]
    }
    return(list(num=varNum,name=varName))
}



### Affichage les axes et redimentionne des graphes 3D
### varNames1 et varNames2 sont les noms des variables qui sont représentées
adjustGraph3d <- function(varName1,varName2){
    axes3d(c('x','y','z'))
    title3d(,,"Time",varName1,varName2)
    box3d()
    aspect3d(c(2,1,1))
    rgl.viewpoint(0,-90,zoom=1.2)
}

.LongData.plot3d <- function(x,y,varY=1,varZ=2,paramTraj=parTraj(),nbSample=200){
    ## Duplication de la couleur (équivalent a expandParLongData)
    if(paramTraj['col']=='clusters'){paramTraj['col']<-'black'}else{}
    if(length(paramTraj['col'])==1){paramTraj['col'] <- rep(paramTraj['col'],x['nbIdFewNA'])}else{color<-paramTraj['col']}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    varY <- varNumAndName(varY,x['varNames'])
    varZ <- varNumAndName(varZ,x['varNames'])

    traj3d <- array(c(rep(x['time'],each=nbSample),x['traj'][toKeep,,c(varY$num,varZ$num)]), dim = c(nbSample,x['nbTime'],3))

    open3d()
    if(paramTraj['type']!="n"){
        for (i in 1:nbSample){lines3d(traj3d[i, , ], col = paramTraj['col'][toKeep[i]],lwd=1)}
    }else{}

     adjustGraph3d(varY$name,varZ$name)
}
setMethod("plot3d",signature=c("LongData","missing"),.LongData.plot3d)



.LongData.Partition.plot3d <- function(x,y,varY=1,varZ=2,paramTraj=parTraj(type="n"),paramMean=parMean(),nbSample=200){
    ## ############################# Preparation ############################# ##
    nbVar <- x['nbVar']
    nbTime <- x['nbTime']
    nbClusters <- y['nbClusters']

    ## Gestion de la partition
    ## TROP compliqué, cf plot3d
    ## if(missing(y)){y <- partition(clusters=rep("A",length(x["idFewNA"])),nbClusters=1)}else{}

    ## Vérification que la partition est de la bonne taille.
    y <- resizePartition(x,y)
    part <- factor(y['clusters'],levels=LETTERS[1:nbClusters])

    ## Prépare les ParLongData en fonction de la partition :
    paramTraj <- expandParLongData(paramTraj,y)
    paramMean <- expandParLongData(paramMean,nbClusters)

    ## si besoin, calcul des trajectoires moyennes et des points a placer
    if(paramMean['type']!="n"){
        trajMean <- calculTrajMean(x['traj'],part)#,nbClusters,nbTime,nbVar)
        if( paramMean['type']%in%c("p","b","o")){# & !identical(paramMean['pch'],NA) ){
            trajMeanPoint <- calculTrajMeanPoint(trajMean,nbClusters,nbTime,paramMean['pchPeriod'])
        }else{}
    }else{}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    varY <- varNumAndName(varY,x['varNames'])
    varZ <- varNumAndName(varZ,x['varNames'])

    ## ############################# Tracé ############################# ##
    open3d()

    ## matrice des trajectoires
    traj3d <- array(c(rep(x['time'],each=nbSample),x['traj'][toKeep,,c(varY$num,varZ$num)]), dim = c(nbSample,nbTime,3))
    if(paramTraj['type']!="n"){
        for (i in 1:nbSample){lines3d(traj3d[i, , ], col = paramTraj['col'][toKeep[i]],lwd=1)}
    }else{}

    ## matrice des moyennes
    mean3d <- array(c(rep(x['time'],each=nbClusters),trajMean[,,c(varY$num,varZ$num)]), dim = c(nbClusters,nbTime,3))
    if(paramMean['type']!="n"){
        for (i in 1:nbClusters){
            if(!all(is.na(mean3d[i,,-1]))){lines3d(mean3d[i, , ], col = paramMean['col'][i],lwd=5)}else{}
        }
    }else{}

    adjustGraph3d(varY$name,varZ$name)
}
setMethod("plot3d",signature=c("LongData","Partition"),.LongData.Partition.plot3d)






cat("###################################################################
########################## Class LongData #########################
############################# plot3dPdf ###########################
###################################################################\n")

misc3dPlan <- function(A,Ax,Ay){
   v1 <- matrix(c(A,Ax),ncol=3,byrow=TRUE)
   v2 <- matrix(c(Ax,Ay),ncol=3,byrow=TRUE)
   v3 <- matrix(c(Ay,Ax+Ay-A),ncol=3,byrow=TRUE)
   return(data.frame(v1=v1,v2=v2,v3=v3))
}


misc3dPave <- function(A,Ax,Ay,Az,color="black",alpha=1){
    Axy <- Ax+Ay-A
    Axz <- Ax+Az-A
    Ayz <- Ay+Az-A
    dataV <- cbind(rbind(misc3dPlan(A,Ax,Ay),misc3dPlan(A,Ax,Az),misc3dPlan(A,Ay,Az),
         misc3dPlan(Az,Axz,Ayz),misc3dPlan(Ax,Axz,Axy),misc3dPlan(Ay,Axy,Ayz)),color=color,alpha=alpha)
    return(dataV)
}

misc3dLine <- function(A,B,color="black",alpha=0.8,lwd=0.05){
    misc3dPave(A-c(0,0,lwd),B-c(0,0,lwd),A-c(0,lwd,0),A+c(0,lwd,0),color=color,alpha=alpha)
}

misc3dLines <- function(x,y,z,color="black",alpha=0.8,lwd=0.05){
    dataV <- misc3dLine(A=c(x[1],y[1],z[1]),B=c(x[2],y[2],z[2]),color=color,alpha=alpha,lwd=lwd)
    for(i in 3:length(x)){
	dataV <- rbind(dataV,misc3dLine(A=c(x[i-1],y[i-1],z[i-1]),B=c(x[i],y[i],z[i]),color=color,alpha=alpha,lwd=lwd))
    }
    return(dataV)
}


.LongData.Partition.plot3dPdf <- function(x,y,varY=1,varZ=2){
    ## ############################# Preparation ############################# ##
    time <- x['time']
    nbClusters <- y['nbClusters']

    ## Vérification que la partition est de la bonne taille.
    y <- resizePartition(x,y)
    part <- factor(y['clusters'],levels=LETTERS[1:nbClusters])

    ## Calcul des trajectoires moyennes et des points a placer
    trajMean <- calculTrajMean(x['traj'],part)#,nbClusters,nbTime,nbVar)

    ## Gestion du choix des variables
    varY <- varNumAndName(varY,x['varNames'])
    varZ <- varNumAndName(varZ,x['varNames'])

    ## Normalisation
    xx <- (time-min(time))/(max(time)-min(time))
    yy <- (trajMean[,,varY$num]-min(trajMean[,,varY$num]))/(max(trajMean[,,varY$num])-min(trajMean[,,varY$num]))*0.5
    zz <- (trajMean[,,varZ$num]-min(trajMean[,,varZ$num]))/(max(trajMean[,,varZ$num])-min(trajMean[,,varZ$num]))*0.5

    ## ############################# Tracé ############################# ##

    ## Cadre
    triangles <- rbind(
        misc3dPave(A=c(0,0,0),Ax=c(1,0,0),Ay=c(0,0.5,0),Az=c(0,0,0.5),alpha=0.02),
        misc3dLine(A=c(0,0,0),B=c(1,0,0),lwd=0.01),
        misc3dLine(A=c(0,0,0),B=c(0,0.5,0),lwd=0.01),
        misc3dLine(A=c(0,0,0),B=c(0,0,0.5),lwd=0.01),

        misc3dLine(A=c(0,0.5,-0.04),B=c(0,0.56,0.04),lwd=0.005),
        misc3dLine(A=c(0,0.5,0.04),B=c(0,0.53,0),lwd=0.005),

        misc3dLine(A=c(0,-0.03,0.60),B=c(0,0.03,0.60),lwd=0.005),
        misc3dLine(A=c(0,0.03,0.60),B=c(0,-0.03,0.53),lwd=0.005),
        misc3dLine(A=c(0,-0.03,0.53),B=c(0,0.03,0.53),lwd=0.005)
    )

    ## Tracé des moyennes
    colorMean <- rainbow(dim(trajMean)[1])
    for(i in 1:dim(trajMean)[1]){triangles <- rbind(triangles,misc3dLines(xx,yy[i,],zz[i,],color=colorMean[i],alpha=0.8,lwd=0.01))}
    return(makeTriangles(v1=as.matrix(triangles[,1:3]),v2=as.matrix(triangles[,4:6]),v3=as.matrix(triangles[,7:9]),
                         alpha=triangles$alpha,color=triangles$color))
}
setMethod("plot3dPdf",signature=c("LongData","Partition"),.LongData.Partition.plot3dPdf)



saveTrianglesAsASY <- function(scene, filename = "scene.asy") {
    scene <- misc3d:::colorScene(scene)
    triangles <- misc3d:::canonicalizeAndMergeScene(scene, "color",
                                                    "color2", "alpha",
                                                    "col.mesh", "fill",
                                                    "smooth")
    ve <- misc3d:::t2ve(triangles)
    f <- file(filename, open = "w")
    on.exit(close(f))

    ## write out header information and vertices
    cat("//generated by saveTrianglesAsASY\n\n",
        "import three;\n\n",
        "size(20cm);\n\n",
        "//currentprojection=perspective(250,-250,250);\n",
        "currentlight=Viewport;\n\n",
        "typedef path3[] trimesh;\n\n",
        "// Vertices\n",
        "triple[] V;\n",
        sep = "", file = f)

    nv <- ncol(ve$vb)
    x <- ve$vb[1,]
    y <- ve$vb[2,]
    z <- ve$vb[3,]
    for (i in 1 : nv)
        cat(sprintf("V[%d] = (%f, %f, %f);\n",
                    i - 1, x[i], y[i], z[i]), file = f)

    ## write out the faces
    cat("\n",
        "guide3 triface_(int i, int j, int k) {\n",
        "  guide3 gh; gh=V[i-1]--V[j-1]--V[k-1]--cycle;\n",
        "  return gh;\n",
        "};\n\n",
        "// Faces\n",
        "trimesh F;\n",
        sep = "", file = f)

    nf <- ncol(ve$ib)
    v1 <- ve$ib[1,]
    v2 <- ve$ib[2,]
    v3 <- ve$ib[3,]
    for (i in 1 : nf)
        cat(sprintf("F[%d] = triface_(%d, %d, %d);\n",
                    i - 1, v1[i], v2[i], v3[i]), file = f)

    ## write out color and transparency values
    cat("\n",
        "// Colors\n",
        "material M[];\n",
        sep = "", file = f)

    cols <- col2rgb(triangles$color)
    alpha <- triangles$alpha
    r <- cols[1,]
    g <- cols[2,]
    b <- cols[3,]
    if (any(alpha < 1))
        for (i in 1 : nf)
            cat(sprintf("M[%d] = rgb(%f, %f, %f) + opacity(%f);\n",
                        i - 1, r[i], g[i], b[i], alpha[i]),
                file = f)
    else
        for (i in 1 : nf)
            cat(sprintf("M[%d] = rgb(%f, %f, %f);\n",
                        i - 1, r[i], g[i], b[i]),
                file = f)

    cat("\ndraw(surface(F), M);\n", file = f)
    invisible(NULL)
}


makeLatexFile <- function(filename="main.tex",asyToInclude="scene+0.prc"){
    f <- file(filename, open = "w")
    on.exit(close(f))

    cat("\\documentclass{article}

\\usepackage[colorlinks=true]{hyperref}
\\usepackage[3D]{movie15}

\\begin{document}
\\includemovie[
   poster,toolbar,
   3Dcoo=0 -0.4 -113,
   3Dc2c=-60 10.4 -3,
   3Droo=61,
   3Daac=30,
   3Droll=-73,
   3Dlights=CAD
]{\\linewidth}{\\linewidth}{",asyToInclude,"}
\\end{document}",
        sep="",file=f)
    return(invisible(NULL))
}


cat("\n-------------------------------------------------------------------
------------------------ Class LongData plot ----------------------
------------------------------- Fin -------------------------------
-------------------------------------------------------------------\n")
