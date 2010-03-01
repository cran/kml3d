source("../R/parLongData.r")

cat("\n####################################################################
######################### Test ParLongData #########################
####################################################################\n")

cleanProg(.ParLongData.validity)
cleanProg(parLongData)
cleanProg(parTraj)
cleanProg(parMean)
cleanProg(.ParLongData.show)
cleanProg(ParLongData.Partition.expand,,,1) # LETTERS
cleanProg(ParLongData.nbClusters.expand,,,1) # LETTERS

new("ParLongData",col="red",type="b",pch="clusters")
#parLongData()
parTraj()
parMean()
par1 <- parTraj(col="red")
par2 <- parMean(type="n")

par1['col']
par1['col']<-"blue"

expandParLongData(par2,p6a)
par2['pch']<-"symbols"
expandParLongData(par2,p6a)
par2['pch']<-"A"
expandParLongData(par2,p6a)

par2['pch']<-"letters"
expandParLongData(par2,3)
par2['pch']<-"symbols"
expandParLongData(par2,5)
par2['pch']<-"A"
expandParLongData(par2,2)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++ Fin Test ParLongData +++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
