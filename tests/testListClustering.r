source("../R/listClustering.r")

cat("\n#######################################################################
########################## Test ListClustering #########################
############################### Creation ###############################
####################################################################\n")

cleanProg(.ListClustering.validity,,,0)
cleanProg(listClustering)
cleanProg(.ListClustering.show)
new("ListClustering")
lcl0 <- lcl3 <- listClustering()

cleanProg(.listClustering.set,,,1) # CLUSTER_NAMES
lcl3['criterionActif'] <- "test"

lcl3['add'] <- c3a
try(lcl3['criterionPossibles'] <- "test")
lcl3['initializationMethod'] <- "randomK"
lcl3['sorted'] <- TRUE
lcl3['add'] <- c3a
lcl3['add'] <- c3b
lcl3['add'] <- c3c
lcl3['add'] <- c3d
lcl3['add'] <- c3e
lcl3['add'] <- c3an
lcl3['add'] <- c3bn
lcl3['add'] <- c3cn
lcl3['add'] <- c3dn
lcl3['add'] <- c3en
lcl3['add'] <- c3fn
lcl3['add'] <- c3gn
lcl3['add'] <- c3hn
lcl3['add'] <- c3in

ordered(lcl3)

lcl3['add'] <- c7an
lcl3['add'] <- c7bn
lcl3['add'] <- c7cn
lcl3['add'] <- c7dn
lcl3['add'] <- c7en
lcl3['add'] <- c7fn
try(lcl3['c18'] <- c3en)
try(lcl3['ccc18'] <- c3en)
lcl3['clear'] <- 'c25'

cleanProg(.listClustering.get,,,3) # CLUSTER_NAMES listI length
lcl3['criterionActif']
lcl3['criterionPossibles']
lcl3['initializationMethod']
lcl3['sorted']
try(lcl3['t3'])
lcl3['c9']
lcl3['test']

cleanProg(.ListClustering.ordered,,,2) # CRITERION_MIN_OR_MAX length
cleanProg(.ListClustering.plot,,,4) # CLUSTER_NAMES CRITERION_MIN_OR_MAX length letters
cleanProg(regroup)

plot(lcl0)
plot(lcl0,nbCriterion=20,criterion="test")

plot(lcl3)
plot(lcl3,criterion=c("test","calinski"))



lcl7 <- listClustering()
lcl7['criterionActif'] <- "calinski"
lcl7['add'] <- c3a
lcl7['add'] <- c3b
lcl7['add'] <- c3c
lcl7['add'] <- c3d
lcl7['add'] <- c3e
lcl7['add'] <- c3an
lcl7['add'] <- c3bn
lcl7['add'] <- c3cn
lcl7['add'] <- c3dn
lcl7['add'] <- c3en
lcl7['add'] <- c3fn
lcl7['add'] <- c3gn
lcl7['add'] <- c3hn
lcl7['add'] <- c3in
lcl7['add'] <- c7an
lcl7['add'] <- c7bn
lcl7['add'] <- c7cn
lcl7['add'] <- c7dn
lcl7['add'] <- c7en
lcl7['add'] <- c7fn
lcl7['clear'] <- 'c18'

par(mfrow=c(2,2))
ordered(lcl7)
plot(lcl7)
plot(lcl7,criterion=c("test","calinski"))

lcl7['criterionActif'] <- "test"
ordered(lcl7)
plot(lcl7)
plot(lcl7,criterion=c("test","calinski"))





lcl3['add'] <- c3a
lcl3['add'] <- c3b
lcl3['add'] <- c3c
lcl3['add'] <- c3d
lcl3['add'] <- c3e
lcl3['add'] <- c3an
lcl3['add'] <- c3bn
lcl3['add'] <- c3cn
lcl3['add'] <- c3dn
lcl3['add'] <- c3en
lcl3['add'] <- c3fn
lcl3['add'] <- c3gn
lcl3['add'] <- c3hn
lcl3['add'] <- c3in
ordered(lcl3)
regroup(lcl3)


lcl7 <- listClustering()
lcl7['criterionActif'] <- "calinski"
lcl7['add'] <- c3b
lcl7['add'] <- c3c
lcl7['add'] <- c3bn
lcl7['add'] <- c3cn
lcl7['add'] <- c3b
lcl7['add'] <- c3c
lcl7['add'] <- c3bn
lcl7['add'] <- c3cn
regroup(lcl7)
dev.off()

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++ Fin Test ListClustering +++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

