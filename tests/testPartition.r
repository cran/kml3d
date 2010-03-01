source("../R/partition.r")

cat("####################################################################
########################## Test  Partition #########################
####################################################################\n")

cleanProg(.Partition.validity,,,2)  # LETTERS MAX_CLUSTERS
new("Partition") # Doit marcher meme apres recompilation
new("Partition",clusters=as.factor(c("A","B","A")),nbClusters=2)
new("Partition",clusters=as.factor(c("A","B","A")),nbClusters=4)
new("Partition",clusters=as.factor(c("A","M","A")),nbClusters=20)
new("Partition",clusters=as.factor(c("A","B",NA,"A")),nbClusters=3)
new("Partition",clusters=as.factor(c(NA,NA)),nbClusters=3)
new("Partition",clusters=as.factor(c("A","B","A")),nbClusters=26)
new("Partition",clusters=as.factor(c("A","B","B")),nbClusters=26)
ordered(new("Partition",clusters=as.factor(c("A","B","B")),nbClusters=26))

try(new("Partition",clusters=factor(c("A","C","A"),levels=LETTERS[1:3]),nbClusters=2))
try(new("Partition",clusters=as.factor(c("A","B","D")),nbClusters=4))
(new("Partition",clusters=as.factor(c("A","C",NA,"A")),nbClusters=3))
(new("Partition",clusters=as.factor(c("A","C",NA,"A")),nbClusters=20))
try(new("Partition",clusters=as.factor(c("A","C",NA,"M")),nbClusters=3))
try(new("Partition",clusters=factor(c("A","C",NA,"aa")),nbClusters=3))

cleanProg(.partition.constructor,,,1) # LETTERS
cleanProg(.Partition.show,,,1) # LETTERS

partition()
partition(clusters=c("A","B"))
partition(clusters=c("A",NA))
try(partition(nbClusters=3))
partition(clusters=c("C","B","C"),nbClusters=4)
partition(clusters=c("C","B","C"))
partition(clusters=c(NA,NA))
partition(clusters=c("A","C","A"),nbClusters=4)
partition(clusters=c(1,3,1),nbClusters=4)
partition(clusters=c(1,3,1))


cat("### Jeux de données ###\n")
cleanProg(.partition.ordered,,,1) # LETTERS
p0a <- p0b <- partition()

p1a <- partition(clusters=c("A","B","B"))
p1a <- ordered(p1a)
p1b <- partition(clusters=c("A","B","A"))
p1c <- partition(nbClusters=2,clusters=c("A","B","B")) # Réarrangement pour avoir le plus gros cluster en A

p2a <- partition(nbClusters=3,clusters=c("A","A","B"))
p2b <- partition(nbClusters=3,clusters=c("A","B","A"))
p2c <- partition(nbClusters=3,clusters=c("A","C","B"))

p2an <- partition(nbClusters=3,clusters=c("A",NA,"B"))
p2bn <- partition(nbClusters=3,clusters=c("A",NA,NA))
p2cn <- partition(nbClusters=3,clusters=c(NA,NA,NA))

p3a <- partition(nbClusters=2,clusters=c(rep(1:2,each=100)))
p3b <- partition(nbClusters=3,clusters=c(rep(1:3,66),1,1))
p3b["clusters"][1:6]<-"B"
p3b["clusters"][7:9]<-"C"
p3b <- ordered(p3b)

p3c <- partition(nbClusters=4,clusters=c(rep(c(1:4,2:3,2:3),25)))
p3d <- partition(nbClusters=5,clusters=c(rep(c(3,2:4,1,3:5),25)))
p3e <- partition(nbClusters=6,clusters=c(rep(c(1:6,2,2),25)))
p3f <- partition(nbClusters=7,clusters=c(rep(c(1:7,4),25)))

p3g <- partition(nbClusters=9,clusters=c(rep(LETTERS[1:9],22),"A","A"))
p3h <- partition(nbClusters=18,clusters=c(rep(c(1:18,1:4,1:2,1),8)))
p3i <- partition(nbClusters=25,clusters=c(rep(LETTERS[1:3],66),"A","A"))


p4a <- partition(nbClusters=2,clusters=rep(LETTERS[1:2],c(10,20)))
p4b <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(10,5,15)))
p4c <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(10,20,0)))
p4d <- partition(nbClusters=4,clusters=rep(LETTERS[1:4],c(10,5,10,5)))
p4e <- partition(nbClusters=3,clusters=rep(LETTERS[1:3],c(10,10,10)))

p4an <- p4a
for(i in 1:5){p4an@clusters[round(runif(1,1,30))] <- NA}
p4an <- partition(nbClusters=p4an["nbClusters"],clusters=p4an["clusters"])
validObject(p4an)

p4bn <- p4b
for(i in 1:10){p4bn@clusters[round(runif(1,1,30))] <- NA}
p4bn <- partition(nbClusters=p4bn["nbClusters"],clusters=p4bn["clusters"])
validObject(p4bn)

p4cn <- p4c
for(i in 1:10){p4cn@clusters[round(runif(1,1,30))] <- NA}
p4cn <- partition(nbClusters=p4cn["nbClusters"],clusters=p4cn["clusters"])
validObject(p4cn)

p4dn <- p4d
for(i in 1:15){p4dn@clusters[round(runif(1,1,30))] <- NA}
p4dn <- partition(nbClusters=p4dn["nbClusters"],clusters=p4dn["clusters"])
validObject(p4dn)

p4en <- p4e
for(i in 1:20){p4en@clusters[round(runif(1,1,30))] <- NA}
p4en <- partition(nbClusters=p4en["nbClusters"],clusters=p4en["clusters"])
validObject(p4en)

p6a <- partition(nbClusters=2,clusters=LETTERS[c(1,2,1,2,1,2,1,2,1,2,1,2)])
p6b <- partition(nbClusters=3,clusters=LETTERS[c(1,2,3,1,2,3,1,2,3,1,2,3)])
p6c <- partition(nbClusters=4,clusters=c("A","A","B","A","C","D","D","C","B","A","C","D"))
p6cn <- partition(nbClusters=4,clusters=c("A","A","B","A",NA,NA,"D","C","A","B","A",NA))

p7a <- partition(nbClusters=2,clusters=LETTERS[rep(c(1,2),100)])
p7b <- partition(nbClusters=3,clusters=LETTERS[c(rep(c(1,2,3),66),1:2)])
p7c <- partition(nbClusters=4,clusters=rep(1:5,40))
p7cn <- partition(nbClusters=4,clusters=rep(c(1:3,NA),50))
p7d <- partition(nbClusters=5,clusters=rep(1:5,40))
p7e <- partition(nbClusters=6,clusters=c(rep(1:6,33),1,2))
p7f <- partition(nbClusters=7,clusters=c(rep(1:7,28),1:4))
p7g <- partition(nbClusters=8,clusters=c(rep(1:8,25)))

p8a <- partition(nbClusters=2,clusters=LETTERS[rep(c(1,2),600)])
p8b <- partition(nbClusters=3,clusters=LETTERS[rep(c(1,2,3),400)])
p8c <- partition(nbClusters=5,clusters=rep(1:5,240))
p8cn <- partition(nbClusters=4,clusters=rep(c(1:3,NA),300))



cat("\n####################################################################
########################## Test  Partition #########################
############################# Accesseurs ###########################
####################################################################\n")

p0a["nbClusters"]
p1a["nbClusters"]
p1b["nbClusters"]
p1c["nbClusters"]
p2a["nbClusters"]
p2bn["nbClusters"]
p2c["nbClusters"]
p3a["nbClusters"]
p3b["nbClusters"]
p3c["nbClusters"]
p4a["nbClusters"]
p4bn["nbClusters"]
p4c["nbClusters"]
p4dn["nbClusters"]
p4e["nbClusters"]
p6a["nbClusters"]
p6b["nbClusters"]
p6cn["nbClusters"]

p0a["clusters"]
p1a["clusters"]
p1b["clusters"]
p1c["clusters"]
p2an["clusters"]
p2bn["clusters"]
p2c["clusters"]
p3a["clusters"]
p3b["clusters"]
p3c["clusters"]
p4an["clusters"]
p4b["clusters"]
p4cn["clusters"]
p4dn["clusters"]
p4e["clusters"]
p6a["clusters"]
p6b["clusters"]
p6cn["clusters"]

p1a["nbClusters"]<-4
try(p1a["nbClusters"]<-0)
p1b["nbClusters"]<-2
try(p0a["nbClusters"]<-4)
try(p1a["nbClusters"]<-1)

p1a["clusters"]<-c("A","B","B")
p1a["nbClusters"]<-2
try(p1a["clusters"]<-c("A","B","C"))
p1a["nbClusters"]<-3
p1a["clusters"]<-c("A","B","C")
p1a <- partition(clusters=c("A","A","B"))



cat("\n####################################################################
########################## Test  Partition #########################
############################# Affichage ############################
####################################################################\n")

cleanProg(.Partition.show,,,1) #LETTERS
p0a
p1a
p4dn


cat("\n####################################################################
########################## Test  Partition #########################
############################### Autre ##############################
####################################################################\n")


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test Partition ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
