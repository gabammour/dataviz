#Importation : 
pop <- read.csv2("Base/pop.csv", sep = ";")
co2 <- read.csv2("Base/co2.csv", sep = ";")
pib_hab_ppa <- read.csv2("Base/PIB_hab_ppa.csv", sep = ";")


# Population mondiale # 
#Modification faite (si besoin pour les autres )
#library(readxl)
#pop <- read.csv2("BASE/pop.csv", sep = ";")
#pop <- pop[,-c(2,3,4)]
#pop$X1960 <- as.numeric(pop)
#pop <- t(pop)
#colnames(pop) <- pop[1,]
#pop <- pop[-1,-c(2,4)]
#write.csv2(pop, "Base/pop.csv", row.names = FALSE)


#Base co2
# co2<-t(co2)
# colnames(co2)<-co2[1,]
# co2<-co2[-1,]
# co2 <- as.data.frame(co2)
# co2 <- apply(co2, 2, as.numeric)
# write.csv2(co2,"BASE/co2.csv", row.names = FALSE)

#Base PIB Hab PPA 
# PPA <- read.csv2("Base/PIB_hab_ppa.csv", sep = ";")
# View(PPA)
# PPA<-PPA[,-2]
# PPA<-t(PPA)
# colnames(PPA)<-PPA[1,]
# PPA <- PPA[-1,]
# PPA <- apply(PPA, 2, as.numeric)
# PPA <- as.data.frame(PPA)
# str(PPA)
# write.csv2(PPA,"BASE/PIB_hab_ppa.csv", row.names = FALSE)


