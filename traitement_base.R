# Population mondiale

#Modification faite (si besoin pour les autres )
#library(readxl)
#pop <- read.csv2("BASE/pop.csv", sep = ";")
#pop <- pop[,-c(2,3,4)]
#pop$X1960 <- as.numeric(pop)
#pop <- t(pop)
#colnames(pop) <- pop[1,]
#pop <- pop[-1,-c(2,4)]

#write.csv2(pop, "Base/pop.csv", row.names = FALSE)

#Importation : 
pop <- read.csv2("Base/pop.csv", sep = ";")


