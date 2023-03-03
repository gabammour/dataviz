# Population mondiale
pop <- read.csv2("BASE/pop.csv", sep = ";")
pop <- pop[,-c(2,3,4)]
pop$X1960 <- as.numeric(pop)


