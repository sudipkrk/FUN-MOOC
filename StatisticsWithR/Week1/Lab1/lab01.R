##
## Lab 1
##

smp <- read.csv2("smp2.csv")

View(smp)

names(smp)

str(smp)

summary(smp)

summary(smp$age)

smp$age

smp$age[1]

smp$age[1:10]

min(smp$age)


help(min)

min(smp$age, na.rm=TRUE)

smp$abus[1:10]

unique(smp$abus)

head(smp$abus, n=10)

length(smp$abus)

nrow(smp)

table(smp$abus)

table(smp$abus,useNA="always")

summary(smp$abus)

head(smp$abus)

head(factor(smp$abus))

abus <- factor(smp$abus,levels=c(0,1),labels=c("Non","Oui"))

table(abus)

names(smp)

head(smp$n.enfant)

summary(smp$n.enfant)

table(smp$n.enfant)

table(smp$n.enfant>4)

smp$n.enfant.cat <- factor(smp$n.enfant)

table(smp$n.enfant)

levels(smp$n.enfant.cat)

nlevels(smp$n.enfant.cat)

levels(smp$n.enfant.cat)[6:13] <- "5+"

table(smp$n.enfant.cat)

save(smp,file="smp_v1.rda")

savehistory("commandes.R")

