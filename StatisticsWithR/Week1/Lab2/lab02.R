##
## Lab 2
##

load("smp_v1.rda")

names(smp)

smp$age[1]

smp[1,1]

smp[1,"age"]

head(smp$prof)

head(smp$prof == "agriculteur")

table(smp$prof == "agriculteur")

which(smp$prof == "agriculteur")

smp$age[which(smp$prof == "agriculteur")]

subset(smp, prof == "agriculteur", age)

subset(smp, prof == "agriculteur", 1:5)

names(smp)[1:5]

subset(smp, prof == "agriculteur", c(age, duree, discip, n.enfant))

subset(smp, prof == "agriculteur" & n.enfant > 2, c(age, duree, discip, n.enfant))

subset(smp, prof == "agriculteur" & n.enfant > 2 & complete.cases(duree), c(age, duree, discip, n.enfant))

table(smp$n.enfant.cat)

tab <- table(smp$n.enfant.cat)

sum(tab)

tab / sum(tab)

prop.table(tab)

round(prop.table(tab),3)

round(prop.table(tab),2)

barplot(prop.table(tab) * 100)

barplot(prop.table(tab) * 100, ylim=c(0,30))

barplot(prop.table(tab) * 100, ylim=c(0,30), las=1)

head(smp$age)

summary(smp$age)

hist(smp$age)

hist(smp$age, nclass=8)

hist(smp$age, nclass=8, prob=TRUE)

hist(smp$age, nclass=8, prob=TRUE, las=1)

lines(density(smp$age, na.rm=TRUE))
