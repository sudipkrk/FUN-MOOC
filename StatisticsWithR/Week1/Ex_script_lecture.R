################################################################################
#################### Example codes from the lecture ############################
################################################################################

################################################################################
################################ Chapter 2 #####################################
################################################################################

############ read the example data 
smp.c <- read.csv2("smp1.csv")

##### check the content of the data with str() function
str(smp.c)

# table () to calculate the number of inmates with profession
table (smp.c$prof)

#### barplot 
barplot(table(smp.c$prof))


# histogram of for the age distribution of inmates 
hist(smp.c$age)

hist(smp.c$age,col="grey",main="",xlab="hee")
boxplot(smp.c$age,xlab="age")
boxplot(smp.c$age~smp.c$rs,ylab="age",xlab="Sensation research")
plot(smp.c$age,smp.c$n.child)
plot(jitter(smp.c$age),jitter(smp.c$n.child))


repdat <-read.csv2("outils_hdrs.csv")
str(repdat)
plotmeans(repdat$HDRS~repdat$VISIT,gap=0,barcol="black")

interaction.plot(repdat$VISIT,repdat$NUMRO,repdat$HDRS,lty=1,legend=FALSE)


################################################################################
####################### Chapter 4: describe function ###########################
################################################################################

describe(smp.c) 
describe (repdat)
