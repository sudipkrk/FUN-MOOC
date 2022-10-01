################################################################################
########################## Week 3:: Quiz #######################################
################################################################################

## get the data 
smp <- read.csv2("data/smp2.csv")
#wilcox.test(smp$duree~smp$dep.cons)
# wilcox.test(smp$duree,smp$dep.cons)


# t.test(smp$duree~smp$dep.cons)
# t.test(smp$duree,smp$dep.cons)



# question 2
str (smp)
# mean age by profession
with (smp, tapply(age, prof, mean, na.rm= TRUE))

# QUESTION 3
t.test(age~dep.cons, data= smp, var.equal= FALSE)


# question 4
t.test(smp$dur.interv[smp$dep.cons == 0], smp$dur.interv[smp$dep.cons == 1],var.equal= TRUE)

kruskal.test(dur.interv~dep.cons, data= smp)
pairwise.wilcox.test(smp$dur.interv, smp$dep.cons, p.adjust.method = "BH")




boxplot(dur.interv~dep.cons, data= smp)
with(smp, tapply(dur.interv, dep.cons, median, na.rm= TRUE))




# question 5
cor(smp$dur.interv, smp$age, use= "complete.obs")
cor.test(smp$dur.interv,smp$age, type= "normal", method= "pearson")

# question 6
wilcox.test(smp$dur.interv~ smp$suicide.past)


################################################################################
################## optional quiz ###############################################
################################################################################



