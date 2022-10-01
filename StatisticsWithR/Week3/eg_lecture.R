################################################################################
#################### Example scripts from the lecture ##########################
################################################################################

# get the data
smp<- read.csv2("data/smp1.csv")

# get the structure of the dataset

str(smp)

# correlation between age and number of enfant
cor(smp$age, smp$n.enfant, use = "complete.obs")

#
smp$ed.b<- ifelse(smp$ed> 2,1,0)
str(smp)

table(smp$ed.b, smp$dep.cons, deparse.level = 2, useNA = "always")
tab<-table(smp$ed.b, smp$dep.cons, deparse.level = 2)
prop.table(tab,1)
prop.table(tab,2)

# chi-square test 
chisq.test(smp$ed.b, smp$dep.cons, correct = FALSE)

# FISCHER TEST
fisher.test(smp$ed.b, smp$dep.cons)

# histogram for the age 
hist(smp$age)

# normal qq plot  and qqline
qqnorm(smp$age)
qqline(smp$age)

# by function
by(smp$age, smp$ed.b, sd, na.rm=TRUE)


# T-TEST
t.test(smp$age~smp$ed.b, var.equal= TRUE)

# WILCOX TEST 
wilcox.test(smp$age~smp$ed.b)
