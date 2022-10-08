################################################################################
############################ Quiz 4 ############################################
################################################################################

# get the data
smp<- read.csv2('data/smp2.csv')

str(smp)

# 1. Perform oneway anova
s<- lm(dur.interv~age, data =smp)
summary(s)
anova(s)



-----------------------------------------------------------------------

aggregate(age ~ prof, data = subset(smp, n.enfant > 3 &
                                      prof %in% c("sans emploi", "ouvrier", "cadre", "employe"), 
                                    c(age, prof)), var)





# recode the variable 

str(smp$n.fratrie)
smp$jpt[smp$n.fratrie <=2] <- "0-2"
smp$jpt[smp$n.fratrie >= 3 & smp$n.fratrie<=4] <- "3-4"
smp$jpt[smp$n.fratrie >=5] <- "5+"


str(smp$jpt)
factor(smp$jpt)
table(smp$jpt)

a<- lm(age~ jpt, data= smp)
summary(a)

summary(aov(age~jpt, data= smp))


# 5 answer from the course
 n.fratrie.c <- cut(smp$n.fratrie, breaks = c(0, 2, 4, 21), include.lowest = TRUE)
 anova(lm(age ~ n.fratrie.c, data = smp))
# or
 summary(aov(age ~ n.fratrie.c, data = smp))


 #6

mod<- glm(separation~ age, data= smp, family= binomial("logit"))
summary(mod)
exp(confint(mod))


# 
m <- glm(separation ~ age, data = smp, family = binomial)
exp(confint(m))
# or
exp(confint(m))["age",1]




#######################################################################
################## Optional Quiz ######################################
#######################################################################
s<- lm(dur.interv~age, data =smp)
confint((lm(dur.interv~age, data = smp)), level = 0.90)


da<- subset(smp, dep.cons==1, c(age, suicide.hr))
sk<- glm(suicide.hr~age, data= da, family= "binomial")
summary(sk)

# log(p/1-p) = -0.01160-0.009821*age
# for 35 years
# log(p/1-p) = -0.01160-0.009821*35

# -0.01160-0.009821*35
# log(p/1-p)= -0.355
# p/(1-p) = exp(-0.355)
# p/(1-p) = 0.70
# p = 0.70 -0.7p
# 1.7p = 0.70
# p = 0.70/1.7
# p = 0.411

