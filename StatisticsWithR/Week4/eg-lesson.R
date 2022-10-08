################################################################################
####################### Week 4: Regression #####################################
################################################################################


# get the smp2 data 
d<- read.csv2("data/smp2.csv")

str(d)              # get the structure of the dataset

# scatter plot 
plot (d$age, d$dur.interv)

# jitter plot 
plot(jitter(d$age), jitter(d$dur.interv))

plot(jitter(d$age), jitter(d$dur.interv, factor= 4))

# draw a line 
abline(lm(d$dur.interv~d$age))

# linear regression model
mod1<- lm(dur.interv~ age, data= d)
summary(mod1)

# correlation test 
cor.test(d$dur.interv, d$age)

# ttest
t.test(d$dur.interv~d$dep.cons, var.equal= TRUE)

# multiple linear regression
mod3<- lm(dur.interv~ age+ dep.cons+subst.cons+scz.cons, data = d)
summary(mod3)

mod4<- lm(dur.interv~ age+ dep.cons+subst.cons+scz.cons+prof, data = d)
summary(mod4)

# relevel the variable 
d$prof<- relevel(d$prof, ref= "ouvrier")

mod5<- lm(dur.interv~ age+ dep.cons+subst.cons+scz.cons+prof, data = d)
summary(mod5)
drop1(mod5,.~., test = "F")


# Add interaction term
mod6<- lm(dur.interv~ age+ dep.cons*subst.cons+scz.cons, data = d)
summary(mod6)

#
mod7<- lm(dur.interv~prof, data= d)
summary(mod7)

# drop1 fucntion compares all the possioble model that can be constructed by droping 
# a single model term
drop1(mod7,.~., test= "F")

# 
mod3<- lm(dur.interv~ age+ dep.cons+subst.cons+scz.cons, data = d)
hist(resid(mod3), col= "grey", main = "")

# Logistic regression
mod1<- glm (suicide.hr~abus, data= d, family = "binomial")
summary(mod1)

exp(0.7688)

# twoway table from epi 
library(Epi)

twoby2(1-d$suicide.hr, 1-d$abus)

# Multiple logistic regression
mod2<- glm(suicide.hr~abus+discip+duree, data=d, family= "binomial")
summary(mod2)

# anti log of the intercept
exp(coefficients(mod2))
