################################################################################
#################### Week 5: Survival Analysis #################################
################################################################################

# read the data 
alc <- read.csv2("data/alcool.csv")

# get the structure of the data frame 
str(alc)

# dimension of the dataset 
dim(alc)

# number of rows and column of the data set 
nrow(alc)
ncol(alc)

# name of the row and column of the dataset 
colnames(alc)
rownames(alc)

# install and load survival package 
install.packages("survival")
library(survival)

# survival plot
plot(survfit(Surv(alc$t,alc$SEVRE)~1), main = "survival curve")

plot (survfit(Surv(t,SEVRE)~SEXE, data= alc), col = c( "red", "black"), main = 'Survival curve')

survfit(Surv(alc$t,alc$SEVRE)~1)


####
survdiff(Surv(t,SEVRE)~SEXE, data= alc)

## cox
coxph(Surv(t,SEVRE)~AGE, data =alc)

# modelling 
mod <- coxph(Surv(t,SEVRE)~AGE+SEXE+EDVNEG, data =alc)
mod
exp(coef(mod))

# plot
par(mfrow= c(2,2))
plot(cox.zph(mod))

# Introduction to multidimensional method 
da<- read.csv2("data/smp1.csv")
var <- c("age","n.enfant","scz.cons","dep.cons","grav.cons","rs","ed","dr")

#
round (cor(da[,var], use = "complete.obs"), digits = 3)

# corrplot: install and load corrplot package 
install.packages('corrplot')
library(corrplot)

par(mfrow= c(1,1))
# corrplot
corrplot(cor(da[,var], use = 'complete.obs'), method = 'circle')


# Principle component analysis
install.packages('psy')
library(psy)
mdspca(da[,var])
sphpca(da[,var])

sphpca(da[,var], v=55)

#
response= "grav.cons"
explanatory = c("age","n.enfant","dep.cons", "scz.cons",
                "rs","ed","dr")
fpca(data=da,y=response,x=explanatory,partial="No")

# hierarchical clustering
cha<- hclust(dist(t(scale(da[,var]))),
               method="ward.D")
plot(cha, xlab = "", ylab = " ", main  = " hieratrchical clustering")

# heatmap
obj<- cor(da[,var], use = "pairwise.complete.obs")
heatmap(obj, col = grey (seq(1,0,length=16)))
