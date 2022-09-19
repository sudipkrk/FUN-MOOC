################################################################################
############################# Homework #########################################
################################################################################


############################## Week 1 ##########################################

# data set: hospital satisfaction data 

# 1. get the data set

hsd<- read.csv2("Homework/Week1/satisfaction_hopital.csv")

# get the structure of the data set
str(hsd)
summary(hsd)
#  change sexe to factor
hsd$sexe<-factor (hsd$sexe, labels = c("homme", "femme"))

prop.table(table(hsd$sexe))


#3. Make the histogram of relation score (score.relation)
hist(hsd$score.relation)


#4. Boxplot 
boxplot(score.relation~sexe, data =hsd)
