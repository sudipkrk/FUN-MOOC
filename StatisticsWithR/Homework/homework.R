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

# Question 1: For the three categorical variables in the file (it is up to you 
# to determine which variables they are), present the percentages of subjects 
# falling under each of the modalities.

#  change numeric variable to factor 

hsd$sexe<-factor (hsd$sexe, 
                  levels= c(0,1),
                  labels = c("homme", "femme"))

hsd$profession <- factor (hsd$profession, 
                          levels = c(1,2,3,4,5,6,7,8),
                          labels = c ( "agriculteur","artisan","cadre",
                                      "intermédiaire","employé","ouvrier",
                                      "sans emploi","autre"))

hsd$service <- factor (hsd$service) 

# get the percentage of subjects falling under each category

# Proportion table for variable sexe
prop.table(table(hsd$sexe))*100 #Percentage of male and female subjects

# Proportion table for variable profession
tab<- prop.table(table(hsd$profession, useNA= "always"))
names(tab)[9]<- "Not decided"
prop.table(tab)*100 # Percentage of subjects in different profession

#Proportion table for variable service
prop.table(table(hsd$service))*100


# Question 2: For the other variables, give a summary: mean, median, standard deviation, 
# minimum, maximum, number of data available (not missing).
ss<-hsd %>%
  select(3,5:9)


ss%>%
  psych::describe( na.rm = T, interp= T, skew = F, range = T)



# The code below does not give the desired answer for question number 2
fxn<- function(x)
  {
  c(n= length(x), min= min(x,na.rm = T), max= max(x,na.rm = T), mean= mean(x,na.rm = T), 
    median = median(x,na.rm = T),standard_dev= sd(x,na.rm = T))
}

#
sapply(ss, FUN=fxn)

# EASIER Way
# This require prettyR package
# install.packages ("prettyR")
# library(prettyR)
describe(hsd[,c(3,5:9)], num.desc= c("mean", "median", "sd", "min", "max","valid.n"))

#3. Make the histogram of relation score (score.relation)
hist(hsd$score.relation,
     main = "Histogram of Score Relation variable",
     xlab = "Score Relation",
     ylab= "Number of Subjects",
     col= "navy")


#4. Using two boxplots, plot side-by-side the relationship score distribution
# for men and women. 
boxplot(score.relation~sexe, 
        main= "Box plot:Score relation as a function of sex ",
        xlab= "Sex",
        ylab = "Number",
        col= "pink",
        horizontal = TRUE,
        data =hsd)



################################################################################

