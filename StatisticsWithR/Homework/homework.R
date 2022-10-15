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


################################################################################
###################### Homework:: Week 2 #######################################
################################################################################

# get the data  and gernaral information
hsd <- read.csv2("Homework/Week1/satisfaction_hopital.csv")
str(hsd)
summary(hsd)

# Question 1: Transform the variable “recommend” into a binary variable 
# “recommend.b”: “recommend.b” is 0 if “recommend” is 0 or 1; “recommend.b” is
# worth 1 if  “recommend” is worth 2.

# transforming variable recommender in to binary variable and naming it recommender.b
hsd$recommander.b<- ifelse(hsd$recommander>1, 1,0)

# Question 2: Using an odds-ratio, estimate the strength of the association 
# between “recommend.b” and “sex”. Estimate a confidence interval of this odds-ratio.


# We require Epi package for the analysis
install.packages("Epi")  # install the Epi package
library(Epi)             # Load the Epi package

twoby2(hsd$recommander.b, hsd$sexe)


# Question 3: Calculate the (Pearson's) correlation between “score.relation” and
# “age”. Test this correlation statistically (the script must include the possible
# verification of the validity conditions of the method used).

# check the distribution of age whether it is normally distributed or not 
hist(hsd$age, main= "Histogram showing age distribution", xlab= "Age", ylab= "Frequency", freq = FALSE)

# Correlation between score.relation and age with cor function and use observation with complete cases
cor(hsd$score.relation, hsd$age, use= "complete.obs")

# for validity of the conditions method use: use cor.test function which gives pearson product-moment correlation $
# with confidence interval

cor.test(hsd$score.relation, hsd$age)


# Question4:
# Is the average relationship score significantly different for men and women?
#(the script must include the possible verification of the validity conditions of the method used)

# Varification of the t-test hypothesis
# varification per group each grop must have more the 30 observation
table (hsd$sexe[!is.na(hsd$score.relation)])

# check the equality of standard deviation for equal variane 
# get the standard deviation of score.realtion grouped by sexe
with(hsd, tapply(score.relation, sexe, sd, na.rm= TRUE))

# t-test 
t.test(hsd$score.relation~ hsd$sexe, var.equal= TRUE)



################################################################################
###################### Homework: Last Week #####################################
################################################################################
hsd<- read.csv2("data/satisfaction_hopital.csv")

# recode categorical variables
hsd$pro_cat <- factor(hsd$profession,
                              labels=c("agriculteur","artisan"
                                       ,"cadre","intermédiaire"
                                       ,"employé","ouvrier"
                                       ,"sans emploi","autre"))
hsd$ser_cat <- factor(hsd$service,
                           labels=c("1","2","3","4","5","6","7","8"))

# Question 1: linear regression model

model <- lm(score.relation~ age+sexe+score.information
          +amelioration.sante + amelioration.moral
          + pro_cat + ser_cat, data=hsd)
# compare all the possible model by dropping the single model term
drop1(model,.~.,test="F")

summary(model)

# check the validity of the model
# diagnostic plot the model
plot(model)
# plot the histogram for residual
hist(resid(model), col= "gray")


# Question 2:
# recode the variable 
hsd$reco_cat <- ifelse(hsd$recommander>1,1,0)

# tabluate the variable created
table(hsd$recommander, hsd$reco_cat, deparse.level=2, useNA="always")

# Logistic regression model
model1 <- glm(reco_cat~ age+sexe+score.information+amelioration.sante
            + amelioration.moral + pro_cat + ser_cat,
            data=hsd, family="binomial")
drop1(model,.~.,test="Chisq")
