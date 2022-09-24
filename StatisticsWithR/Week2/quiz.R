################################################################################
################################ Week 2 Quiz ###################################
################################################################################


# Get the data
smp<- read.csv2('data/smp2.csv')

# Explore the data set
dim(smp)     # get the dimension of the dataset
nrow(smp)    # Number of rows of the dataset
ncol(smp)    # Number of columns of the dataset
names(smp)   # Variables names in the dataset
colnames(smp)# Columns names of the dataset

summary(smp)
str(smp)

# 1. For a qualitative variable z with 3 categories {A, B, C}, the command which(z == A) returns
(which(smp$prof== "autre"))
# returns the observation number fulfilling the requirement

#2  To represent in the form of a bar diagram the distribution of frequencies according 
# to the categories of a qualitative variable z, which command can we use?
barplot(table(smp$prof))

#4. What is the average number of children (variable n.child) among individuals 
# diagnosed as depressive (dep.cons = 1) (2 decimal places)?
round(mean(smp [smp$dep.cons==1, "n.enfant"], na.rm = TRUE),2)

# 5. Give the upper limit of the interquartile interval for the duration variable in individuals 
# whose age is strictly less than 35 years (2 decimal places). Upper bound 

round(quantile(smp[smp$age<35, "duree"], .75, na.rm = TRUE),2)


# 6. We want to estimate the average duration of intervention (dur.interv) in individuals 
# with at least one history of suicide attempt (suicide.past = 1). What command can we use?
mean(smp[smp$suicide.past == 1, "dur.interv"], na.rm = TRUE)


#7
summary(smp$age)
smp$age.cat <- cut(smp$age, breaks = c(19, 28, 37, 48, 83),include.lowest = TRUE)
table(smp$age.cat)



#################################################################################
########################### Optional quiz #######################################
#################################################################################

