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
                          labels = c ( "agriculteur exploitant",
                                      "artisan, commerçant, chef d'entreprise",
                                      "cadre, profession intellectuelle ou 
                                      artistique, profession libérale",
                                      "profession intermédiaire de l'enseignement, 
                                      de la santé, du travail social ou de la 
                                      fonction publique, technicien, contremaître,
                                      agent de maîtrise, clergé",
                                      "employé",
                                      "ouvrier",
                                      "étudiant, militaire, chômeur sans avoir 
                                      jamais travaillé",
                                      "autre"))

hsd$recommander <- factor (hsd$recommander, 
                           levels = c (1,2,3),
                           labels = c("non", "oui, probablement", 
                                      "oui,sûrement")) 

# get the percentage of subjects falling under each category

prop.table(table(hsd$sexe))*100       #Percentage of male and female subjects
prop.table(table(hsd$profession))*100 # Percentage of subjects in different profession
prop.table(table(hsd$recommander))*100# Percentage of subjects with different recommendation


# Question 2: For the other variables, give a summary: mean, median, standard deviation, 
# minimum, maximum, number of data available (not missing).
ss<-hsd %>%
  select(1,3,5:6,8:9)

ss%>%
  summarise(across(where(is.numeric),                 
                    .fns = list(
                     Mean = mean (.ss, na.rm = TRUE),
                     Median = median(.ss, na.rm = TRUE),
                     Standard_deviation = sd(.ss, na.rm = TRUE),
                     Minimum = min(.ss, na.rm = TRUE),
                     Maximum = max(.ss, na.rm = TRUE),
                     Number = sum(.ss, na.rm = TRUE)
                   )))
            

#3. Make the histogram of relation score (score.relation)
hist(hsd$score.relation)


#4. Using two boxplots, plot side-by-side the relationship score distribution
# for men and women. 
boxplot(score.relation~sexe, data =hsd)



################################################################################

