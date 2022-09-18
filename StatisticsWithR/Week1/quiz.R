################################################################################
#################################### Quiz ######################################
################################################################################

smp <- read.csv2("smp2.csv")
View(smp)

# display the minimun value for age 
min (smp$age, na.rm  = T)

smp$new<- factor(smp$abus, levels = c(1,0), labels = c("Yes", "No"))
sum(table(smp$new))
table (smp$new)
# IQR Upper limit
quantile(smp$dur.interv, c(.25, .50, .75),na.rm = T)

table(smp$ecole <=3)


nrow(smp[smp$prof == "sans emploi",])
sum(with(smp, prof== "sans emploi",))
table(smp$prof)

sum(complete.cases(smp))

# average age of first 10 obeservation

mean(smp$age[1:10])
median(smp$dur.interv[1:300], na.rm = T)
