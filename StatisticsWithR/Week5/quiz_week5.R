################################################################################
########################## Quiz: Week 5 ########################################
################################################################################

# make categorical variable for age 
alc$age_cat<- as.factor(ifelse(alc$AGE> 50, 1,0))

# Question 1: We want to compare the risk of relapse of the alcoholic disease in 
# two sub-groups: the over-50 group (strictly over 50, recoded as “1”) and the 
# under-50 group (50 or less, recoded to “0”). Give the p-value associated with 
# the corresponding statistical test (4 decimal places):
survdiff(Surv(t, SEVRE)~age_cat, data=alc)

coxph(Surv(t, SEVRE)~age_cat, data=alc)

# Right answer
alcool$AGE50 <- rep(0, length(alcool$AGE))
alcool$AGE50[alcool$AGE>50] <- 1
survdiff(Surv(alcool$t, alcool$SEVRE)~ alcool$AGE50)
diff <- survdiff(Surv(alcool$t, alcool$SEVRE)~ alcool$AGE50)
p <- pchisq(diff$chisq, length(diff$n)-1, lower.tail = FALSE)
sprintf("%.4f",p)

# Question 2: We wish to test the association between the risk of relapse of the
# alcoholic disease and the variables SEX, AGE and the interaction between the 
# variables SEX and AGE. Give the p-value associated with the interaction between
# the SEX and AGE variables in the corresponding test (2 decimal places):
coxph(Surv(t, SEVRE)~SEXE+AGE+SEXE*AGE, data=alc)
