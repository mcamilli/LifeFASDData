### Survival Graph ###

install.packages("tidyverse")
install.packages("janitor")
install.packages("tidyquant")
install.packages("patchwork")
install.packages("survival")
install.packages("survminer")
remove.packages("rlang")
install.packages("rlang")
install.packages("dplyr")


library(tidyverse)
library(janitor)
library(tidyquant)
library(patchwork)
library(survival)
library(survminer)
library(dplyr)
library(rlang)


#1 Load the data

survivalData <- survivalData 

#2 Fit and summarize the data

fit1 <- survfit(Surv(as.numeric(time,event)) ~ 1, data = survivalData)

summary(fit1)

#3 Plot all the survival data

ggsurvplot(fit1, data = survivalData)

kmsurviv <- ggsurvplot(fit1, data = survivalData)




## stats

fit2 <- survfit(Surv(as.numeric(time,event)) ~ treatment, data = survivalData)
print(fit2)

survfit(Surv(as.numeric(time,event)) ~ treatment, data = control_3)

survfit(Surv(as.numeric(time,event)) ~ treatment, data = controlAnd6)

survfit(Surv(as.numeric(time,event)) ~ treatment, data = control_9)

print(summary(fit2, times=c(6,20)), digits = 4)


#4 Plot for all treatments
ggsurvplot(fit2, data = survivalData)


kmTreatment <- ggsurvplot(fit2, data = survivalData,
                          xlim= c(1,20),
                          break.x.by= 2, 
                          xlab = "Time (days)", ylab= c ("Survival "),
                          surv.scale="percent",
                          legend.title = "",
                          legend = c(0.08,0.15),
                          legend.labs = c("10%", "6%", "3%", "0%"),
                          palette = c("red","blue","darkorange", "darkgreen"),
                          pval = F,
                          risk.table = F,
                          risk.table.col = "strata",
                          conf.int = T,
                          tables.theme = theme_cleantable())

print(kmTreatment)



#5 for each treatment x control group

fitc3 <- survfit(Surv(as.numeric(time,event)) ~ treatment , data = control_3)

fitc6 <- survfit(Surv(as.numeric(time,event)) ~ treatment , data = controlAnd6)

fitc9 <- survfit(Surv(as.numeric(time,event)) ~ treatment , data = control_9)



ggsurvplot(fitc3, data = control_3,
           legend.title = "treatments",
           legend.labs = c("3%", "control"),
           pval = T,
           risk.table = T,
           conf.int = T,
           tables.theme = theme_cleantable())


ggsurvplot(fitc6, data = controlAnd6,
           legend.title = "treatments",
           legend.labs = c("6%", "control"),
           pval = T,
           risk.table = T,
           conf.int = T,
           confint.lm = T,
           pval.method = T,
           tables.theme = theme_cleantable())



ggsurvplot(fitc9, data = control_9,
           legend.title = "treatments",
           legend.labs = c("9%", "control"),
           pval = T,
           risk.table = T,
           conf.int = T,
           tables.theme = theme_cleantable())

#6 Perform long rank test

surv_diff <- survdiff(Surv(time, event) ~ treatment, data = survivalData)
surv_diff


survdiff(Surv(time, event) ~ treatment, data = control_6)

survdiff(Surv(time, event) ~ treatment, data = control_9)
  