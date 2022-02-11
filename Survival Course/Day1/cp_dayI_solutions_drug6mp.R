
# Computer Practical Day 1 -examples using drug6mp data
# clinical trial of a drug 6-mercaptopurine (6-MP) in 21 children with acute leukemia


# install any missing libraries
# To install packages
install_packages <- c("survival", "survminer", "haven")
for (i in 1:length (install_packages)){
  if (!install_packages[i] %in% installed.packages()){
    install.packages(install_packages[i])
  }
}


library(survival)
library(survminer)
library(haven) # better read spss file with library haven to be able to also use plots from survminer


Day_1_drug6mp <- read_sav("Day 1 - drug6mp.sav")


# Kaplan-Meier for all
km_fit <- survfit(Surv(timerelmp, Status) ~ 1, data = Day_1_drug6mp)
km_fit
summary(km_fit)     #summary(km_fit, time = 15) for time 15
plot(km_fit, xlab = "Time since the start of treatment", 
     ylab = "Survival probability")

ggsurvplot(km_fit,
           pval = FALSE, conf.int = TRUE,
           risk.table = TRUE, 
           risk.table.col = "strata", 
           linetype = "strata", 
           xlab = "Time since the start of treatment",
           surv.median.line = "hv", 
           ggtheme = theme_bw(), # 
           palette = c("#E7B800", "#2E9FDF"))


# extra: Kaplan-Meier per group
km_fit2 <- survfit(Surv(timerelmp, Status) ~ groups, data = Day_1_drug6mp)
km_fit2
summary(km_fit2)
plot(km_fit2, xlab = "Time since the start of treatment", 
     ylab = "Survival probability")

ggsurvplot(km_fit2,
           pval = TRUE, conf.int = FALSE,
           risk.table = TRUE, 
           risk.table.col = "strata", 
           linetype = "strata", 
           xlab = "Time since the start of treatment",
           surv.median.line = "hv", 
           ggtheme = theme_bw(), # 
           palette = c("#E7B800", "#2E9FDF"))



# Cumulative hazards
plot(km_fit, cumhaz=TRUE, xlab="Time since the start of treatment",
     ylab="Cumulative hazard")
# to put confidence intervals only at certain time points
plot(km_fit, cumhaz=TRUE, col=1:2, conf.times=c(10, 20, 30),
     xlab="Time since the start of treatment", ylab="Cumulative hazard")

# using survminer with fun = "cumhaz"
ggsurvplot(km_fit,
           fun = "cumhaz",
           pval = FALSE, conf.int = FALSE,
           risk.table = TRUE, 
           risk.table.col = "strata", 
           linetype = "strata", 
           xlab = "Time since the start of treatment",
           ggtheme = theme_bw(), # 
           palette = c("#E7B800", "#2E9FDF"))


plot(km_fit2, cumhaz=TRUE, xlab="Time since the start of treatment", 
     ylab="Cumulative hazard")
# to put confidence intervals only at certain time points
plot(km_fit2, cumhaz=TRUE, col=1:2, conf.times=c(10, 20, 30),
     xlab="Time since the start of treatment", ylab="Cumulative hazard")

ggsurvplot(km_fit2,
           fun = "cumhaz",
           pval = FALSE, conf.int = FALSE,
           risk.table = TRUE, 
           risk.table.col = "strata", 
           linetype = "strata", 
           xlab = "Time since the start of treatment",
           ggtheme = theme_bw(), # 
           palette = c("#E7B800", "#2E9FDF"))


###########################################
# extra
###########################################

# cox regression example
res <- coxph(Surv(timerelmp, Status) ~ groups, data=Day_1_drug6mp)
summary(res)

# test cox proportional hazards assumption
zfit <- cox.zph(res); plot(zfit)
# or with 2nd way using the survminer package
ggcoxzph(zfit)

# log-rank test
survdiff(Surv(timerelmp, Status) ~ groups, Day_1_drug6mp)


# follow-up with reverse Kaplan-Meier
library(prodlim)
quantile(prodlim(Hist(timerelmp, Status)~1, data = Day_1_drug6mp,
                 reverse=TRUE))
# 2nd way
## global estimate
KM0 <- survfit(Surv(timerelmp, Status == 0) ~ 1,  type="kaplan-meier",
               conf.type="log-log", data=Day_1_drug6mp)
KM0




