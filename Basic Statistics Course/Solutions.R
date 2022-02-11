library(dplyr)
library(survival)
library(ggplot2)
library(gtsummary)

df <- read.csv2("trial_csv_clean_20171018.csv", header = T, sep = ";", quote = "\"", dec = ",", fill = T)

# survival object
surv.obj <- Surv(as.numeric(df$trand), df$death == "Yes")
surv.obj

# NAC
table(df$code)

# hist of survivals
qplot(agerand, facets = ~ code, fill = code,  data = df)

# mean and sd
df.plb <- df[df$code == "Placebo",]
mean(df.plb$agerand)
sd(df.plb$agerand)

df.nac <- df[df$code == "NAC",]
mean(df.nac$agerand)
sd(df.nac$agerand)

# sex diff
table(df.plb$male)
table(df.nac$male)

# site
df.plb$site

# dist of site R ex 1.4b
table(df.plb$site)
round(prop.table(table(df.plb$site)) * 100)

table(df.nac$site)
round(prop.table(table(df.nac$site)) * 100)

# tonset
hist(df$tonset)

median(df.plb$tonset)
quantile(df.plb$tonset)

round(median(df.nac$tonset),1)
quantile(df.nac$tonset)

# totmrc0
hist(df$totmrc0)

mean(df.plb$totmrc0)
sd(df.plb$totmrc0)

mean(df.nac$totmrc0)
sd(df.nac$totmrc0)


# fvc0
hist(df$fvc0)

mean(df.plb$fvc0)
sd(df.plb$fvc0)

mean(df.nac$fvc0)
sd(df.nac$fvc0)


# barth0
hist(df$barth0)

median(df.plb$barth0)
quantile(df.plb$barth0)

median(df.nac$barth0)
quantile(df.nac$barth0)

# rankin0
hist(df$rankin0)

table(df.plb$rankin0)
prop.table(table(df.plb$rankin0)) *100


table(df.nac$rankin0)
round(prop.table(table(df.nac$rankin0)) *100)


# bulbar0
hist(df$bulbar0)

round(median(df.plb$bulbar0))
round(quantile(df.plb$bulbar0))

round(median(df.nac$bulbar0))
round(quantile(df.nac$bulbar0))


# Exercises 2.1
70 + 1.96 * 28

# Exercises 2.2
#SE
5.8/sqrt(216)

# Ex 2.4.1
34.46 + 1.96 * (9.7/sqrt(600))
34.46 - 1.96 * (9.7/sqrt(600))

# 2.4.2
sqrt(225)*(0.6/1.96)


# R Ex 2.1
hist(df$fvc0)

# Ex 2.2b
summary(df$fvcn0.zscore)
sd(df$fvcn0.zscore)

# Ex 3b
mean(df$fvcn0)

mean(df$fvcn0) - 1.96 * (sd(df$fvcn0)/sqrt(110))
mean(df$fvcn0) + 1.96 * (sd(df$fvcn0)/sqrt(110))

mean(df$fvcn0) + 1.96 * (sd(df$fvcn0)/sqrt(110))

# z-score
(mean(df$fvcn0) - 81)/sd(df$fvcn0) > 1

# Ex 4a
hist(df$fvcn4)
mean(df$fvcn4)

mean(df$fvcn4) - 1.96 * (sd(df$fvcn4)/sqrt(110))
mean(df$fvcn4) + 1.96 * (sd(df$fvcn4)/sqrt(110))

# Ex 5a
mean(df$fvcn4) - 3 * (sd(df$fvcn4)/sqrt(110))
mean(df$fvcn4) + 3 * (sd(df$fvcn4)/sqrt(110))


# 2.6a
mean(df.plb$fvcn4)
mean(df.plb$fvcn4) - 1.96 * (sd(df.plb$fvcn4)/sqrt(nrow(df.plb)))
mean(df.plb$fvcn4) + 1.96 * (sd(df.plb$fvcn4)/sqrt(nrow(df.plb)))

mean(df.nac$fvcn4)
mean(df.nac$fvcn4) - 1.96 * (sd(df.nac$fvcn4)/sqrt(nrow(df.nac)))
mean(df.nac$fvcn4) + 1.96 * (sd(df.nac$fvcn4)/sqrt(nrow(df.nac)))

# Exercise 3
boxplot(df$fvcn0, df$totmrc0)

# ex 3.2
hist(df$fvcn0)
hist(df$totmrc0)

# ex 3.3b
t.test(fvcn0 ~ male, data = df, var.equal = T)

# ex 3.5
summary(aov(fvcn0 ~ male, data = df))

# ex 3.6
df %>% group_by(site) %>% summarise(mean(fvcn0))
summary(aov(fvcn0 ~ site, data = df))

# ex 3.7b
mean(df$fvcn0)
mean(df$fvcn4)
t.test(df$fvcn0 , df$fvcn4, paired = T)

# ex 3.8
hist(df$totmrc0)
wilcox.test(totmrc0 ~ male, data =df)

# ex 3.9
kruskal.test(totmrc0 ~ site, data =df)

## Post-hoc tests comparing totmrc0 for site = arm and site = leg
wilcox.test(df$totmrc0[df$site=='Arm'], df$totmrc0[df$site=='Leg'])

## Post-hoc tests comparing totmrc0 for site = arm and site = bulbar
wilcox.test(df$totmrc0[df$site=='Arm'], df$totmrc0[df$site=='Bulbar'])

## Post-hoc tests comparing totmrc0 for site = arm and site = bulbar
wilcox.test(df$totmrc0[df$site=='Leg'], df$totmrc0[df$site=='Bulbar'])

# ex 3.10
hist(df$height)
summary(aov(height ~ site , data = df))

# ex 4.3.1

# risk ratio
(120/200)/(50/150)
# odds ratio
(120/80)/(50/100)

#ex 4.3.2
# RR
(120/600)/(50/650)
# OR
(120/480)/(50/600)

# ex 4.3.3
# RR
(120/120120)/(50/100050)
# OR
(120/120000)/(50/100000)

# R ex 4.1a
round(prop.table(table(df$code, df$death12), 1),digits = 2)



# R ex 4.3.3
chisq.test(df$code, df$death12, correct = F)

# R ex 4.4c
library(epitools)
tbl <- table(df$code, df$death12)
epitab(tbl, rev = "columns", method = "riskratio")
epitab(tbl, rev = "columns")

# R ex 4.5a
prop.table(table(df$death12, df$diagerand), margin = 2)

chisq.test(df$death12, df$diagerand, correct = F)

# R ex 4.6.6
tbl <- table(df$diagerand, df$death12)
epitab(tbl, rev = "columns", method = "riskratio")
epitab(tbl, rev = "columns", method = "oddsratio")

# ex 5.2.1
50 * 0.549 + 2.507

# R ex 5.1a
plot(df$weight, df$fvc0)
summary(lm(fvc0 ~ weight , data = df))

# R ex 5.1b
plot(df$agerand, df$fvc0)

# R ex 5.2
cor.test(df$weight, df$fvc0)

# R ex 5.3a
cor(df$Rweight, df$Rfvc0)
cor(df$Ragerand, df$Rfvc0)

# R ex 5.3b
cor(df$weight, df$fvc0,method = "spearman")
cor(df$agerand, df$fvc0,method = "spearman")

# R ex 5.5a
summary(lm(fvc0 ~ agerand, data=df))

# R ex 5.5c

model <- lm(fvc0 ~ agerand, data=df)

plot(df$agerand, df$fvc0)
abline(model)

# R ex 5.5d
predict(model, newdata = data.frame(agerand = 50))


# R ex 5.6a
model <- lm(fvc0 ~ weight,data=df)
summary(model)

plot(model)

# Quiz 6.1.3
126.548 + 0.176 * 1 + 0.177 * 73

# R ex 6.1a
lm(fvc0 ~ agerand,data=df)

# R ex 6.1b
lm(fvc0 ~ height,data=df)

# R ex 6.1c
summary(lm(fvc0 ~ height + agerand,data=df))

# R ex 6.2
summary(lm(fvc0 ~ height + agerand,data=df))

# R ex 6.3
summary(lm(fvc0 ~ as.factor(rankin_new),data=df))

predict(lm(fvc0 ~ as.factor(rankin_new),data=df))

# R ex 6.4
summary(lm(fvc0 ~ height + agerand + rankin_new, data=df))

anova(lm(fvc0 ~ height + agerand + rankin_new, data=df))

predict(lm(fvc0 ~ height + agerand + rankin_new, data=df), 
        newdata = data.frame(height = 180, agerand = 50, rankin_new = "rankin 4"))

# R ex 6.5
model_6.5 <-lm(fvc0 ~ height + agerand * rankin_4, data=df)
predict(model_6.5, 
        newdata = data.frame(agerand = 50, rankin_4 = 1, height = 180))

summary(model_6.5)

# R ex 7.1
model_7.1 <- glm(death12 == "Yes" ~ code_ref, family = binomial(link = "logit"), data=df)
summary(model_7.1)

round(exp(coef(model_7.1)), 2)

# R ex 7.2
model_7.2 <- glm(death12 == "Yes" ~ site_ref, family = binomial(link = "logit"), data=df)

round(exp(coef(model_7.2)),3)

anova(model_7.2, test = "Chisq")

# R ex 7.3
model_7.3 <- glm(death12 == "Yes" ~ agerand, family = binomial(link = "logit"), data=df)


anova(model_7.3, test = "Chisq")

round(exp(coef(model_7.3)),2)
round(exp(confint(model_7.3)),2)

# 10 years
round(exp(coef(model_7.3)*10),2)

# R ex 7.4
model_7.4 <- glm(death12 == "Yes" ~ agerand + code_ref, family = binomial(link = "logit"), data=df)
round(exp(coef(model_7.4)),2)
round(exp(confint(model_7.4)),2)

# R ex 8.1
Surv(df$trand, df$death == "Yes") %>% sort

(110-85)/110
#or

summary(survfit(Surv(df$trand, df$death == "Yes") ~ 1))

survfit(Surv(df$trand, df$death == "Yes") ~ 1)

# R ex 8.2
survfit(Surv(trand, death == "Yes") ~ code_ref, data = df)
summary(survfit(Surv(trand, death == "Yes") ~ code_ref, data = df))

survdiff(Surv(trand, death == "Yes") ~ code_ref, data = df)

# R ex 8.3
table(df$diagerand)


survfit(Surv(trand, death == "Yes") ~ diagerand, data = df)
survdiff(Surv(trand, death == "Yes") ~ diagerand, data = df)


# 9.3.1
exp(0.9742)

log(6.655)

exp(0.9742)/1.546

# R ex 9.1
cox_9.1 <- coxph(Surv(trand, death == "Yes") ~ code_ref, data = df)
summary(cox_9.1)

ggsurvplot(survfit(Surv(trand, death == "Yes") ~ code_ref, data = df))

# R ex 9.2
cox_9.2 <- coxph(Surv(trand, death == "Yes") ~ fvc0 , data = df)
summary(cox_9.2)

cox_9.2b <- coxph(Surv(trand, death == "Yes") ~ fvc0 + code_ref, data = df)
summary(cox_9.2b)

# R ex 10.1
head(df[, paste0("barth", 0:1)])

# R ex 10.2
df$barthdiff <- df$barth1 -  df$barth0

summary(df$barthdiff)
# If true people get better, if false worsen
sum(df$barthdiff > -2)/nrow(df) > 50

wilcox.test(barthdiff~code, data=df)

# R ex 10.3
round(mean(df$fvcn0),1)
round(mean(df$fvcn1),1)
round(mean(df$fvcn4),1)

round(sd(df$fvcn0),1)
round(sd(df$fvcn1),1)
round(sd(df$fvcn4),1)

t.test(df$fvcn0, df$fvcn1, paired = T)
t.test(df$fvcn0, df$fvcn4, paired = T)
t.test(df$fvcn4, df$fvcn1, paired = T)

# R ex 10.4
colSums(!is.na(df[,paste0("fvc", c(0,1,4,7,10))]))
colMeans(df[,paste0("fvc", c(0,1,4,7,10))], na.rm = T)
apply(df[,paste0("fvc", c(0,1,4,7,10))], 2, function(x){sd(na.omit(x))})

range(rowSums(!is.na(df[,paste0("fvc", c(0,1,4,7,10))])))
