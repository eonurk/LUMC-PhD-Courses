    # install.packages("haven")
    # install.packages("survival")

    library(haven)
    library(survival)
    library(survminer)

### Question 3 - Kaplan Meier plot

    # load the data
    df <- read_sav("Day 1_data set - 61162_mice.sav")

    Surv(df$Time, df$Death)

    ##  [1]  30   50   50+  51   66+  82   92  120+ 140  150  180  190

    fit.KM <- survfit(Surv(Time, Death) ~ 1, data = df)

    ggsurvplot(
        fit = fit.KM, 
        xlab = "Hours", 
        ylab = "Overall survival probability", risk.table = T)

![](Exercises-Day1_files/figure-markdown_strict/Kaplan-Meier%20curve-1.png)

### Question 4 - Estimate the survival and cumulative hazard functions

    df2 <- read_sav("Day 1 - drug6mp.sav")

    min(df2$timerelmp)

    ## [1] 6

    max(df2$timerelmp)

    ## [1] 35

    fit.KM2 <- survfit(Surv(timerelmp, Status) ~ groups, data = df2)

    ggsurvplot(
        fit = fit.KM2, 
        xlab = "Hours", 
        ylab = "Overall survival probability", risk.table = T, conf.int = T)

![](Exercises-Day1_files/figure-markdown_strict/KM%20curve-1.png)
