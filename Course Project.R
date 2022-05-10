install.packages('dplyr', 'ggplot2', 'olsrr', 'PerformanceAnalytics')
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

#Text Files read into tables

boboa <- read.table(file="BOBOA.txt", sep="\t", quote="", header = T)

cheecarocks <- read.table(file="CHEECAROCKS.txt", sep="\t", quote="", header = T)

keo <- read.table(file="KEO.txt", sep="\t", quote="", header = T)

#BOBOA Multi Linear Regression Model Information

#BOBOA multi linear regression model
boboa.mod = lm(pH_sw ~ pCO2_sw + pCO2_air + SST + SSS, data = boboa)
summary(boboa.mod)

#Finding and viewing the residuals for the normality check
boboares.full <- rstandard(boboa.mod)
boboafit.full <- fitted.values(boboa.mod)

qqnorm(boboares.full, pch=19, col="grey50")
qqline(boboares.full)

#Finding and viewing the residuals for the linear, equal variance and independent checks
plot(boboafit.full,boboares.full, pch=19, col="grey50")
abline(h=0)

# Making a correlation matrix to check for multicolinearity
boboareg.data <- data.frame(boboa$pCO2_sw, boboa$pCO2_air, boboa$SST, boboa$SSS)
chart.Correlation(boboareg.data, histogram=TRUE, pch=19)

# View the full step model results
boboafull.step <- ols_step_forward_aic(boboa.mod)
boboafull.step 

#Viewing the AIC of the stepwise forward selection to conclude our model is the best model
boboafull.step$model
plot(boboafull.step )

#Cheecarocks Multi Linear Regression Information

#Cheecarocks multi linear regression model
cr.mod = lm(pH_sw ~ pCO2_sw + pCO2_air + SST + SSS, data = cheecarocks)

summary(cr.mod)

#Finding and viewing the residuals for the normality check
crres.full <- rstandard(cr.mod)
crfit.full <- fitted.values(cr.mod)

qqnorm(crres.full, pch=19, col="grey50")
qqline(crres.full)

#Finding and viewing the residuals for the linear, equal variance and independent checks
plot(crfit.full,crres.full, pch=19, col="grey50")
abline(h=0)

# Making a correlation matrix to check for multicolinearity
crreg.data <- data.frame(cheecarocks$pCO2_sw, cheecarocks$pCO2_air, cheecarocks$SST, cheecarocks$SSS)
chart.Correlation(crreg.data, histogram=TRUE, pch=19)

# View the full step model results
crfull.step <- ols_step_forward_aic(cr.mod)
crfull.step 

#Viewing the AIC of the stepwise forward selection to conclude our model is the best model
crfull.step$model
plot(crfull.step )

#KEO Multi Linear Regression Information

#KEO multi linear regression model
keo.mod = lm(pH_sw ~ pCO2_sw + pCO2_air + SST + SSS, data = keo)
summary(keo.mod)

#Finding and viewing the residuals for the normality check
keores.full <- rstandard(keo.mod)
keofit.full <- fitted.values(keo.mod)

qqnorm(keores.full, pch=19, col="grey50")
qqline(keores.full)

#Finding and viewing the residuals for the linear, equal variance and independent checks
plot(keofit.full,keores.full, pch=19, col="grey50")
abline(h=0)

# Making a correlation matrix to check for multicolinearity
keoreg.data <- data.frame(keo$pCO2_sw, keo$pCO2_air, keo$SST, keo$SSS)
chart.Correlation(keoreg.data, histogram=TRUE, pch=19)

# View the full step model results
keofull.step <- ols_step_forward_aic(keo.mod)
keofull.step 

#Viewing the AIC of the stepwise forward selection to conclude our model is the best model
keofull.step$model
plot(keofull.step )

