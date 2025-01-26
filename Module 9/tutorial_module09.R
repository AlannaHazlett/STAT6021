library(leaps)

Data<-mtcars
############################
## Check variables to see if there is categorical variables-are they dummy coded or do you need to force factor()?
############################
?mtcars
# In the information for the dataset we see [, 8]	vs	Engine (0 = V-shaped, 1 = straight) [, 9]	am	Transmission (0 = automatic, 1 = manual) are already dummy coded for us.


############################
##all possible regressions##
############################

##perform all possible regressions (nbest=1) nbest default = 1
allreg <- leaps::regsubsets(mpg ~., data=Data, nbest=1)
summary(allreg)

##perform all possible regressions (nbest=2)
allreg2 <- leaps::regsubsets(mpg ~., data=Data, nbest=2)
summary(allreg2)
# Shows first and second best models for a one predictor model, 2 predictor model, etc. Two lines of output for each.

##see what can be extracted
names(summary(allreg2))

##find model with best according to different criteria
which.max(summary(allreg2)$adjr2)
which.min(summary(allreg2)$cp)
which.min(summary(allreg2)$bic)

##find coefficients and predictors of model with best
##adj r2, cp, bic
coef(allreg2, which.max(summary(allreg2)$adjr2))
coef(allreg2, which.min(summary(allreg2)$cp))
coef(allreg2, which.min(summary(allreg2)$bic))

#########################################################
##Forward selection, backward elimination, stepwise reg##
#########################################################

##intercept only model
regnull <- lm(mpg~1, data=Data)
##model with all predictors
regfull <- lm(mpg~., data=Data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
# It is coincidence that all three generate the same candidate model, this is not always the case.
#---------------------------------------------------------------------------------------------------
# We MUST check the regression assumptions for our candidate model(s)
regcand<-lm(mpg~wt+cyl+hp,data=Data)
#par(mfrow=c(2,2))
plot(regcand)
acf(regcand$residuals, main="ACF Plot of Residuals")
boxcox(regcand)
# Assumption 1: Do the errors have mean of 0 for each value of the predictor -No
# Assumption 2: Do the errors have constant variance for each value of the predictor -No
# Assumption 3: Are the errors independent (acf plot) - Yes
# Assumption 4: Are the errors normally distributed? -Mostly

# We want to address both assumption 1 & 2 so we transform y variable, we see our boxcox plot has 0, so let's do log(y)
ystar<-log(Data$mpg)
Data<-data.frame(Data,ystar)
  
# Rerun regsubsets() and step() to see what candidate models are suggested
# need to define intercept only and full models again based on ystar
Data2<-Data[,2:12]
regnull2 <- lm(ystar~1, data=Data2)
regfull2 <- lm(ystar~., data=Data2)

##forward selection, backward elimination, and stepwise regression
step(regnull2, scope=list(lower=regnull2, upper=regfull2), direction="forward")
step(regfull2, scope=list(lower=regnull2, upper=regfull2), direction="backward")
step(regnull2, scope=list(lower=regnull2, upper=regfull2), direction="both")
#forward result: ystar ~ wt + hp + cyl
#backward result: ystar ~ hp + wt + qsec + gear
#stepwise result: ystar ~ wt + hp + cyl

# Check regression assumptions for regression candidates
regcand2<-lm(ystar ~ wt + hp + cyl,data=Data2)
#par(mfrow=c(2,2))
plot(regcand2)
acf(regcand2$residuals, main="ACF Plot of Residuals")
boxcox(regcand2)
# Assumptions 1,2, 4 seem better
# Assumption 3 seems slightly worse, with one value outside CV, but no big deal
# Interestingly we may need a stronger transformation as boxcox still shows 0 in lambda CI


regcand3<-lm(ystar ~ hp + wt + qsec + gear,data=Data2)
#par(mfrow=c(2,2))
plot(regcand3)
acf(regcand3$residuals, main="ACF Plot of Residuals")
boxcox(regcand3)
# Assumption 1,2, 4 seem better, but aren't good enough
# Assumption 3 seems slightly worse, with one value outside CV, but no big deal
# Boxcox confirms we need another transformation on lambda