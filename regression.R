#################################################################################################
### Linear Regression
head(mtcars)

### Build the linear model 
lmModel <- lm(mpg ~ wt, data = mtcars)

### Use the linear model to make prediction
predValue <- predict(lmModel, data.frame(wt = 3))
predValue <- predict(lmModel, data.frame(wt = c(3,4)))
predValue <- predict(lmModel, data.frame(wt = mtcars$wt))

### Access the model parameters
coef(lmModel)
resid(lmModel)
plot(mtcars$wt, predValue, type = "l")
points(mtcars$wt, predValue+resid(lmModel))

### Model Performance assessment
sumModel <- summary(lmModel)
sumModel$r.squared
r <- cor(mtcars$mpg, mtcars$wt)
r^2      # r.squared is literally the r (correlation coefficient) squared

### Build models with  more than one predictors
lmModel2 <- lm(mpg ~ wt + hp + disp, data = mtcars) # wt, hp, and disp will be used as predictor
lmModel3  <- lm(mpg ~ ., data = mtcars)   # All the variables will be used

sumModel <- summary(lmModel2)
sumModel$r.squared

sumModel <- summary(lmModel3)
sumModel$r.squared

################################################################################
# factor data 1
InsectSprays
boxplot(count ~ spray, data = InsectSprays)
lm1 <- lm(count ~ spray, data = InsectSprays)
coef(lm1)
means <- tapply(InsectSprays$count, InsectSprays$spray, mean)
means
means - means[1]

# factor data 2
class(mtcars$cyl)
lm2 <- lm(mpg ~ wt + cyl, data = mtcars)
coef(lm2)

lm3 <- lm(mpg ~ factor(cyl), data = mtcars)
coef(lm3)
means <- tapply(mtcars$mpg, mtcars$cyl, mean)
means - means[1]

lm4 <- lm(mpg ~ wt + factor(cyl), data = mtcars)
coef(lm4)


####################################################################
# Polynomial regression
head(mtcars)

lm1 <- lm(mpg ~ wt, data = mtcars)
lm2 <- lm(mpg ~ poly(wt,2), data = mtcars)
lm3 <- lm(mpg ~ poly(wt,3), data = mtcars)
lm4 <- lm(mpg ~ poly(wt,4), data = mtcars)

anova(lm1,lm2,lm3,lm4)
# p value comparing the linear model 1 to quadratic model 2 is close to zero
# indicating that a linear fit is not sufficient. Model 3 and 4 are unnecessary
# as the p-values are very high
coef(lm2)
summary(lm2)
max(mtcars$wt)
min(mtcars$wt)
wtGrid <- seq(1.5, 5.5, length.out = 50)
prediction <- predict(lm2, newdata = list(wt = wtGrid))
plot(x=mtcars$wt, y = mtcars$mpg, pch = 19, col= "blue", xlab = "wt", ylab = "mpg",
     cex.lab = 2.0, cex.axis = 1.5, cex = 1.5) 
points(x= wtGrid, y = prediction, type="l", col="green", lwd = 2)

####################################################################
## Generalized additive model

drange <- range(mtcars$disp)
dispGrid <- seq(drange[1], drange[2], length.out = 50)
wtGrid <- seq(1.5, 5.5, length.out = 50)

library(gam) 
gam1 <- gam(mpg ~ s(wt,2) + disp, data = mtcars)
gam2 <- gam(mpg ~ s(wt,2) + s(disp,2), data = mtcars)
anova(gam1, gam2)
prediction1 <- predict(gam1, newdata = list(wt = wtGrid, disp = dispGrid))
prediction2 <- predict(gam2, newdata = list(wt = wtGrid, disp = dispGrid))
plot(x=mtcars$wt, y = mtcars$mpg, pch = 19, col= "blue", xlab = "wt", ylab = "mpg") 
points(x= wtGrid, y = prediction1, type="l", col="green")
points(x= wtGrid, y = prediction2, type="l", col="red")

##################################################################################################
## Forward selection model
##install.packages("leaps")

library(leaps)
fwdSelection <- regsubsets(mpg ~ ., data = mtcars, method = "forward")
sumFwdSel <- summary(fwdSelection)
names(sumFwdSel)

sumFwdSel$outmat 
ind <- which.max(sumFwdSel$adjr2)
ind
sumFwdSel$which[ind,]
coef(fwdSelection, ind)

###################################################################################################
## Lasso model
## install.packages("glmnet")

library(glmnet)

## 1. Creating training and the test data set
x <- model.matrix(mpg ~ ., mtcars)[,-1] 
y <- mtcars$mpg

set.seed(1) 
inTrain <- sample(1: nrow(x), 0.6*nrow(x)) 
yTest <- y[-inTrain]

## 2. Model training
lModel <- glmnet(x[inTrain,],y[inTrain], alpha=1) # alpha = 0 (ridge)

## 3. Model Prediction
predV <- predict(lModel, newx = x[-inTrain,], s = 2)

## 4. Model visualization
plot(lModel, xvar = "lambda", label = TRUE, lwd =2, 
     cex.axis = 1.5, cex.lab = 1.5)
coef(lModel, s = exp(-1))
coef(lModel, s = exp(-1))[5]

## 5. Model Assesment
mean((predV - yTest)^2)

## 6. Finding the optimal value of lambda using cross-validation
cvModel <- cv.glmnet(x[train,],y[train], alpha=1, nfolds = 5) 
bestlam  <- cvModel$lambda.min 
cvpredV <- predict(cvModel, s=bestlam, newx=x[-inTrain,]) 
mean((cvpredV - yTest)^2) 

## 7. Variation of mean-squared error with the penalty parameter
plot(cvModel)
