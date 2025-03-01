# Load packages used in this file
library(boot)
library(leaps)
library(ISLR)
library(gam)
library(tree)
library(randomForest)
library(gbm)
library(glmnet)

set.seed(1)

# JWHT is referred as to An Introduction to Statistical Learning with Applications in R 
# by Gareth James, Daniela Witten, Trevor Hastie, and Robert Tibshirani. (Details see reference in README.md)

# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------

# Question 1 (based on JWHT Chapter 5, Problem 8)

# In this problem, you will perform cross-validation on a simulated data set.

y <- rnorm(100)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

# Number of the data created
length(y)

# Number of predictors is 2, which are x and y.
   
# Scatterplot of x against y
plot(x,y)

# Compute the LOOCV errors that result from fitting the following four models using least squares:
# 1. $Y = \beta_0 + \beta_1 X + \epsilon$
Data = data.frame(x, y)
fit.glm.1 = glm(y ~ x)
delta1 = cv.glm(Data, fit.glm.1)$delta[1]
delta1

# 2.  $Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \epsilon$
fit.glm.2 = glm(y ~ poly(x, 2))
delta2 = cv.glm(Data, fit.glm.2)$delta[1]
delta2
  
# 3.  $Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \beta_3 X^3 + \epsilon$
fit.glm.3 = glm(y ~ poly(x, 3))
delta3 = cv.glm(Data, fit.glm.3)$delta[1]
delta3

# 4.  $Y = \beta_0 + \beta_1 X + \beta_2 X^2 + \beta_3 X^3 + \beta_4 X^4 + \epsilon$
fit.glm.4 = glm(y ~ poly(x, 4))
delta4 = cv.glm(Data, fit.glm.4)$delta[1]
delta4


# Compare the four LOOCV error 
which.min(c(delta1,delta2,delta3,delta4))
# So, the second model, which is a quadratic one, has the smallest LOOCV error.
# It is because y is a quadratic function.

# Comment on the statistical significance of the coefficient estimates 
# that results from fitting each of the models using least squares. 
summary(fit.glm.4)
# From the column of p-value, it clearly shows that the linear and quadratic terms 
# are statistically significants and that the cubic and 4th degree terms are not statistically significants. 
# Thus, we can say that the results agree with cross-validation results.



# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------

# Question 2 (based on JWTH Chapter 7, Problem 10)

# The question refers to the 'College' data set

attach(College)

# Split the data into a training set and a test set
set.seed(personal)
train = sample(1:nrow(College), nrow(College) / 2)
College.train = College[train, ]
College.test = College[-train, ]

# Using out-of-state tuition as the response 
# and the other variables as the predictors
fit = regsubsets(Outstate ~ ., data = College.train, 
                  nvmax = length(College)-1, method = "forward")
fit.summary = summary(fit)

# Determine how many variables will be used in the model
par(mfrow = c(1, 3))
# Mallow's Cp 
plot(fit.summary$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")
min.cp = min(fit.summary$cp)
std.cp = sd(fit.summary$cp)
abline(h = min.cp + .2 * std.cp, col = "red", lty = 2)
abline(h = min.cp, col = "blue", lty = 3)

# Bayesian Information Criterion (BIC)
plot(fit.summary$bic, xlab = "Number of variables", ylab = "BIC", type='l')
min.bic = min(fit.summary$bic)
std.bic = sd(fit.summary$bic)
abline(h = min.bic + .2 * std.bic, col = "red", lty = 2)
abline(h = min.bic, col = "blue", lty = 3)

# Adjusted R2
plot(fit.summary$adjr2, xlab = "Number of variables", 
     ylab = "Adjusted R2", type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(fit.summary$adjr2)
std.adjr2 = sd(fit.summary$adjr2)
abline(h = max.adjr2, col = "blue", lty = 3)
abline(h = max.adjr2 - .2 * std.adjr2, col = "red", lty = 2)


# From these graphs using Mallow's Cp and BIC, 6 is the minimum number for variables, 
# which has the minimum error for the subset within 0.2 standard devitations.


# I choose forward stepwise selection,  
# and find out the six varables 
fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coeffs = coef(fit, id = 6)
names(coeffs)
# The six variables selected are PrivateYes, Room.Board, PhD, perc.alumni, Expend, and Grad.Rate.



# Fit a GAM on the training data, using out-of-state tuition as the response 
# and the features selected in the previous step as the predictors, using splines of each feature with 5 df.

# A model using spline based smooths
# Each feature with 5 df
fit = gam(Outstate ~ Private + s(Room.Board, df = 5) + s(PhD, df = 5) 
          + s(perc.alumni, df = 5) + s(Expend, df = 5) + s(Grad.Rate, df = 5), data=College.train)
par(mfrow = c(2, 3))
plot(fit, se = T, col = "blue")


# Evaluate the model obtained on the test set, and explain the results obtained
# Predict the out-of-state tuition using test set
preds = predict(fit, College.test)

# Calculate mean sqaured error
mse = mean((College.test$Outstate - preds)^2)
print (paste("The mean squared error is", 
             round(mse,4), "using GAM with 6 predictors." ))

# Calculate r-squared
rss = sum((College.test$Outstate - preds)^2)
tss = sum((College.test$Outstate - mean(College.test$Outstate))^2)
r2 = 1 - rss / tss
print (paste("The R-squared is", 
             round(r2,4), "using GAM with 6 predictors."))

summary(fit)
# ANOVA shows a significant evidence of non-linear relationship between “Outstate” and “Expend”, 
# and a moderate non-linear relationship between the response and PhD, 
# whose p-value is less than 0.1, between ”Outstate" and “Room.Board”. 
# Other three variables might have linear relationship with "Outstate".

detach(College)

# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------

# Question 3 (based on JWHT Chapter 7, Problem 6)

# In this exercise, you will further analyze the `Wage` data set.

# Perform polynomial regression to predict `wage` using `age.` 
# Use cross-validation to select the optimal degree d for the polynomial. 

attach(Wage)

# Use cross-validation to select the optimal degree d for the polynomial
deltas = rep(NA, 10)
for (i in 1:10) {
    fit = glm(wage ~ poly(age, i), data = Wage)
    deltas[i] = cv.glm(Wage, fit, K = 10)$delta[1]
}

# Plot optimal degree d for the polynomial
par(mfrow = c(1, 1))
plot(1:10, deltas, xlab = "Degree", xlim = c(1,10), ylab = "Test MSE", type = "l")
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)
# d=7 is the optimal degree for the polynomial. 

# Plot of the resulting polynomial fit to the data
plot(wage ~ age, data = Wage, col = "darkgrey")
age.grid = seq(from = min(Wage$age), to = max(Wage$age))
fit = lm(wage ~ poly(age, 7), data = Wage)
preds = predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

# Fit a step function to predict `wage` using `age`, 
# and perform cross-validation to choose the optimal number of cuts. Make a plot of the fit obtained.

# Perform cross-validation to choose the optimal number of cuts
cvs = rep(NA, 10)
for (i in 2:10) {
    Wage$age.cut = cut(Wage$age, i)
    fit = glm(wage ~ age.cut, data = Wage)
    cvs[i] = cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "MSE", type = "l")
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)
# The error is minimum when there are 8 cuts, so we make 8 cuts into age.

# Plot of the fit obtained
plot(wage ~ age, data = Wage, col = "darkgrey", xlab = "Age", ylab = "Wage")
age.grid = seq(from = min(Wage$age), to = max(Wage$age))
fit = glm(wage ~ cut(age, 8), data = Wage)
preds = predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

detach(Wage)

# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------

# Question 4 (based on JWHT Chapter 8, Problem 8)

# In the lab, a classification tree was applied to the `Carseats` data set 
# after converting Sales into a qualitative response variable. 
# Now we will seek to predict Sales using regression trees and related approaches, 
# treating the response as a quantitative variable.

# Split the data set into a training set and a test set.


# Split the data set
train = sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

# Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?
# Fit a regression tree to the training set
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

# Plot the tree
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# From the summary, it shows that number of terminal nodes are 18. 
# Also, from the tree, it shows "price < 94.5" has more nodes than "price <117".

# Test MSE
tree.pred = predict(tree.carseats, newdata = Carseats.test)
mse = mean((tree.pred - Carseats.test$Sales)^2)
print (paste("The test MSE is approximately", round(mse,4) ))


# Use cross-validation in order to determine the optimal level of tree complexity. 
# Does pruning the tree improve the test MSE?

# Perform cross-validation and plot
cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min = which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)
# The model with the lowest deviance is at size=8 which will prune the tree and simplify the model.

prune.carseats = prune.tree(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

pruned.pred = predict(prune.carseats, Carseats.test)
mse.prune = mean((pruned.pred - Carseats.test$Sales)^2)
print (paste("The test MSE is approximately", round(mse.prune,4) ))


# Use the bagging approach in order to analyze this data. 
# What test MSE do you obtain? 


# Use the bagging approach
bag.carseats = randomForest(Sales ~ ., data = Carseats.train, 
                            mtry = 10, ntree = 500, importance = TRUE)
yhat.bag = predict(bag.carseats, newdata = Carseats.test)
mse = mean((yhat.bag - Carseats.test$Sales)^2)
print (paste("The test MSE is approximately", round(mse,4) ))

# Use importance function to determine which variables are most important
importance(bag.carseats)
detach(Carseats)
# "Price" and "ShelveLoc" are the two most important variable in this function using bagging approach.



# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------
# Question 5 (based on JWTH Chapter 8, Problem 10)

# Use boosting (and bagging) to predict Salary in the Hitters data set
attach(Hitters)

# Remove the observations for which salary is unknown, and then log-transform the salaries
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

# Split the data into training and testing sets for cross validation purposes.
train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

# Perform boosting on the training set with 1000 trees 
# for a range of values of the shrinkage parameter $\lambda$.  
# Produce a plot with different shrinkage parameters on the x-axis 
# and the corresponding training set MSE on the y-axis

# Give shrinkage parameters 'lambda'
lambdas = 10^seq(0, 0.2, by = 0.001)

train.err = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  
    boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                        n.trees = 1000, shrinkage = lambdas[i])
    pred.train = predict(boost.hitters, Hitters.train, n.trees = 1000)
    train.err[i] = mean((pred.train - Hitters.train$Salary)^2)
}

# Plot with different shrinkage parameters on the x-axis 
# and the corresponding training set MSE on the y-axis
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")


# Produce a plot similar to the last one, but this time using the test set MSE 
test.err = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  
    boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                         n.trees = 1000, shrinkage = lambdas[i])
    yhat = predict(boost.hitters, Hitters.test, n.trees = 1000)
    test.err[i] = mean((yhat - Hitters.test$Salary)^2)
}

# Plot with different shrinkage parameters on the x-axis 
# and the corresponding training set MSE on the y-axis
plot(lambdas, test.err, type = "b", xlab = "Shrinkage values", ylab = "Test MSE")

mse_boosting = round(min(test.err), 4)
paste("The MSE using boosting",mse_boosting)


# Fit the model using two other regression techniques (from previous classes) 
# and compare the MSE of those techniques to the results of these boosted trees.

# Linear regression
fit1 = lm(Salary ~ ., data = Hitters.train)
pred1 = predict(fit1, Hitters.test)
mse_linear = round(mean((pred1 - Hitters.test$Salary)^2), 4)
paste("The MSE using linear regression",mse_linear)

# Ridge regression
x.train = model.matrix(Salary ~ ., data = Hitters.train)
x.test = model.matrix(Salary ~ ., data = Hitters.test)
y = Hitters.train$Salary
fit2 = glmnet(x.train, y, alpha = 0)
pred2 = predict(fit2, s = 0.01, newx = x.test)

mse_ridge = round(mean((pred2 - Hitters.test$Salary)^2), 4)
paste("The MSE using ridge regression",mse_ridge)

# Compared with using linear regression and ridge regression, the test MSE using boosting is the lowest.

(f) Reproduce (c) and (d), but this time use bagging instead of boosting and compare to the boosted MSE's and the MSE's from (e)

# Bagging 
bag.hitters = randomForest(Salary ~ ., data = Hitters.train, 
                            mtry = length(Hitters)-1, ntree = 500)
yhat.bag = predict(bag.hitters, newdata = Hitters.test)

mse_bag = round(mean((yhat.bag - Hitters.test$Salary)^2), 4)
paste("The MSE using bagging",mse_bag)

# Compare linear regression, ridge regression, and bagging approach
df = data.frame(mse_boosting, mse_linear, mse_ridge, mse_bag)
colnames(df) = c("Boosting", "Linear", "Ridge", "Bag")
rownames(df) = "test MSE"
df

detach(Hitters)
# The test MSE using bagging is the minimum, and then using boosting. 
# The test MSE using linear regression is the largest.
