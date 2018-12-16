# Load packages used in this file
library(ISLR) 
library(leaps)
library(MASS) 
library(data.table)
library(class)
library(boot)

set.seed(1) 


# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------
# Question 1 (Introduce regsubsets function)

# Use the Auto data set from the textbook's website. 
# When reading the data, use the options as.is = TRUE and na.strings="?". 
# Remove the unavailable data using the na.omit() function.

fix(Auto)

# Load data
Auto = read.table('http://www-bcf.usc.edu/~gareth/ISL/Auto.data',
                  header=TRUE,na.strings='?', as.is = TRUE)

# Number of rows **before** remove unavailable data
nrow(Auto)

# Number of rows **after** remove unavailable data
Auto = na.omit(Auto)
nrow(Auto)

# Remove columns 'origin' and 'name', which are unimportant variables.
usefulAuto = subset(Auto, select = c("mpg","cylinders","displacement",
                                     "horsepower","weight","acceleration","year"))

# To know more information of 'Auto' dataset 
# I want to get the range of each quantitative variable
# so, create a table of the ranges of the variables
rangeAuto = data.frame(sapply(usefulAuto, range))
rownames(rangeAuto) = c("min", "max")
rangeAuto = t(rangeAuto)
rangeAuto

# Then, get the mean and standard deviation of each variable.
msAuto = data.frame(sapply(usefulAuto, mean), sapply(usefulAuto, sd) )
colnames(msAuto) = c("mean", "standard.deviation" )
msAuto


# Create a scatterplot matrix that includes 
# the variables mpg, displacement, horsepower, weight, and acceleration.
pairs( ~ mpg + displacement + horsepower + weight + acceleration, data=usefulAuto,
       panel = panel.smooth, main = "Scatterplot Matrix")


# Using the regsubsets function in the leaps library, 
# regress mpg onto displacement, horsepower, weight, and acceleration.

# I demonstrated best subsets, forward selection and backwards elimination
# to identify a great subset and take in to my statistical model.
# First, Best Subset Selection
BestSubset = regsubsets(mpg ~ displacement + horsepower + weight +
                          acceleration, data = usefulAuto)
t(summary(BestSubset)$which)

# Second, Forward Stepwise Selection
Forward = regsubsets(mpg ~ displacement + horsepower + weight + 
                       acceleration, data = usefulAuto, method="forward")
t(summary(Forward)$which)

# Third, Backward Stepwise Selection
Backward = regsubsets(mpg ~ displacement+ horsepower + weight +
                        acceleration, data = usefulAuto, method="backward")
t(summary(Backward)$which)

# In general, three selection methods might have vary results.
# However, in this case, we have the exactly same results.
print(paste("From these three selections, weight is the most",
  "important variable and horsepower is the second one."))


# mpg and the four predictors, displacement, horsepower, weight, 
# acceleration, might have quadratic relation,
# so I used forward stepwise selection for all predictors 
# (displacement, horsepower, weight, acceleration) up to order 2 (i.e. weight and weight^2).

# Create a table with all predictors up to order 2
# predictors: displacement, horsepower, weight, acceleration
regTable = subset(Auto, select = c("mpg","displacement", "horsepower",
                                   "weight","acceleration"))
regTable$SQdisplacement = with(regTable, displacement^2)
regTable$SQhorsepower = with(regTable, horsepower^2)
regTable$SQweight = with(regTable, weight^2)
regTable$SQacceleration = with(regTable, acceleration^2)

# Forward Stepwise Selection
Forward = regsubsets(mpg ~., data = regTable, method="forward")
Forward.summary = summary(Forward)
t(Forward.summary$which)

# I chose forward stepwise selection to find out three most important vaiables, 
# becasue it chooses a subset of the predictor variables for the final model. 
# That is, predictors are added one at a time beginning with the predictor 
# with the highest correlation with the dependent variable. 
# In this case, the most important variable is **weight**, 
# the second one is **square of weight**, and the third one is **horsepower**.

# Plots showing various optimal numbers of features 
par(mfrow=c(1,1))
plot(Forward,scale="bic")
print(paste("This plot can support my answers.",
            "Weight is always the selected variable,",
            "SQweight is the next, and horsepower is the third one."))



# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------
# Question 2 (Initial data analysis)

# Load in the Boston data set, which is part of the MASS library in R.


fix(Boston)
?Boston # Get information of Boston

# Get sample data of Boston
head(Boston)

# How many rows are in this data set?
paste(nrow(Boston), "rows in Boston data set.")
print("Each row represents one case for a given neighborhood in Boston.")

# How many columns?
paste(length(Boston), "columns in Boston data set.")
for (i in c(1:length(Boston))) {
  print (paste("Column", i, "represents", toupper(names(Boston)[i]), 
               "for each neighborhood in Boston."))
}

# Get the ranges of the variables
rangeBoston = data.frame(sapply(Boston, range))
rownames(rangeBoston) = c("min", "max")
rangeBoston = t(rangeBoston)
print ("The Range of Each Variabls")
rangeBoston

# Use questions to understand the dataset
# Do any of the suburbs of Boston appear to have particularly high crime rates?
attach(Boston)
plot(crim, main="Crime Rate of the Suburbs of Boston", 
     xlab="Suburbs of Boston", ylab="Crime Rates" )
# Yes, from 'Crime Rate of the Suburbs of Boston', 
# it clearly shows there are some suburbs of Boston with particularly 
# high crime rate around 400 on the x-Axis.


# Tax rates?
plot(tax, main="Tax Rates of the Suburbs of Boston", 
     xlab="Suburbs of Boston", ylab="Tax Rates" )
# Yes, from 'Tax Rates of the Suburbs of Boston', 
# it shows there are some suburbs of Boston with particularly high tax rate 
# between 400 and 500 on the x-axis.


# Pupil-teacher ratios?
plot(ptratio, main="Pupil-Teacher Ratio of the Suburbs of Boston", 
     xlab="Suburbs of Boston", ylab="Pupil-Teacher Ratio" )
# No, from 'Pupil-Teacher Ratio of the Suburbs of Boston', 
# there is no suburb of Boston with particularly high Pupil-Teacher Ratio.


# How many of the suburbs in this data set bound the Charles river?
print(paste(sum(chas), "suburbs in this data set bound the Charles river."))


# What is the median pupil-teacher ratio among the towns in this data set?
print(paste("In this data set, the median pupil-teacher ratio among the towns is ",
            median(ptratio), ".", sep=""))


# How many of the suburbs average more than seven rooms per dwelling?
countRM = 0
for (i in 1:nrow(Boston)){
  if (rm[i]>7){
    countRM = countRM+1
  }
  else{countRM = countRM}
}
print(paste("In this data set,", countRM, 
            "suburbs have average more than seven rooms per dwelling."))


# More than eight rooms per dwelling?
countRM = 0
for (i in 1:nrow(Boston)){
  if (rm[i]>8){
    countRM = countRM+1
  }
  else{countRM = countRM}
}
print(paste("In this data set,", countRM, 
            "suburbs have average more than eight rooms per dwelling."))

detach(Boston)


# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------
# Question 3 (Do statistical learning on binary outcomes)

# This question should be answered using the Weekly data set, which is part of the ISLR package. 
# This data contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.


fix(Weekly)
?Weekly # Get more information

head(Weekly)

# Use the full data set to perform a logistic regression 
# with Direction as the response and the five lag variables plus Volume as predictors. 
# Use the summary function to print the results. 

attach(Weekly)
# Logistic regression
fit.glm1 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
               data = Weekly, family = binomial)
summary(fit.glm1)
# From the summary, only the p-value of Lag2 is less than 0.05, 
# which means Lag2 is the only predictor with statistically significant.


# Fit a logistic regression model using a training data period from 1990 to 2008, 
# using the predictors from the previous problem 
# that you determined were statistically significant. 
# Test your model on the held out data (that is, the data from 2009 and 2010) 
# and express its accuracy.

# Set the training data set, whose Year are from 1990 to 2008
trainingdata = (Year < 2009)

# Set the testing data set, whose Year are from 2009 to 2010
# Data of testing data set between Year 2009 and Year 2010
Weekly.testingdata = Weekly[!trainingdata, ]       
# Index of testing data set between Year 2009 and Year 2010
Direction.testingdata = Direction[!trainingdata]   

# -------------------------------------------------------------------- 
# Logistic regression
fit.glm2 = glm(Direction ~ Lag2, data = Weekly, 
               family = binomial, subset = trainingdata)

# Predict Lag2 in tesing data set
pred.prob = predict(fit.glm2, Weekly.testingdata, type = "response")

# Use Direction to test the model accuracy
pred.glm2 = rep("Down", length(pred.prob))
pred.glm2[pred.prob > 0.5] = "Up"
pred.table = table(pred.glm2, Direction.testingdata)
pred.table

# Sensitivity: TP/P = TPR
Sensitivity = pred.table[1,1] / sum(pred.table[,1])
# Specificity: TN/N = TNR
Specificity = pred.table[2,2] / sum(pred.table[,2])
# Accuracy: (TP + TN)/(P + N)
Accuracy = sum(pred.table[1,1],pred.table[2,2]) / sum(pred.table[,])
# Total Error Rate: (FP + FN)/(P + N)
TotalError = sum(pred.table[1,2],pred.table[2,1]) / sum(pred.table[,])

glm.Confusion = data.frame(Sensitivity, Specificity, Accuracy, TotalError)
row.names(glm.Confusion) = "GLM"

print (paste("The accuracy of Logistic regression is", Accuracy))
detach(Weekly)

# -------------------------------------------------------------------- 
# Linear Discriminant Analysis (LDA)
fit.lda = lda(Direction ~ Lag2, data = Weekly, subset = trainingdata)

# Predict Lag2 in tesing data set
pred.prob.lda = predict(fit.lda, Weekly.testingdata)

# Use Direction to test the model accuracy
pred.table.lda = table(pred.prob.lda$class, Direction.testingdata)
pred.table.lda

# Sensitivity: TP/P = TPR
Sensitivity = pred.table.lda[1,1] / sum(pred.table.lda[,1])
# Specificity: TN/N = TNR
Specificity = pred.table.lda[2,2] / sum(pred.table.lda[,2])
# Accuracy: (TP + TN)/(P + N)
Accuracy = sum(pred.table.lda[1,1],pred.table.lda[2,2]) / sum(pred.table.lda[,])
# Total Error Rate: (FP + FN)/(P + N)
TotalError = sum(pred.table.lda[1,2],pred.table.lda[2,1]) / sum(pred.table.lda[,])

lda.Confusion = data.frame(Sensitivity, Specificity, Accuracy, TotalError)
row.names(lda.Confusion) = "LDA"
print (paste("The accuracy of LDA is", Accuracy))

# -------------------------------------------------------------------- 
# Quadratic discriminant analysis (QDA)
fit.qda = qda(Direction ~ Lag2, data = Weekly, subset = trainingdata)

# Predict Lag2 in tesing data set
pred.prob.qda = predict(fit.qda, Weekly.testingdata)

# Use Direction to test the model accuracy
pred.table.qda = table(pred.prob.qda$class, Direction.testingdata)
pred.table.qda

# Sensitivity: TP/P = TPR
Sensitivity = pred.table.qda[1,1] / sum(pred.table.qda[,1])
# Specificity: TN/N = TNR
Specificity = pred.table.qda[2,2] / sum(pred.table.qda[,2])
# Accuracy: (TP + TN)/(P + N)
Accuracy = sum(pred.table.qda[1,1],pred.table.qda[2,2]) / sum(pred.table.qda[,])
# Total Error Rate: (FP + FN)/(P + N)
TotalError = sum(pred.table.qda[1,2],pred.table.qda[2,1]) / sum(pred.table.qda[,])

qda.Confusion = data.frame(Sensitivity, Specificity, Accuracy, TotalError)
row.names(qda.Confusion) = "QDA"

print (paste("The accuracy of QDA is", Accuracy))

# -------------------------------------------------------------------- 
# K - nearest neighbor (KNN)
# Set training data and testing data as matrix, respectively
train.X = as.matrix(Lag2[trainingdata])
test.X = as.matrix(Lag2[!trainingdata])

# K-nearest neighbors algorithm
train.Direction = Direction[trainingdata]

# for K = 1, 2, 3
pred.tables.knn = table(NULL)
Accuracy.table = table(NULL)
for(K in 1:3){
  set.seed(personal)
  pred.knn = knn(train.X, test.X, train.Direction, k = K)
  pred.table.knn = table(pred.knn, Direction.testingdata)
  Accuracy = sum(pred.table.knn[1,1],pred.table.knn[2,2]) / sum(pred.table.knn[,])
  
  # rbind Accuracy table and Confusion table for K=1,2,3
  Accuracy.table = rbind(Accuracy.table, Accuracy)
  pred.tables.knn = rbind(pred.tables.knn, pred.table.knn)
  
}

# Convert pred.tables.knn from **matrix** into **data.table**
predicts = rownames(pred.tables.knn)
pred.tables.knn = data.table(pred.tables.knn)

# Create two columns for the predictions and K, respectively
pred.tables.knn$Predicts = predicts
pred.tables.knn$K = c(1,1,2,2,3,3)

# Swith the order of columns
pred.tables.knn = pred.tables.knn[,c(4,3, 1:2)]
pred.tables.knn

# Rename the rows and columns of the Accuracy table
rownames(Accuracy.table) = c("K=1", "K=2", "K=3")
colnames(Accuracy.table) = "Accurancy"
Accuracy.table = t(Accuracy.table)
Accuracy.table

# -------------------------------------------------------------------- 
# Compare resultes 
Accuracy = c(glm.Confusion$Accuracy, lda.Confusion$Accuracy, 
                 qda.Confusion$Accuracy, Accuracy.table[1], 
                 Accuracy.table[2], Accuracy.table[3])
Methods = c("GLM", "LDA", "QDA", "KNN K=1", "KNN K=2", "KNN K=3")
all.accuracy = data.frame(Methods, Accuracy)
all.accuracy[which(all.accuracy$Accuracy == max(all.accuracy$Accuracy)),]
# Logistic regression and LDA have the highest accuracy 0.625.


# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------

# Write a function of linear regression on this case without using package 'lm'
# Give parameter which will be used following
x1 = rnorm(20)
x2 = rnorm(20) 
output = x1*2 + x2^2 
input = cbind(x1,x2)

estimate_se_coef = function(input, output){
  ## Calculating regression coefficients
  
  # Paramenters
  num_parameters = ncol(input)
  n = length(output)
  x1 = input[,1]
  x2 = input[,2]
  y = output
  
  # Pre-calculate 
  ss_y = sum(y^2) - sum(y)^2/n
  ss_x1 = sum(x1^2) - sum(x1)^2/n
  ss_x2 = sum(x2^2) - sum(x2)^2/n
  ss_x1y = sum(x1*y) - sum(x1)*sum(y)/n
  ss_x2y = sum(x2*y) - sum(x2)*sum(y)/n
  ss_x1x2 = sum(x1*x2) - sum(x1)*sum(x2)/n
  
  # Calculate regression coefficients
  denominator = ss_x1*ss_x2 - ss_x1x2^2
  b_1 = (ss_x2 * ss_x1y - ss_x1x2 * ss_x2y)/denominator 
  b_2 = (ss_x1 * ss_x2y - ss_x1x2 * ss_x1y)/denominator
  b_0 = mean(y) - b_1*mean(x1) - b_2*mean(x2)
  b_table = data.frame(b_0,b_1,b_2)
  
  # Predeict output by the linear regression
  y_pred = b_0 + b_1*x1 + b_2*x2
  
  ss_reg = b_1*ss_x1y + b_2*ss_x2y
  ss_res = ss_y - ss_reg
  
  # Error between y and predict y 
  err = y - y_pred
  # Residual Sum of Squares (RSS)
  rss = sum(err^2)
  # Mean squared error (MSE) 
  mse = rss/length(y)
  # Standard error of the model (SE)
  se = sqrt(rss/(length(y)-num_parameters-1))
  # R squared (R2)
  r2 = ss_reg / ss_y
  # Error between y and predict y 
  Residuals = data.frame(min(err), median(err), max(err))
  
  # Calculate Standard error of coefficients
  cor_x12 = cor(x1,x2)
  s_y_12 = ss_res / (n - num_parameters -1)
  se_b1 = sqrt(s_y_12/ss_x1/(1-cor_x12^2))
  se_b2 = sqrt(s_y_12/ss_x2/(1-cor_x12^2))
  
  result = data.frame(
    Estimate = c(b_0,b_1,b_2),
    Standard_Error = c("", se_b1, se_b2))
  rownames(result) = c("beta_0", "beta_1", "beta_2")
  
  return(result)
}
  
estimate_se_coef(input, output)

# Compare the output of your function to that of the lm command in R.
summary(lm(output~x1+x2))
# I only return the estimates and standard errors of coefficients 
# from my linear regression function. The function of linear regression 
# with two variables I wrote down has the same result of lm package. 
# However, I also got the smae result of other statistical analysis, 
# such as Residual standard error and Adjusted R-squared, as lm package. 


# -----------------------------------------------------------------------------
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
# -----------------------------------------------------------------------------

# Question 5 (Do statistical learning on quantitative data)
# Using the Advertising data set (Sales, TV, Radio, Newspaper), do the following:

# Randomly split the data into two different pieces of roughly equal size.
ads = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")

indx = sample(nrow(ads), nrow(ads)/2)
group1 = ads[indx,]
group2 = ads[-indx,]

head(group2)


# Pick one set to run a linear regression to predict sales based on all TV and Radio, 
# and then test your accuracy using the other set

# Take group1 as training data set
# and group2 as testing data set
fit.linear = lm( sales ~ TV + radio, data = group1)

# Predict Lag2 in tesing data set
pred.prob = predict(fit.linear, group2, type = "response")

plot(group2$sales,pred.prob)

mse = mean((group2$sales - pred.prob)^2)
print (paste("The mean squared error of predict sales is", mse))

# Repeat the previous problem using all three predictors (including newspaper).  
# Take group1 as training data set
# and group2 as testing data set
fit.linear = lm( sales ~ TV + radio + newspaper, data = group1)

# Predict Lag2 in tesing data set
pred.prob = predict(fit.linear, group2, type = "response")

plot(group2$sales,pred.prob)

mse = mean((group2$sales - pred.prob)^2)
print (paste("The mean squared error of predict sales is", mse))
# The mean squared error of the predict sales from all three predictors 
# is larger than the MSE of the predict sales from two predictors, TV and Radio. 
# Thus, using TV and Radio for predicting the sales can get better result.


# Determine the LOOCV error for the linear regression using all three predictors.
glm.fit = glm(sales ~ TV + radio + newspaper ,data = ads)
cv.err=cv.glm(ads,glm.fit)
print (cv.err$delta)
