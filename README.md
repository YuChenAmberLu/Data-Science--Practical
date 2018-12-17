# Introduction of statistical learning 

#### There are two files and five questions for each file for you to practice statistical learning step by step. 



### The first file, StatisticalLearning.r, contains five queations, which cover factor selection, initial analysis, statistical learning on both qualitative and quantitative data. 


#### Question 1 (Introduce regsubsets function)
Demonstrate best subsets, forward selection and backwards elimination to identify a great subset of `Auto` and take in to my statistical model. `Auto` is a dataset on http://www-bcf.usc.edu/~gareth/ISL/Auto.data

> Best subsets

> Forward selection

> Backwards elimination


#### Question 2 (Initial data analysis)
Get ideas how to do initial analysis using `Boston` dataset, which is also a dataset in ISLR.

> How many columns?

> What is the range of each quatitative variables?

> Is there any relation between two variables?

> How many data while given constraints?


#### Question 3 (Do statistical learning on binary outcomes)
Train `Weekly` dataset, which is also a dataset in ISLR, using Logistic regression, Linear Discriminant Analysis, Quadratic discriminant analysis, and K - nearest neighbor.

> Logistic regression

> Linear Discriminant Analysis

> Quadratic discriminant analysis

> K - nearest neighbor


#### Question 4 (Write a function of linear regression without using 'lm') 
Create a function of linear regression on binary outcomes


#### Question 5 (Do statistical learning on quantitative data)
Train, test, and predict sales, which is set as response among others factors of `ads`, using linear regression. `ads` is a dataset on http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv




### The second file, NonlinearRegressions.r, contains five queations, which cover cross-validation, generalized additive models, classification tree, random forest, gradient boosted machines, and regularized generalized linear models on quantitative data. 


#### Question 1 (Cross-validation for generalized linear models)
Compare errors using leave-one-out cross validation on different polynomial terms 


#### Question 2 (Generalized additive models)
Do statistical learning on out-of-state tuition against other factors in `College` dataset using gam. `College` is a dataset in ISLR.


#### Question 3 (Do statistical learning on binary outcomes)
Perform cross-validation to choose the optimal number of cuts on Age, a factor in `Wage` dataset, and then find a fit regression to predict wage.


#### Question 4 (Classification tree and random forest) 
In the lab, a classification tree was applied to the `Carseats` data set after converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable.


#### Question 5 (Gradient boosted machines and regularized generalized linear models)
Use boosting (and bagging) to predict Salary in the `Hitters`, a dataset in ISLR. Aslo, compare the mean-squared errors of linear regression and ridge regression. 


### Reference: 

Gareth James, Daniela Witten, Trevor Hastie, and Robert Tibshirani. An Introduction to Statistical Learning with Applications in R. Springer, 2014. **(1)**
This book is available free from the author's web site at http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Fourth%20Printing.pdf (Links to an external site.)Links to an external site. 


Trevor Hastie, Robert Tibshirani, and Jerome Friedman. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. Second Edition., Springer (Tenth Printing) 2013. **(2)**
This book is available free from the author's web site at http://statweb.stanford.edu/~tibs/ElemStatLearn/printings/ESLII_print10.pdf

