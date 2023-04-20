### Script prepared for MLSS by Lewin Schmitt
### Based on: https://hastie.su.domains/ISLR2/ISLRv2_website.pdf

install.packages("ISLR2")
install.packages("glmnet") # the glmnet() package has fast implementations to fit ridge regression models, lasso models, and more
# input: x matrix + y vector, does not use the y ∼ x syntax

library(tidyverse)
library(glmnet)
library(ISLR2)

### Inspect and prepare the data
View(Hitters) # this is a sample dataset from the ISLR2 package
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) # to see how many rows have missing salary information
Hitters <- na.omit(Hitters) # to filter out those rows
dim(Hitters)


y <- Hitters$Salary # define the outcome we want to predict 
x <- model.matrix(Salary ~ ., Hitters)[, -1] # construct a matrix with all the inputs

# Note: model.matrix() produces a matrix corresponding to all predictors.
# Moreover, it automatically transforms any qualitative variables into dummy variables.
# This is important because glmnet() can only take numerical, quantitative inputs.

#glmnet() can be controlled by setting alpha to =0 (ridge) or =1 (lasso)

### 1. Ridge Regression ----

grid <- 10^seq(10, -2, length = 100) # optionally, we can define our own grid for lambda
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # here, we already fit the model to the data

###
dim(coef(ridge.mod))
### peak at some of the coefficients for a given lambda
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
### and for a lower lambda
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

### how well is our model doing? Let's have a look!

# First, we need to split our data into training and test sets (50/50)
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

### Then we train the model
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

### Now, let's use the trained model to predict Salary on our test data
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])

# What's the MSE of this model?
mean((ridge.pred - y.test)^2)

# What's our benchmark? Mean of all data?
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ]) # setting such a low lambda cancels all predictors, only intercept remains 
mean((ridge.pred - y.test)^2)

# What's our benchmark? Regular OLS?
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
                      exact = T, x = x[train, ], y = y[train]) # setting such a high lambda diminishes the ridge effect
mean((ridge.pred - y.test)^2)


### Better to use CV for determining lambda. cv.glmnet() is our friend here:

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

### What's our test MSE with this lambda?
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x[test, ])
mean((ridge.pred - y.test)^2)

### Ridge keeps all the parameters (unlike Lasso, which effectively performs variable selection, see next section)
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]

### So Ridge is better than null model & least squares.
### Can we improve our predictions even further with Lasso?

### 2. The Lasso ----

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid) # note that we set alpha to 1, thus switching the behaviour of glmnet to lasso

# Variable selection in play! Compare this to the ridge
plot(lasso.mod)
plot(ridge.mod)

### Fitting glmnet with Lasso and CV:

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef == 0]
lasso.coef[lasso.coef != 0]
