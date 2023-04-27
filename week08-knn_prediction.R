### This tutorial is adapted from Bruno Castanho Silva (2020), "Introduction to Machine Learning" (ECPR WS)
### The CHES data is from Polk, Jonathan, Jan Rovny, Ryan Bakker, Erica Edwards, Liesbet Hooghe, Seth Jolly, Jelle Koedam, Filip Kostelka, Gary Marks, Gijs Schumacher, Marco Steenbergen, Milada Anna Vachudova and Marko Zilovic. 2017. "Explaining the salience of anti-elitism and reducing political corruption for political parties in Europe with the 2014 Chapel Hill Expert Survey data," Research & Politics (January-March): 1-9.
### See https://www.chesdata.eu/ches-europe#2014-chapel-hill-expert-survey

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, caret)

set.seed(123)

# Let's load the data (make sure to have the csv file in your working directory if you want to work with local files!)
data <- read.csv('2014_CHES_dataset_means.csv')
data <- read.csv('https://www.chesdata.eu/s/2014_CHES_dataset_expert-level.csv')

# For this exercise, we will only work with selected variables:
data <- data %>%
  select(cname, party_name, eu_position, lrgen, galtan, immigrate_policy, urban_rural, environment)

## Let's check out the data. ----
# summary() gives us the basic structure and summary statistics for each variables
summary(data)

# hist() allows to quickly plot a histogram for a given variable
hist(data$eu_position)

# plot() allows to quickly draw a 2d plot for two variables
plot(data$immigrate_policy, data$urban_rural)
cor(data$immigrate_policy, data$urban_rural, use = "complete.obs")

# our outcome of interest is eu_position
plot(data$immigrate_policy, data$eu_position)
cor(data$immigrate_policy, data$eu_position, use = "complete.obs")

# We want to see whether parties are pro- or anti-European (binary outcome)
# need to transform the eu_position coding into a dichotomous variable 
# anti = 0, pro = 1
median(data$eu_position)
data <- data %>%
  mutate(eu_stance = ifelse(eu_position < 5.5, 'anti','pro'))
table(data$eu_stance)

data %>%
  ggplot(aes(x = immigrate_policy, y = urban_rural, colour = eu_stance)) +
  geom_point() +
  scale_colour_manual(values = c('red','blue'))

# Linear model won't work well here. How about a KNN?
# KNN cannot take missing values, so need to filter these out.
knn_data <- data %>%
  select(eu_stance, urban_rural, immigrate_policy) %>%
  na.omit()
predictors <- knn_data[,c('urban_rural','immigrate_policy')]
outcomes <- knn_data[,'eu_stance']

library(class)
eu_predictions_knn <- knn(train = predictors, 
                          test = predictors,
                   cl = outcomes,
                   k = 1)

head(eu_predictions_knn, 10)
table(eu_predictions_knn)
# This looks extremely like the distribution of our original data.
# So let's compare it:
table(eu_predictions_knn, outcomes)
# And indeed, no misclassifications in the extremely flexible model of k=1
# But this would likely be an extreme case of overfitting.
# Let's increase k to decrease the flexibility:

eu_predictions_knn_20 <- knn(train = predictors, 
                          test = predictors,
                          cl = outcomes, 
                          k = 20)
table(eu_predictions_knn_20, outcomes)

# We can use the caret package to get a confusion matrix and some prediction accuracy metrics:
install.packages("caret")
library(caret)
confusionMatrix(eu_predictions_knn_20,factor(outcomes))

# Let's approach the search for k more systematically:
results <- tibble(k = matrix(c(1:nrow(knn_data)),ncol=1),
                  accuracy = NA)

for(k in 1:nrow(knn_data)){
  eu_prediction.temp<-knn(train = predictors, test = predictors,
                          cl = outcomes, k = k)
  results$accuracy[k] <-
    sum(eu_prediction.temp == outcomes)/length(eu_prediction.temp)
}

results %>%
  ggplot(aes(x = k, y = accuracy)) +
  geom_line()

## Cross Validation (LOOCV) ----
# Since we have very few observations, we use LOOCV.

# Create a storage tibble for each iteration of K in KNN
results1 <- tibble(k = matrix(c(1:(nrow(knn_data)-1)),ncol=1),
                   accuracy.loocv = NA)
# Create a storage tibble for each observation in CV. These are called lo (left-out)
results2 <- tibble(lo = matrix(c(1:nrow(knn_data)), ncol=1), 
                   match = NA)
for(k in 1:(nrow(knn_data)-1)){
  for(lo in 1:nrow(knn_data)){
    train.predictors.loocv <- knn_data[-lo,c('urban_rural','immigrate_policy')]
    test.predictors.loocv <- knn_data[lo,c('urban_rural','immigrate_policy')]
    train.out.loocv <- knn_data[-lo,'eu_stance']
    test.out.loocv <- knn_data[lo,'eu_stance']
    predict_test_loocv <- knn(train = train.predictors.loocv,test = test.predictors.loocv,
                            cl = train.out.loocv, k = k)
    results2$match[lo] <- predict_test_loocv == test.out.loocv
  }
  results1$accuracy.loocv[k] <- sum(results2$match)/nrow(results2)
}

results1 %>%
  ggplot(aes(k, accuracy.loocv)) +
  geom_line() + ylab('LOOCV test-set accuracy')

# What would be our best k based on LOOCV?
max(results1$accuracy.loocv)

## Exercises: ----
# Does the prediction accuracy improve if you use other predictors (lrgen, galtan, environment)?
# Can you use more than two simultaneously?
# What about using 10-fold CV instead of LOOCV?
