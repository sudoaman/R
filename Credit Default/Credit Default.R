#Install required libraries
library(C50)
library(gmodels)

#ReadDataSet
credit <- read.csv("credit.csv")

str(credit)

table(credit$default)

#Create Sample Data using sample function
set.seed(123)
train_sample <- sample(1000,900)
str(train_sample)

#Create Sample data and test data
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

#Verify Data propotions
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))



credit_train$default<-as.factor(credit_train$default)
#Train model on training data set
credit_model <- C5.0(credit_train[-17] , credit_train$default)
str(credit_model)
summary(credit_model)

#Make predictions on test data set

credit_predict <- predict(credit_model , credit_test)




CrossTable(credit_test$default, credit_predict,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

credit_boost <- C5.0(credit_train[-17] , credit_train$default , trail = 20)
summary(credit_boost)
credit_boost
credit_boost_pred10 <- predict(credit_boost, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))

#Add Penalties to improve model

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")

matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)

error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default,
costs = error_cost)

credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
dnn = c('actual default', 'predicted default'))


