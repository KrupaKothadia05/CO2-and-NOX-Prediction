# Importing libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(caTools)
library(glmnet)
library(plotly)



# Loading Dataset
dataframe <- dir("CO2 and NOX Dataset/", full.names = T) %>% map_df(read_csv)
summary(dataframe)

# Check Null Columns 
colSums(is.na(dataframe))

#Data Visualization
cormat <- round(cor(dataframe), 2)
head(cormat)
melted_cormat <- melt(cormat, na.rm = T)
ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', limit = c(-1, 1), midpoint = 0)
theme_classic()

# Train- Test Split
dataset <- dataframe%>%
  dplyr::select(-3,-7,-10,1,2,4,5,6,8,9,11)
head(dataset)
#dataset <- dataframe[, -c(3,7,11)]
set.seed(123)
split = sample.split(dataset$CO, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting model
regressor = lm(formula = NOX ~ AT + AFDP + GTEP + TIT + TEY + CDP, data = training_set)
summary(regressor)$r.squared
y_pred = predict(regressor, newdata = test_set)
y_pred


# Lasso Regression
train_x <- model.matrix(NOX ~ ., training_set)[, -c(8)]
train_y <- log(training_set$NOX)

test_x <- model.matrix(NOX ~ ., test_set)[, -c(8)]
test_y <- log(test_set$NOX)

set.seed(123)
lasso <- glmnet(x = train_x, y = train_y, alpha = 1)
plot(lasso, xvar = "lambda")

# Cross-val Lasso
set.seed(123)
lasso_cv <- cv.glmnet(x = train_x, y = train_y, alpha = 1)
plot(lasso_cv)

print('Your Vertical Lines w/ corresponding information')
min_mse_lasso_cv<-min(lasso_cv$cvm)       # minimum MSE

lambda_for_min_mse_lasso<-lasso_cv$lambda.min  # lambda for this min MSE

# 1 st.error of min MSE
minMSE_1stderr_lasso<-lasso_cv$cvm[lasso_cv$lambda == lasso_cv$lambda.1se]

lambda_for_minMSe_1stderr_lasso<-lasso_cv$lambda.1se  # lambda for this MSE


sprintf('Min. MSE for CV: %.4f',min_mse_lasso_cv)
sprintf('Lambda for Corresponding Min. MSE : %.4f',lambda_for_min_mse_lasso)
sprintf('Min MSE for 1st Error: %.4f',minMSE_1stderr_lasso)
sprintf('Corresponding Lambda for 1 Std Err : %.4f',lambda_for_minMSe_1stderr_lasso)

sprintf('Log(Lambda Min. MSE): %.3f',log(lambda_for_min_mse_lasso))
sprintf('Log(Lambda 1 Std Error MSE): %.3f',log(lambda_for_minMSe_1stderr_lasso))

# Training
set.seed(123)
IA_lasso <- glmnet(x = train_x,y = train_y,alpha = 1)


# Tuning:
fit.lasso.cv <- cv.glmnet(train_x, train_y, type.measure="mse", alpha=1)

# Predicting Values:
pred_fit_lasso_train <- predict(fit.lasso.cv, train_x, s = "lambda.min")

pred_fit_lasso_test <- predict(IA_lasso, test_x, s =fit.lasso.cv$lambda.min )

print('LASSO:')
sprintf('Test RMSE: %.4f',sqrt(mean((pred_fit_lasso_test-test_y)^2)))
sprintf('Train RMSE: %.4f',sqrt(mean((pred_fit_lasso_train-train_y)^2)))