library(ggplot2)
library(car)
library(tidyr)
library(tidyverse)
library(dplyr)
library(rlang)
rm(list = ls())
setwd("C:/Users/91979/Desktop/MSCI 609 Quantitative Data Analysis/Project")
movie<- read.csv("movie_2000s.csv")




#Lasso and Ridge Regression
# Prepare the matrix of predictor variables and the response variable
library(glmnet)

mm_preditor <- model.matrix(revenue ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm + country_norm+genre_f+ is_female, data = movie)
mm_outcome <- movie$revenue


# Set alpha for Lasso and Ridge regression
# alpha = 1 for Lasso; alpha = 0 for Ridge
alpha_lasso <- 1
alpha_ridge <- 0

# Fit the Lasso model
lasso_model <- glmnet(mm_preditor, mm_outcome, alpha = alpha_lasso)

# Fit the Ridge model
ridge_model <- glmnet(mm_preditor, mm_outcome, alpha = alpha_ridge)

#No pooling
model_no_pooling <- glmnet(x = mm_preditor, y = mm_outcome, alpha= 0, lambda = 0)
coef(model_no_pooling)
#Complete Pooling
model_complete_pooling <- glmnet(x = mm_preditor, y = mm_outcome, alpha = 0, lambda = 10^5)
coef(model_complete_pooling)


cv_model_ridge <- cv.glmnet(x = mm_preditor, y = mm_outcome, alpha = 0)
best_lambda_ridge <- cv_model_ridge$lambda.min
best_lambda_ridge
## [1] 0.109076

model_ridge <- glmnet(x = mm_preditor, y = mm_outcome, alpha = 0, lambda =best_lambda_ridge )
coef(model_ridge)

#Partial Pooling 2 with Ridge
p_fac <- rep(1, ncol(mm_preditor))
p_fac[2] <- 0
cv_model_ridge_p_fac <- cv.glmnet(x = mm_preditor, y = mm_outcome, alpha = 0,
                                  penalty.factor = p_fac)
best_lambda_ridge_p_fac <- cv_model_ridge_p_fac$lambda.min
best_lambda_ridge_p_fac
model_ridge_p_fac <- glmnet(x = mm_preditor, y = mm_outcome, alpha = 0, lambda =
                              best_lambda_ridge_p_fac )
coef(model_ridge_p_fac )

#Lasso
cv_model_lasso <- cv.glmnet(x = mm_preditor, y = mm_outcome, alpha = 1)
best_lambda_lasso <- cv_model_lasso$lambda.min
best_lambda_lasso
#[1] 0.02191592

model_lasso <- glmnet(x = mm_preditor, y = mm_outcome, alpha = 1, lambda= best_lambda_lasso)
coef(model_lasso )

#Lasso with higher Penalty
model_lasso_more_penalty <- glmnet(x = mm_preditor, y = mm_outcome, alpha
                                   = 1, lambda = 1)
coef(model_lasso_more_penalty )



y_predicted_ridge <- predict(model_ridge, s = best_lambda_ridge, newx = mm_preditor)
head(y_predicted_ridge)

y_predicted_ridge <- predict(model_ridge, s = best_lambda_ridge, newx = mm_preditor)
y_predicted_no_pooling <- predict(model_no_pooling, s = 0, newx = mm_preditor)
y_predicted_complete_pooling <- predict(model_complete_pooling, s = 0, newx = mm_preditor)
sum((y_predicted_ridge - mm_outcome)^2)
## [1] 9052.52
sum((y_predicted_no_pooling - mm_outcome)^2)
## [1] 9043.342

