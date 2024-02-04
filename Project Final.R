library(ggplot2)
library(car)
library(tidyr)
library(tidyverse)
library(dplyr)
library(rlang)
library(caret)
library(stargazer)
rm(list = ls())
setwd("C:/Users/91979/Desktop/MSCI 609 Quantitative Data Analysis/Project")
movie<- read.csv("movie_3000_datapoints.csv")

#movie <- read.csv("movie_gender_genre.csv")

#Visualizing Revenue with each independent variable
#Revenue Vs Budget
ggplot(movie,aes(budget, revenue))+
  geom_point()
#Revenue Vs Vote Avg
ggplot(movie,aes(vote_average, revenue))+
  geom_point()
#Revenue Vs Runtime
ggplot(movie,aes(runtime, revenue))+
  geom_point()
#Revenue Vs Main Lead as Male group
ggplot(movie,aes(is_male, revenue))+
  geom_point()
#Revenue Vs Main Lead as Female group
ggplot(movie,aes(is_female, revenue))+
  geom_point()
#No. of Countries movies were released in
ggplot(data= movie, aes(country_release_count,revenue))+
  geom_point()

#Distribution of movie revenue
hist(movie$revenue)
#Taking the log of revenue
movie$rev_norm <- log(movie$revenue)
hist((movie$rev_norm))


#Creating the dataframe
movie<- data.frame(movie)
#Summary of movie Data
summary(movie)

#Ordinary Linear Square Regression OLS
l_reg<- lm( revenue~ budget,  data= movie)
#Summary of OLS estimate results
summary(l_reg)  #Multiple R-squared:  0.2748,	Adjusted R-squared:  0.2746 
#Plotting OLS
plot(movie$budget,movie$revenue)
abline(l_reg, col="red")
hist(l_reg$residuals)

#Multiple Linear Regression with more predictor variables 
mul_reg<- lm( revenue~ budget + runtime +vote_average, data = movie)
summary(mul_reg)  #Multiple R-squared:  0.3015,	Adjusted R-squared:  0.3008
avPlots(mul_reg)
hist(mul_reg$residuals)




normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
movie$rev_norm <- normalize(movie$revenue)
movie$bud_norm <- normalize(movie$budget)
movie$vote_avg_norm <- normalize(movie$vote_average)
movie$country_norm <- normalize(movie$country_release_count)
movie$popularity_norm <- normalize(movie$popularity)
movie$runtime_norm <- normalize(movie$runtime)
movie$is_female <- as.factor(movie$is_female)
movie$is_male <- as.factor(movie$is_male)
movie$genre_f <- as.factor(movie$primary_genre)
movie$rev_norm <- log(movie$revenue)
movie$bud_norm <- log(movie$budget)
movie$vote_count_norm <- normalize(movie$vote_count)

min_value<- min(movie$vote_count,na.rm = TRUE) 
max_value<- max(movie$vote_count,na.rm = TRUE)

# #Normalizing numeric predictor variables
# #Revenue
min_value <- min(movie$revenue, na.rm = TRUE)
 max_value <- max(movie$revenue, na.rm = TRUE)
# 
# rev_norm <- as.data.frame((movie$revenue - min_value) / (max_value - min_value))
# movie<- cbind(movie, rev_norm)
# colnames(movie)[colnames(movie) == "(movie$revenue - min_value)/(max_value - min_value)"] <- "rev_norm"
# 
# 
# 
# #Budget
 min_value <- min(movie$budget, na.rm = TRUE)
 max_value <- max(movie$budget, na.rm = TRUE)
# 
# bud_norm <- as.data.frame((movie$budget - min_value) / (max_value - min_value))
# movie<- cbind(movie, bud_norm)
# colnames(movie)[colnames(movie) == "(movie$budget - min_value)/(max_value - min_value)"] <- "bud_norm"
# 
# #Vote Average
# vote_avg_norm <- as.data.frame(movie$vote_average/10)
# movie<- cbind(movie, vote_avg_norm)
# colnames(movie)[colnames(movie) == "movie$vote_average/10"] <- "vote_avg_norm"
# 
# #Runtime
min_value <- min(movie$runtime, na.rm = TRUE)
max_value <- max(movie$runtime, na.rm = TRUE)
# runtime_norm <- as.data.frame((movie$runtime - min_value) / (max_value - min_value))
# movie<- cbind(movie, runtime_norm)
# colnames(movie)[colnames(movie) == "(movie$runtime - min_value)/(max_value - min_value)"] <- "runtime_norm"
# 
# #Popularity
min_value <- min(movie$popularity, na.rm = TRUE)
 max_value <- max(movie$popularity, na.rm = TRUE)
# popularity_norm <- as.data.frame((movie$popularity - min_value) / (max_value - min_value))
# movie<- cbind(movie, popularity_norm)
# colnames(movie)[colnames(movie) == "(movie$popularity - min_value)/(max_value - min_value)"] <- "popularity_norm"
# #Country Release count
 min_value <- min(movie$country_release_count, na.rm = TRUE)
 max_value <- max(movie$country_release_count, na.rm = TRUE)
# country_norm <- as.data.frame((movie$country_release_count - min_value) / (max_value - min_value))
# movie<- cbind(movie, country_norm)
# colnames(movie)[colnames(movie) == "(movie$country_release_count - min_value)/(max_value - min_value)"] <- "country_norm"
# 
# #factorize gender
# movie$is_female <- as.factor(movie$is_female)
# #Viewing updated dataframe
# 
# 
# #Introducing Categorical variables as genre
# movie$genre_f <- as.factor(movie$primary_genre)

View(movie)

#Multiple Linear Rgression 
mul_reg_init<- lm( revenue ~ budget + runtime + popularity+ country_release_count+vote_average +is_male +genre_f, data= movie)
summary(mul_reg_init)  #Multiple R-squared:  0.6309,	Adjusted R-squared:  0.6282   
hist(mul_reg_init$residuals, main="Histogram of Residuals of Initial Model")

#Multiple Linear Rgression on Log and  Normalized predictor variables, Genre and Gender as Dummy variables
mul_reg_transf<- lm( rev_norm ~ bud_norm + runtime_norm +popularity_norm+ country_norm+vote_avg_norm  +genre_f+is_male , data=movie)
summary(mul_reg_transf) #Multiple R-squared:  0.5321,	Adjusted R-squared:  0.5286 
avPlots(mul_reg_transf)
hist(mul_reg_transf$residuals)


#Multiple Linear Rgression on Normalized predictor variables, Genre and Gender as Dummy variables
mul_reg_norm<- lm( rev_norm ~ bud_norm + runtime_norm +popularity_norm+ country_norm+vote_count_norm +vote_avg_norm +genre_f+is_male , data=movie)
summary(mul_reg_norm) #Multiple R-squared:  0.5321,	Adjusted R-squared:  0.5286 
par(mfrow= c(2,2))
plot(mul_reg_norm)
avPlots(mul_reg_norm)
hist(mul_reg_norm$residuals)
vif(mul_reg_norm)


vif(mul_reg_inte)
#Interaction terms OLS Normalized
mul_reg_inte<- lm( rev_norm ~ bud_norm + runtime_norm +popularity_norm+ country_norm+vote_avg_norm*vote_count_norm  +genre_f+is_male , data=movie)
summary(mul_reg_inte) #Multiple R-squared:  0.5601,	Adjusted R-squared:  0.5565  
avPlots(mul_reg_inte)
hist(mul_reg_inte$residuals, main="Histogram of Residuals for Final Model with Interaction Term")
par(mfrow= c( 2 , 2))
plot( mul_reg_inte)
hist(mul_reg_inte$residuals)


#Plotting Diagnostic plots
par(mfrow = c(2, 2))
plot(mul_reg_init, which = 1, main="Initial Model")
plot(mul_reg_transf, which = 1, main="Log Transformed Model")
plot(mul_reg_inte, which = 1, main="Interaction term added and log Transformed Model")
plot(movie_full, which = 1, main="Full Model")

#Plot to check the Residual Vs Fitted Values
fitted_vals <- fitted(mul_reg_transf)
res_vals <- resid(mul_reg_transf)
plot(mul_reg_transf, which=1)
plot(fitted_vals,res_vals, main ="Residuals VS Fitted", ylab="residuals", xlab = "fitted values")
hist(res_vals, xlab = "Residuals", main = "Histogram of Residuals for Transformed Variable Model")


# Residuals vs Fitted
plot(mul_reg_transf, which = 1)
# Autocorrelation Check No Autocorrelation of Errors:
acf(resid(mul_reg_transf))
# Q-Q Plot
plot(mul_reg_init, which = 2)
# Scale-Location Plot Homoscedasticity:
plot(mul_reg_transf, which = 3)
library(car)
# Check VIF Multicollinearity:
vif(mul_reg_init)


vif(mul_reg_transf)
par(mfrow = c(2, 2))
plot(mul_reg_init, main = "Diagnostic Plot  Initial Model")


# library(sandwich)
# library(lmtest)
# model_robust <- coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))
# model_robust


#Checking Independence of variables
durbinWatsonTest(model_glm)

# # Assuming 'movie' is your data frame and it has a column 'year_release'
# # Convert year of release to a factor
# movie$release_year <- as.factor(movie$release_year)
# 
# # Fit the GLM model including the year of release as a factor
# model_with_year <- glm(revenue ~ bud_norm + runtime_norm + vote_avg_norm + Drama + Comedy + 
#                          Action + is_male + release_year, data = movie)
# # Summary of the model
# summary(model_with_year)




#Cross Validation
# Prepare a formula for the model
formula_norm<- rev_norm ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm + country_norm+genre_f+ is_male  
#formula_log <- rev_norm ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm + country_norm+genre_f+ is_male  
formula_inte <- rev_norm ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm*vote_count_norm + country_norm+genre_f+ is_male


# Define control parameters for cross-validation
# Here, method = "cv" specifies cross-validation, and number = 10 sets up 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

#CV for log term model 
cv_log <- train(formula_log, data = movie, method = "lm", trControl = train_control)
summary(cv_log)
# Print the results
print(model_cv_log$results) #Multiple R-squared:  0.5321,	Adjusted R-squared:  0.5286 

#CV for norm term model 
cv_norm <- train(formula_norm, data = movie, method = "lm", trControl = train_control)
summary(cv_norm) 
# Print the results
print(cv_log$results) #Multiple R-squared:  0.6309,	Adjusted R-squared:  0.6282

#CV for interaction term model 
cv_inte <- train(formula_inte, data = movie, method = "lm", trControl = train_control)
summary(cv_inte)
# Print the results
print(cv_inte$results) #Multiple R-squared:  0.5601,	Adjusted R-squared:  0.5565 


#Weighted Regression
wt <- 1 / lm(abs(mul_reg_norm$residuals) ~ mul_reg_norm$fitted.values)$fitted.values^2
w_lreg <- lm(rev_norm ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm  +genre_f+ is_female+country_norm , data = movie, weights = wt)
summary(w_lreg) #Multiple R-squared:  0.8134,	Adjusted R-squared:  0.8119 

cv_wreg <- train(rev_norm ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm  +genre_f+ is_female+country_norm , data = movie, method = "lm", trControl = train_control, weights = wt)
summary(cv_wreg)
hist(w_lreg$residuals)

par( mfrow=c(2,2))
plot(w_lreg)
hist(w_lreg$residuals)

#Mean of Log(Revenue)
mean_revenue <- mean(movie$rev_norm, na.rm = TRUE)

# Calculate the variance for revenue
variance_revenue <- var(movie$rev_norm, na.rm = TRUE)

# Print the results

print(paste("Mean Revenue:", mean_revenue))
print(paste("Variance of Revenue:", format(variance_revenue,scientific=TRUE )))

# Predicted values from the model
fitted_values <- fitted(w_lreg)

# Observed values
observed_values <- movie$rev_norm

# Residuals from the model
residuals <- residuals(w_lreg)

# Plot of observed vs. fitted values
plot(fitted_values, observed_values, xlab = "Fitted Values", ylab = "Observed Values")
abline(a = 0, b = 1, col = "red") # A 45-degree line for reference
# If the relationship is perfectly linear, points should scatter around the red line

# Alternatively, a plot of fitted values vs. residuals to check for any systematic pattern
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red") # A horizontal line at zero for reference

# Residuals should scatter randomly around the horizontal line with no clear pattern

# # CR plot for each predictor in the model
# crPlots(mul_reg_gend)

# revenue_log <- log(movie$revenue + 1)
# hist(revenue_log)
# hist(movie$revenue)
# set.seed(123)  # Setting seed for reproducibility
# data <- rgamma(1000,1,0.5)
# hist(data)




#To choose best model
wt <- 1 / lm(abs(mul_reg_inte$residuals) ~ mul_reg_inte$fitted.values)$fitted.values^2
movie_full <- lm(rev_norm ~ bud_norm + runtime_norm + popularity_norm+ vote_avg_norm+vote_count_norm
               +genre_f+ 
               is_male+country_norm , data = movie)
movie_empty <- lm(rev_norm~1, data=movie)

#using AIC for model selection and no trace output
movies_step<-step(movie_full,scope=list(upper=movie_empty), direction= c("backward"), k=2,trace=0)
movie_step_forward <-step (movie_full,scope=formula(movie_full), direction= c("forward") , k=2,trace=0)

summary(movies_step)
movies_step$anova


summary(movie_step_forward)
movie_step_forward$anova

par(mfrow = c(2, 2))
plot(movies_step)
avPlots(mul_reg_transf)

#Visualizing the key reulst
# Assuming movie_full is your model with interactions
# Create a new model without interactions
movie_no_interaction <- update(movie_full, . ~ . -  vote_avg_norm*vote_count_norm)

# Now use crPlots on the model without interactions
crPlots(movies_step)

#LRT Test
anova( mul_reg_inte,mul_reg_transf, test="LRT")

#capturing outputs
stargazer( movies_step ,type="text")

#Pearson residuals
plot(residuals(movie_full, type="pearson"), ylab="Pearson Residuals")

#CV for the final model
formula_final <- rev_norm ~ bud_norm + runtime_norm + popularity_norm + 
  vote_count_norm + genre_f + is_male + country_norm

cv_final <- train(formula_final, data = movie, method="lm", trControl=train_control)
summary(cv_final)
print(cv_final$results)

hist(movies_step$residuals)

coeff <- 0.013

valu <- ((coeff* 2700000000) /(max_value - min_value))  +5

valu <- (coeff* 2700000000) + 5
valu
