# Regression Analysis using 2017 Home Data from Kaggle


# reset global environment
rm(list = ls(all.names = TRUE))

# libraries
library(tidyverse)
library(knitr)
library(PerformanceAnalytics)
library(reshape2)
library(caret)
library(glmnet)
library(randomForest)
library(kableExtra)
library(gridExtra)


#----functions---------------------------------

#standardized in accordance with statistical learning methods
standardize <- function(vector){
  
  numerator <- vector
  denominator <- sqrt(mean((vector-mean(vector))**2))
  
  return(numerator/denominator)
  
}


#----cleaning-------------------------------------

# load data
homes_load <- read_csv("~/Documents/R/RProjects-Public/Regression-Analysis-Data/homes_data.csv")

# select 40 key columns (qualitative) from the data
homes_full <- cbind(select_if(homes_load, .predicate =  is.double), 
                    select(homes_load, Utilities, CentralAir, Street)) %>% select(-Id)

# recode the last 3 variable to get 0,1
homes_full$Utilities <- as.numeric(recode(homes_full$Utilities, "AllPub" = "1", "NoSeWa" = "0"))
homes_full$CentralAir <- as.numeric(recode(homes_full$CentralAir, "Y" = "1", "N" = "0"))
homes_full$Street <- as.numeric(recode(homes_full$Street, "Pave" = "1", "Grvl" = "0"))

#replace NA values in LotFrontage
for (i in 1:length(homes_full$LotArea)){
  
  if (is.na(homes_full$LotFrontage[i])){
    homes_full$LotFrontage[i] <- mean(homes_full$LotFrontage, na.rm = TRUE)
  }
  
  if (is.na(homes_full$GarageYrBlt[i])){
    homes_full$GarageYrBlt[i] <- mean(homes_full$GarageYrBlt, na.rm = TRUE)
  }
  
  if (is.na(homes_full$MasVnrArea[i])){
    homes_full$MasVnrArea[i] <- mean(homes_full$MasVnrArea, na.rm = TRUE)
  }
  
}

# SalePrice is skewed right: using log(SalePrice) for model
par(mfrow = c(1,2))
hist(homes_full$SalePrice, main = "Original", xlab = "Sale Price")
hist(log(homes_full$SalePrice), main = "Transform", xlab = "log(Sale Price)")

SalePrice <- homes_full$SalePrice
#SalePrice <- log(homes_full$SalePrice) #remove comment and delete above to run with log transform

#standardize the independent columns of the data and combine the response variable back
homes_full <- select(homes_full,-SalePrice) %>% mutate_all(.funs = standardize)
homes_full <- cbind(SalePrice,homes_full)

#create full dataset with errors for bootstrap error bar construction
 homes_full_bs <- homes_full


#----data split------------------------------------------------

# seed values and empty vector creation
seed_values <- seq(200,800, length.out = 100)

homes_train <- list(c("1"))
homes_test <- list(c("1"))

X_train <- list(c("1"))
Y_train <- list(c("1"))
X_test <- list(c("1"))
Y_test <- list(c("1"))

# set seed within for loop for reproducibility: 100 repetitions for eror boxplot evaluation
for (i in 1:100){
  set.seed(seed_values[i])

  # split data using caret package, step 1: create train indices
  train_indices <- as.vector(createDataPartition(homes_full$SalePrice, p = .8, list = FALSE, times = 1))
  
  # split accoring to train indices
  homes_train[[i]] <- homes_full[train_indices,]
  homes_test[[i]] <- homes_full[-train_indices,]
  
  # step 3: split further into matrix sets.
  X_train[[i]] <- select(homes_train[[i]],-SalePrice)
  Y_train[[i]] <- homes_train[[i]]$SalePrice
  X_test[[i]] <- select(homes_test[[i]], -SalePrice)
  Y_test[[i]] <- homes_test[[i]]$SalePrice

} # end of loop

# sample sizes
n_train <- length(train_indices)
n_test <- length(homes_full$SalePrice) - n_train


#----collinearity check amongst features-----------------------

# set up matrix view of the features
correlation_matrix <- round(cor(select(homes_full, -SalePrice)),2)

# graph the correlation values using shape package
ggplot(data = melt(correlation_matrix), mapping = aes(x = Var1, y = Var2, fill = value))+
  geom_tile(color = "white")+xlab("Features")+ylab("Features")+
  scale_fill_gradient(low = "darkgreen", high = "lightgreen")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = .5))+
  labs(title = str_to_title("visual representation of correlation matrix"))
  

#----linear regression model------------------------------------
# form: BetaHat(LR) = argmin(Beta) [(1/n)* ||Y-XBeta|| 2 ]  | using homes_train[[1]]

# model fit and test
model_lr <- lm(SalePrice ~ ., homes_train[[1]])
Beta_star <- as.matrix(model_lr$coefficients) # set beta star values for matrix multiplication

# model coefficient check, should NA values be present than variables need to be dropped 
# as they are highly collinear with other variables
removal <- (which(is.na(Beta_star)))

#remove variables at indices 13 and 17
homes_train_lr <- select(homes_train[[1]], -removal)

# new model
model_lr_new <- lm(SalePrice ~ ., homes_train_lr)
Beta_star <- as.matrix(model_lr_new$coefficients) # set beta star values for matrix multiplication

# set values using model parameters
Y_train_hat_lr <- as.matrix(cbind(rep(1,length(train_indices)),X_train[[1]][,-removal])) %*% Beta_star
Y_test_hat_lr <- as.matrix(cbind(rep(1,length(homes_full$SalePrice)-length(train_indices)),X_test[[1]][,-removal])) %*% Beta_star
#Y_test_hat_lr2 <- predict(model_lr_new, newdata = as.matrix(cbind(rep(1,length(train_indices)),X_test[[1]][,-removal])))

# errors for linear model
rse_train_lr <- sqrt(mean((Y_train_hat_lr - Y_train[[1]])**2))
rse_test_lr <- sqrt(mean((Y_test_hat_lr - Y_test[[1]])**2))

# test correlation
tcor <- round(cor(as.vector(Y_test_hat_lr), as.vector(Y_test[[1]])),3)

# quick view of errrors
par(mfrow = c(1,1))
plot(Y_test_hat_lr, Y_test[[1]], main = paste0("Test Correlation:"," ",tcor),
     xlab = "Predicted Values", ylab = "Actual Values")


#----elastic net models------------------------------------
# form: BetaHat(EN) = argmin(Beta) [(1/n)* ||Y-XBeta|| 2 + lambda* r(Beta)]
#     : r(Beta) = [ (1-alpha)*(1/2) ||Beta|| 2 + alpha*||Beta|| 1 ]
#     : Ridge (alpha = 0), Lasso (alpha = 1), Elastic_Net (alpha = arbitrary)


#----ridge----

# create empty R2 vector
R2_train_ridge <- rep(1,100)
R2_test_ridge <- rep(1,100)
rse_train_ridge <- rep(1,100)
rse_test_ridge <- rep(1,100)

# create empty matrix to store bootstrap methods
boots_ridge <- matrix(0, 39, 100)

# start time for loop to evaluate method efficiency 
ridge_start <- Sys.time()

#for loop to run 100 times
for (i in 1:100){

  # cast data as matrices 
  X_train <- as.matrix(select(homes_train[[i]],-SalePrice))
  Y_train <- as.matrix(homes_train[[i]]$SalePrice)
  X_test <- as.matrix(select(homes_test[[i]], -SalePrice))
  Y_test <- as.matrix(homes_test[[i]]$SalePrice)


  # 10 fold cross validation model to generate lambda that minimizes cross-validation error
  cv_model_ridge <- cv.glmnet(x = X_train, y = Y_train, alpha = 0)

  # create ridge model with cv lambda
  model_ridge <- glmnet(x = X_train, y = Y_train, alpha = 0, lambda = cv_model_ridge$lambda.min)

  # generate y_hat matrices for the train and test sets
  Y_train_hat_ridge <- predict.glmnet(model_ridge, newx = X_train, type = "response")
  Y_test_hat_ridge <- predict.glmnet(model_ridge, newx = X_test, type = "response")

  # errors for linear model
  rse_train_ridge[i] <- sqrt(mean((Y_train_hat_ridge - Y_train)**2))
  rse_test_ridge[i] <- sqrt(mean((Y_test_hat_ridge - Y_test)**2))

  # R**2 calculation
  R2_train_ridge[i] <- model_ridge$dev.ratio #pull directly from the model
  R2_test_ridge[i] <- 1 - (mean((Y_test_hat_ridge - Y_test)**2)/var(Y_test))
  
  # cbind bootstrap samples
  boots_ridge[,i] <- as.vector(model_ridge$beta)

} # end of loop

# end time for loop
ridge_stop <- ridge_start - Sys.time()

# calculate bootstrap standard errors
boots_ridge_sd <- apply(boots_ridge, 1, "sd")

#----lasso----

# create empty R2 vector
R2_train_lasso <- rep(1,100)
R2_test_lasso <- rep(1,100)
rse_train_lasso <- rep(1,100)
rse_test_lasso <- rep(1,100)

# create empty matrix to store bootstrap methods
boots_lasso <- matrix(0, 39, 100)

# start time for loop to evaluate method efficiency 
lasso_start <- Sys.time()


#for loop to run 100 times
for (i in 1:100){
  
  # cast data as matrices 
  X_train <- as.matrix(select(homes_train[[i]],-SalePrice))
  Y_train <- as.matrix(homes_train[[i]]$SalePrice)
  X_test <- as.matrix(select(homes_test[[i]], -SalePrice))
  Y_test <- as.matrix(homes_test[[i]]$SalePrice)
  
  
  # 10 fold cross validation model to generate lambda that minimizes cross-validation error
  cv_model_lasso <- cv.glmnet(x = X_train, y = Y_train, alpha = 1)
  
  # create ridge model with cv lambda
  model_lasso <- glmnet(x = X_train, y = Y_train, alpha = 1, lambda = cv_model_lasso$lambda.min)
  
  # generate y_hat matrices for the train and test sets
  Y_train_hat_lasso <- predict.glmnet(model_lasso, newx = X_train, type = "response")
  Y_test_hat_lasso <- predict.glmnet(model_lasso, newx = X_test, type = "response")
  
  # errors for linear model
  rse_train_lasso[i] <- sqrt(mean((Y_train_hat_lasso - Y_train)**2))
  rse_test_lasso[i] <- sqrt(mean((Y_test_hat_lasso - Y_test)**2))
  
  # R**2 calculation
  R2_train_lasso[i] <- model_lasso$dev.ratio #pull directly from the model
  R2_test_lasso[i] <- 1 - (mean((Y_test_hat_lasso - Y_test)**2)/var(Y_test))
  
  # cbind bootstrap samples
  boots_lasso[,i] <- as.vector(model_lasso$beta)
  
} # end of loop

# end time for loop
lasso_stop <- lasso_start - Sys.time()

# calculate bootstrap standard errors
boots_lasso_sd <- apply(boots_lasso, 1, "sd")


#----elastic net----

# data previoulsy converted to matrix format

# create empty R2 vector
R2_train_en <- rep(1,100)
R2_test_en <- rep(1,100)
rse_train_en <- rep(1,100)
rse_test_en <- rep(1,100)

# create empty matrix to store bootstrap methods
boots_en <- matrix(0, 39, 100)

# start time for loop to evaluate method efficiency 
en_start <- Sys.time()

#for loop to run 100 times
for (i in 1:100){
  
  # cast data as matrices 
  X_train <- as.matrix(select(homes_train[[i]],-SalePrice))
  Y_train <- as.matrix(homes_train[[i]]$SalePrice)
  X_test <- as.matrix(select(homes_test[[i]], -SalePrice))
  Y_test <- as.matrix(homes_test[[i]]$SalePrice)
  
  
  # 10 fold cross validation model to generate lambda that minimizes cross-validation error
  cv_model_en <- cv.glmnet(x = X_train, y = Y_train, alpha = 0.5)
  
  # create ridge model with cv lambda
  model_en <- glmnet(x = X_train, y = Y_train, alpha = 0.5, lambda = cv_model_en$lambda.min)
  
  # generate y_hat matrices for the train and test sets
  Y_train_hat_en <- predict.glmnet(model_en, newx = X_train, type = "response")
  Y_test_hat_en <- predict.glmnet(model_en, newx = X_test, type = "response")
  
  # errors for linear model
  rse_train_en[i] <- sqrt(mean((Y_train_hat_en - Y_train)**2))
  rse_test_en[i] <- sqrt(mean((Y_test_hat_en - Y_test)**2))
  
  # R**2 calculation
  R2_train_en[i] <- model_en$dev.ratio #pull directly from the model
  R2_test_en[i] <- 1 - (mean((Y_test_hat_en - Y_test)**2)/var(Y_test))
  
  # cbind bootstrap samples
  boots_en[,i] <- as.vector(model_en$beta)
  
} # end of loop

# end time for loop
en_stop <- en_start - Sys.time()

# calculate bootstrap standard errors
boots_en_sd <- apply(boots_en, 1, "sd")


#----random forest model--------------------------------------------

# create empty R2 vector
R2_train_rf <- rep(1,100)
R2_test_rf <- rep(1,100)
rse_train_rf <- rep(1,100)
rse_test_rf <- rep(1,100)

# create 
boots_rf <- matrix(0, 39, 100)

# start time for loop to evaluate method efficiency 
rf_start <- Sys.time()

#for loop to run 100 times
for (i in 1:100){
  
  # cast data as matrices 
  X_train <- as.matrix(select(homes_train[[i]],-SalePrice))
  Y_train <- as.matrix(homes_train[[i]]$SalePrice)
  X_test <- as.matrix(select(homes_test[[i]], -SalePrice))
  Y_test <- as.matrix(homes_test[[i]]$SalePrice)
  
  # create random forest model with sqrt(p) parameter, here p = 40
  model_rf <- randomForest(x = X_train, y = Y_train, importance = TRUE)
  
  # generate y_hat matrices for the train and test sets
  Y_train_hat_rf <- predict(model_rf, newdata = X_train)
  Y_test_hat_rf <- predict(model_rf, newdata = X_test)
  
  # errors for linear model
  rse_train_rf[i] <- sqrt(mean((Y_train_hat_rf - Y_train)**2))
  rse_test_rf[i] <- sqrt(mean((Y_test_hat_rf - Y_test)**2))
  
  # R**2 calculation
  R2_train_rf[i] <- 1 - (mean((Y_train_hat_rf - Y_train)**2)/var(Y_train))
  R2_test_rf[i] <- 1 - (mean((Y_test_hat_rf - Y_test)**2)/var(Y_test))
  
  # cbind bootstrap samples
  boots_en[,i] <- as.vector(model_rf$importance[,1])
  
} # end of loop

# end time for loop
rf_stop <- rf_start - Sys.time()

# calculate bootstrap standard errors
boots_rf_sd <- apply(boots_rf, 1, "sd")

#----Plots---------------------------------------------------------

# view cross validation plots for the last cv models generated to discuss shape, etc.
par(mfrow = c(2,2))
plot(cv_model_ridge)
plot(cv_model_lasso)
plot(cv_model_en)
plot(model_rf, main = "Random Forest Curve")
par(mfrow = c(1,1))

#aggregate and tidy R2 data
R_sq_train <- as.data.frame(cbind(R2_train_ridge,R2_train_lasso, R2_train_en, R2_train_rf))
names(R_sq_train) <- c("Ridge", "Lasso", "Elastic Net", "Random Forest")
R_sq_test <- as.data.frame(cbind(R2_test_ridge,R2_test_lasso, R2_test_en, R2_test_rf))
names(R_sq_test) <- c("Ridge", "Lasso", "Elastic Net", "Random Forest")

R_sq_train <- gather(R_sq_train, key = "Technique", value = "R2")
R_sq_test <- gather(R_sq_test, key = "Technique", value = "R2")

# view boxplots of R2 values
ggplot(data = R_sq_train, mapping = aes(x = Technique, y = R2, group = Technique)) +
  geom_boxplot(mapping = aes(group = Technique), fill = "skyblue")+
  ylab("R^2 Values")+
  scale_y_log10()+
  theme_linedraw()+
  labs(title = str_to_title("Training Data Comparison"))+
  theme(plot.title = element_text(hjust = .5))

ggplot(data = R_sq_test, mapping = aes(x = Technique, y = R2, group = Technique)) +
  geom_boxplot(mapping = aes(group = Technique), fill = "skyblue")+
  ylab("R^2 Values")+
  scale_y_log10()+
  theme_linedraw()+
  labs(title = str_to_title("Test Data Comparison"))+
  theme(plot.title = element_text(hjust = .5))


#----barplot with bootstrap errors-------------------------

# ridge
model_ridge_full <- glmnet(x =as.matrix(homes_full_bs[,2:40]), y = as.matrix(homes_full_bs[,1]), alpha = 0, lambda = cv_model_ridge$lambda.min)
bsp_df_ridge <- data.frame(rep("Ridge",39), names(homes_full_bs)[2:40], as.vector(model_ridge_full$beta), 2*boots_ridge_sd)
names(bsp_df_ridge) <- c("Method","Feature", "Significance", "Error (95% CI)")

# lasso
model_lasso_full <- glmnet(x =as.matrix(homes_full_bs[,2:40]), y = as.matrix(homes_full_bs[,1]), alpha = 1, lambda = cv_model_lasso$lambda.min)
bsp_df_lasso <- data.frame(rep("Lasso",39), names(homes_full_bs)[2:40], as.vector(model_lasso_full$beta), 2*boots_lasso_sd)
names(bsp_df_lasso) <- c("Method", "Feature", "Significance", "Error (95% CI)")

#elastic net
model_en_full <- glmnet(x =as.matrix(homes_full_bs[,2:40]), y = as.matrix(homes_full_bs[,1]), alpha = 0, lambda = cv_model_en$lambda.min)
bsp_df_en <- data.frame(rep("Elastic Net",39), names(homes_full_bs)[2:40], as.vector(model_en_full$beta), 2*boots_en_sd)
names(bsp_df_en) <- c("Method","Feature", "Significance", "Error (95% CI)")

# random forest
model_rf_full <- randomForest(x =as.matrix(homes_full_bs[,2:40]), y = as.matrix(homes_full_bs[,1]), importance = TRUE)
bsp_df_rf <- data.frame(rep("Random Forest",39), names(homes_full_bs)[2:40], as.vector(model_rf_full$importance[,1]), 2*boots_rf_sd)
names(bsp_df_rf) <- c("Method","Feature", "Significance", "Error (95% CI)")

# combine the data frames to face_wrap() for better visual comparison 
boot_data <- rbind(bsp_df_ridge, bsp_df_lasso, bsp_df_en, bsp_df_rf)

# plot the data
ggplot(data = bsp_df_ridge, mapping = aes(x = Feature, y = Significance))+
  geom_col(fill = "darkgreen")+ 
  geom_errorbar(aes(ymin = Significance - `Error (95% CI)`, ymax = Significance + `Error (95% CI)`), 
                width = .5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = .5))+
  labs(title = str_to_title("bootstrapped error of ridge parameter significance"))

ggplot(data = bsp_df_lasso, mapping = aes(x = Feature, y = Significance))+
  geom_col(fill = "darkblue")+ 
  geom_errorbar(aes(ymin = Significance - `Error (95% CI)`, ymax = Significance + `Error (95% CI)`), 
                width = .5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = .5))+
  labs(title = str_to_title("bootstrapped error of lasso parameter significance"))

ggplot(data = bsp_df_en, mapping = aes(x = Feature, y = Significance))+
  geom_col(fill = "orange")+ 
  geom_errorbar(aes(ymin = Significance - `Error (95% CI)`, ymax = Significance + `Error (95% CI)`),
                width = .5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = .5))+
  labs(title = str_to_title("bootstrapped error of elastic net parameter significance"))

ggplot(data = bsp_df_rf, mapping = aes(x = Feature, y = Significance))+
  geom_col(fill = "darkred")+
  geom_errorbar(aes(ymin = Significance - `Error (95% CI)`, ymax = Significance + `Error (95% CI)`), 
                width = .5)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = .5))+
  labs(title = str_to_title("bootstrapped error of random forest parameter significance"))


#----residuals EDA for the 100th iteration of each model----

# combine residuals to explore with faceted ggplot
resid_ridge <- as.vector(Y_test_hat_ridge - Y_test)
resid_lasso <- as.vector(Y_test_hat_lasso - Y_test)
resid_en <- as.vector(Y_test_hat_en - Y_test)
resid_rf <- as.vector(Y_test_hat_rf - Y_test)

resid_data <- data.frame(resid_ridge, resid_lasso, resid_en, resid_rf)
names(resid_data) <- c("Ridge","Lasso", "Elastic Net", "Random Forest")

resid_data <- gather(resid_data, key = "Technique", value = "Test Residuals")

# plot
ggplot(data = resid_data, mapping = aes(x = `Test Residuals`))+
  geom_histogram()+facet_wrap(~Technique)+theme_classic()+
  ylab("Frequency")

#----list of process time by method-----------------------
paste0("Ridge Regression:"," ",abs(round(ridge_stop,2))," ","Seconds")
paste0("Lasso Regression:"," ", abs(round(lasso_stop,2))," ","Seconds")
paste0("Elastic Net Regression:"," ", abs(round(en_stop,2))," ","Seconds")
paste0("Random Forest Regression:"," ", abs(round(rf_stop,2))," ","Minutes")





