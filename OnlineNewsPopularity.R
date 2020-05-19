install.packages("glmnet")
library(glmnet)
install.packages("randomForest")
library(randomForest)
install.packages("magicfor")
library(magicfor)
install.packages("gridExtra")
library(gridExtra)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
install.packages("plyr")
library(plyr)
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
install.packages("broom")
library(broom)
install.packages("caret")
library(caret)

df= read.csv('OnlineNewsPopularity.csv')
df = as.data.frame(scale(df[,3:59], center=FALSE, scale=TRUE)) 
n = dim(df)[1]     # Number of observations = 39644
p = dim(df)[2]-1   # Number of features = 56
y = df[,57] 
X = data.matrix(df[,-57])
X.orig = X

# Apply a 80-20 split for train and test, respectively 
n.train = floor(n*0.8)
n.test = n-n.train

# Rsquare for train and test
M = 1`00`
Rsq.test.rf = rep(0,M) # rf = randomForest
Rsq.train.rf = rep(0,M)
Rsq.test.en = rep(0,M) # en = elastic-net
Rsq.train.en = rep(0,M)
Rsq.test.lasso = rep(0,M) 
Rsq.train.lasso = rep(0,M)
Rsq.test.ridge = rep(0,M) 
Rsq.train.ridge = rep(0,M)

magic_for(silent = TRUE)
for (m in c(1:M)) {
  shuffled_indexes = sample(n)
  train = shuffled_indexes[1:n.train]
  test = shuffled_indexes[(1+n.train):n]
  X.train = X[train, ]
  y.train = y[train]
  X.test = X[test, ]
  y.test = y[test]
  
  # fit elastic-net and calcultae and record the train and test R squares
  cv.fit.en=cv.glmnet(X.train, y.train, alpha = 0.5, nfolds = 10)
  fit.en = glmnet(X.train, y.train, alpha = 0.5, lambda = cv.fit.en$lambda.min)
  y.train.hat.en = predict(fit.en, newx = X.train, type = "response") # y.train.hat = X.train
  y.test.hat.en  = predict(fit.en, newx = X.test, type = "response")  # y.test.hat = X.test
  Rsq.test.en[m] = 1-mean((y.test - y.test.hat.en)^2)/mean((y - mean(y))^2)
  Rsq.train.en[m] = 1-mean((y.train - y.train.hat.en)^2)/mean((y - mean(y))^2)
  
  # fit ridge and calcultae and record the train and test R squares
  cv.fit.ridge = cv.glmnet(X.train, y.train, alpha = 0, nfolds = 10)
  fit.ridge = glmnet(X.train, y.train, alpha = 0, lambda = cv.fit.ridge$lambda.min)
  y.train.hat.ridge = predict(fit.ridge, newx = X.train, type = "response") # y.train.hat = X.train
  y.test.hat.ridge = predict(fit.ridge, newx = X.test, type = "response")  # y.test.hat = X.test
  Rsq.test.ridge[m] = 1-mean((y.test - y.test.hat.ridge)^2)/mean((y - mean(y))^2)
  Rsq.train.ridge[m] = 1-mean((y.train - y.train.hat.ridge)^2)/mean((y - mean(y))^2)
  
  # fit lasso and calcultae and record the train and test R squares
  cv.fit.lasso = cv.glmnet(X.train, y.train, alpha = 1, nfolds = 10)
  fit.lasso = glmnet(X.train, y.train, alpha = 0, lambda = cv.fit.lasso$lambda.min)
  y.train.hat.lasso = predict(fit.lasso, newx = X.train, type = "response")
  y.test.hat.lasso = predict(fit.lasso, newx = X.test, type = "response")
  Rsq.test.lasso[m] = 1-mean((y.test - y.test.hat.lasso)^2)/mean((y - mean(y))^2)
  Rsq.train.lasso[m] = 1-mean((y.train - y.train.hat.lasso)^2)/mean((y - mean(y))^2)
  
  # fit RF and calculate and record the train and test R squares
  rf = randomForest(X.train, y.train, mtry = sqrt(p), importance = TRUE)
  y.test.hat = predict(rf, X.test)
  y.train.hat = predict(rf, X.train)
  Rsq.test.rf[m] = 1-mean((y.test - y.test.hat)^2)/mean((y - mean(y))^2)
  Rsq.train.rf[m] = 1-mean((y.train - y.train.hat)^2)/mean((y - mean(y))^2)
  
  # catch for-loop results
  put(Rsq.test.rf[m], Rsq.test.en[m], Rsq.train.rf[m], Rsq.train.en[m], Rsq.test.lasso[m], Rsq.train.lasso[m],Rsq.test.ridge[m], Rsq.train.ridge[m])
  
}

# save for-loop results as a dataframe
Rsquared_results = magic_result_as_dataframe() 
# trim & organize dataframe
Rsquared_results = Rsquared_results[,2:9]
Rsquared_results = round(Rsquared_results[,1:8],2)

Rsq.test.rf = data.frame(group = "Random Forest", value = Rsquared_results[,1])
Rsq.test.en = data.frame(group = "Elastic-Net", value = Rsquared_results[,2])
Rsq.train.rf = data.frame(group = "Random Forest", value = Rsquared_results[,3])
Rsq.train.en = data.frame(group = "Elastic-Net", value = Rsquared_results[,4])
Rsq.test.lasso = data.frame(group = "LASSO", value = Rsquared_results[,5])
Rsq.train.lasso = data.frame(group = "LASSO", value = Rsquared_results[,6])
Rsq.test.ridge = data.frame(group = "Ridge", value = Rsquared_results[,7])
Rsq.train.ridge = data.frame(group = "Ridge", value = Rsquared_results[,8])

plot.train.data = rbind(Rsq.train.ridge, Rsq.train.en, Rsq.train.lasso, Rsq.train.rf)
plot.test.data = rbind(Rsq.test.ridge, Rsq.test.en, Rsq.test.lasso, Rsq.test.rf)

training_Rsquared = ggplot(plot.train.data, aes(x=group, y=value, color=group)) + 
  scale_x_discrete(limits=c("Ridge", 'Elastic-Net', 'LASSO', 'Random Forest')) + 
  #scale_y_continuous(breaks =seq(0, 1, .1), limit = c(0, 1)) +
  labs(title="Training R-Squared (100 Iterations per Model)",x="Models", y = "R-Squared") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16) +
  coord_cartesian(ylim = c(0, 1)) +
  stat_summary(fun=mean, geom="point", shape=5, size=4, color='red') +
  theme_economist()

testing_Rsquared = ggplot(plot.test.data, aes(x=group, y=value, color=group)) + 
  scale_x_discrete(limits=c("Ridge", 'Elastic-Net', 'LASSO', 'Random Forest')) + 
  #scale_y_continuous(breaks =seq(0, 1, .1), limit = c(0, 1)) +
  labs(title="Testing R-Squared (100 Iterations per Model)",x="Models", y = "R-Squared") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16) +
  coord_cartesian(ylim = c(0, 1)) +
  stat_summary(fun=mean, geom="point", shape=5, size=4, color='red') +
  theme_economist()

Rsquared_plots = grid.arrange(training_Rsquared, testing_Rsquared, ncol=1, nrow = 2)
############################################################################################################
bootstrapSamples =     100
beta.rf.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)    
beta.en.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)    
beta.lasso.bs    = matrix(0, nrow = p, ncol = bootstrapSamples) 
beta.ridge.bs    = matrix(0, nrow = p, ncol = bootstrapSamples) 

for (m in 1:bootstrapSamples){
  bs_indexes       =     sample(n, replace=T)
  X.bs             =     X[bs_indexes, ]
  y.bs             =     y[bs_indexes]
  
  # fit bs rf
  rf               =     randomForest(X.bs, y.bs, mtry = sqrt(p), importance = TRUE)
  beta.rf.bs[,m]   =     as.vector(rf$importance[,1])
  
  # fit bs en
  cv.fit.en           =     cv.glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0.5, nfolds = 10)
  fit.en              =     glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0.5, lambda = cv.fit.en$lambda.min)  
  beta.en.bs[,m]   =     as.vector(fit.en$beta)
  cat(sprintf("Bootstrap Sample %3.f \n", m))
  
  # fit bs lasso
  cv.fit.lasso           =     cv.glmnet(X.bs, y.bs, intercept = FALSE, alpha = 1, nfolds = 10)
  fit.lasso              =     glmnet(X.bs, y.bs, intercept = FALSE, alpha = 1, lambda = cv.fit.lasso$lambda.min)  
  beta.lasso.bs[,m]   =     as.vector(fit.lasso$beta)
  cat(sprintf("Bootstrap Sample %3.f \n", m))
  
  # fit bs ridge
  cv.fit.ridge           =     cv.glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0, nfolds = 10)
  fit.ridge              =     glmnet(X.bs, y.bs, intercept = FALSE, alpha = 0, lambda = cv.fit.ridge$lambda.min)  
  beta.ridge.bs[,m]   =     as.vector(fit.ridge$beta)
  cat(sprintf("Bootstrap Sample %3.f \n", m))
  
  
  
}
############################################################################################################
# calculate bootstrapped standard errors / alternatively you could use qunatiles to find upper and lower bounds
rf.bs.sd    = apply(beta.rf.bs, 1, "sd")
en.bs.sd    = apply(beta.en.bs, 1, "sd")
lasso.bs.sd = apply(beta.lasso.bs, 1, "sd")
ridge.bs.sd = apply(beta.ridge.bs, 1, "sd")

# fit rf to the whole data
rf               =     randomForest(X, y, mtry = sqrt(p), importance = TRUE)

# fit en to the whole data
cv.fit.en2           =     cv.glmnet(X, y, alpha = 0.5 , nfolds = 10)
fit.en2              =     glmnet(X, y, alpha = 0.5 , lambda = cv.fit.en2$lambda.min)

# fit lasso to the whole data
cv.fit.lasso2           =     cv.glmnet(X, y, alpha = 0.5 , nfolds = 10)
fit.lasso2              =     glmnet(X, y, alpha = 0.5 , lambda = cv.fit.lasso2$lambda.min)

# fit ridge to the whole data
cv.fit.ridge2           =     cv.glmnet(X, y, alpha = 0.5 , nfolds = 10)
fit.ridge2              =     glmnet(X, y, alpha = 0.5 , lambda = cv.fit.ridge2$lambda.min)
############################################################################################################
betaS.rf               =     data.frame(c(1:p), as.vector(rf$importance[,1]), 2*rf.bs.sd)
colnames(betaS.rf)     =     c( "feature", "value", "err")

betaS.en               =     data.frame(c(1:p), as.vector(fit.en2$beta), 2*en.bs.sd)
colnames(betaS.en)     =     c( "feature", "value", "err")

betaS.lasso               =     data.frame(c(1:p), as.vector(fit.lasso2$beta), 2*lasso.bs.sd)
colnames(betaS.lasso)     =     c( "feature", "value", "err")

betaS.ridge               =     data.frame(c(1:p), as.vector(fit.ridge2$beta), 2*ridge.bs.sd)
colnames(betaS.ridge)     =     c( "feature", "value", "err")

betaS.rf$feature     =  factor(betaS.rf$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.en$feature     =  factor(betaS.en$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.lasso$feature     =  factor(betaS.en$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.ridge$feature     =  factor(betaS.en$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])

rfPlot =  ggplot(betaS.rf, aes(x=factor(feature), y=value)) +
  geom_bar(stat = "identity", fill="white", colour="blue")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=0.2, size=.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="Random Forest Bootstrap",x="Predictor Index", y = "Value") +
  theme_calc()

enPlot =  ggplot(betaS.en, aes(x=factor(feature), y=value)) +
  geom_bar(stat = "identity", fill="white", colour="blue")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=0.2, size=.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="Elastic-Net Bootstrap",x="Predictor Index", y = "Value") +
  theme_calc()

lassoPlot =  ggplot(betaS.lasso, aes(x=factor(feature), y=value)) +
  geom_bar(stat = "identity", fill="white", colour="blue")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=0.2, size=.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="LASSO Bootstrap",x="Predictor Index", y = "Value") +
  theme_calc()

ridgePlot =  ggplot(betaS.ridge, aes(x=factor(feature), y=value)) +
  geom_bar(stat = "identity", fill="white", colour="blue")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=0.2, size=.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title="Ridge Bootstrap",x="Predictor Index", y = "Value") +
  theme_calc()

grid.arrange(rfPlot, enPlot, lassoPlot, ridgePlot, nrow = 4)
#####################################################################################################################
# SPLIT THE DATA 80-20 TRAIN-SPLIT
set.seed(4)
ind = sample(2, nrow(df), replace = T, prob = c(0.8,0.2))
n.train = df[ind==1,]
n.test = df[ind==2,]

# PREPARE THE DATA FOR LASSO, ridge, and elastic-net
X.train = n.train[,1:56]
y.train = n.train[,57]
X.test = n.test[,1:56]
y.test = n.test[,57]

# ridge model
ridge_model  = cv.glmnet(as.matrix(X.train), y.train, alpha = 0)
plot(ridge_model)
ridge_model$lambda.min # the model indicates that this is the minimum lambda
ridge_model$lambda.1se # the model indicates that this is the lambda choosridge with 1sd rule
ridge_fit = glmnet(as.matrix(X.train), y.train, alpha = 0, lambda = ridge_model$ridge_model$lambda.1se)
plot(ridge_fit)
y.train.hat.ridge = predict(ridge_fit, newx = as.matrix(X.train), type = "response") 
y.test.hat.ridge = predict(ridge_fit, newx = as.matrix(X.test), type = "response") 
ridge_train.err = sqrt(mean((y.train.hat.ridge - y.train)^2))
ridge_test.err = sqrt(mean((y.test.hat.ridge - y.test)^2))
cat(sprintf("alpha = %.1f| ridge train error=%.3f| ridge test error=%.3f \n", 0, ridge_train.err, ridge_test.err))
barplot(as.vector(ridge_fit$beta), main = paste("alpha = ", 0.5))

# elastic-net model
elastic_model  = cv.glmnet(as.matrix(X.train), y.train, alpha = 0.5)
plot(elastic_model)
elastic_model$lambda.min # the model indicates that this is the minimum lambda
elastic_model$lambda.1se # the model indicates that this is the lambda choosen with 1sd rule
elastic_fit = glmnet(as.matrix(X.train), y.train, alpha = 0.5, lambda = elastic_model$elastic_model$lambda.1se)
plot(elastic_fit)
y.train.hat.en = predict(elastic_fit, newx = as.matrix(X.train), type = "response") 
y.test.hat.en = predict(elastic_fit, newx = as.matrix(X.test), type = "response") 
elastic_train.err = sqrt(mean((y.train.hat.en - y.train)^2))
elastic_test.err = sqrt(mean((y.test.hat.en - y.test)^2))
cat(sprintf("alpha = %.1f| elastic-net train error=%.3f| elastic-net test error=%.3f \n", 0.5, elastic_train.err, elastic_test.err))
barplot(as.vector(elastic_fit$beta), main = paste("alpha = ", 0.5))

# Lasso model
lasso_model  = cv.glmnet(as.matrix(X.train), y.train, alpha = 1)
plot(lasso_model)
lasso_model$lambda.min # the model indicates that this is the minimum lambda
lasso_model$lambda.1se # the model indicates that this is the lambda choosen with 1sd rule

lasso_fit = glmnet(as.matrix(X.train), y.train, alpha = 1, lambda = lasso_model$lasso_model$lambda.1se)
plot(lasso_fit)
y.train.hat.lasso = predict(lasso_fit, newx = as.matrix(X.train), type = "response") 
y.test.hat.lasso = predict(lasso_fit, newx = as.matrix(X.test), type = "response") 
lasso_train.err = sqrt(mean((y.train.hat.lasso - y.train)^2))
lasso_test.err = sqrt(mean((y.test.hat.lasso - y.test)^2))
cat(sprintf("alpha = %.1f| lasso train error=%.3f| lasso test error=%.3f \n", 1, lasso_train.err, lasso_test.err))
barplot(as.vector(lasso_fit$beta), main = paste("alpha = ", 1))

par(mfrow=c(1,3))
plot(ridge_model)
plot(elastic_model)
plot(lasso_model)

#####################################################################################################################
n = dim(df)[1]     # Number of observations = 39644
p = dim(df)[2]-1   # Number of features = 58

rf = randomForest(X.train, y.train, mtry = sqrt(p), importance=TRUE)
y.test.hat.rf = predict(rf, X.test)
y.train.hat.rf = predict(rf, X.train)
Rsq.test.rf[m] = 1-mean((y.test - y.test.hat.rf[,50])^2)/mean((y - mean(y))^2)
Rsq.train.rf[m] = 1-mean((y.train - y.train.hat.rf[,50])^2)/mean((y - mean(y))^2)

rf_train_error = data.frame(group = "Random Forest", value = (y.train.hat.rf - y.train))

#####################################################################################################################
ridge_train_error = data.frame(group = "Ridge Train", value = (y.train.hat.ridge[,50] - y.train))
en_train_error = data.frame(group = "EN Train", value = (y.train.hat.en[,50] - y.train))
lasso_train_error = data.frame(group = "LASSO Train", value = (y.train.hat.lasso[,50] - y.train))
rf_train_error = data.frame(group = "RF Train", value = (y.train.hat.rf - y.train))

plot.train.error.data = rbind(ridge_train_error, en_train_error, lasso_train_error, rf_train_error)

training_error = ggplot(plot.train.error.data, aes(x=group, y=value, color=group)) + 
  #scale_y_continuous(breaks =seq(0, 1, .1), limit = c(0, 1)) +
  labs(title="Training Residuals",x="Models", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16) +
  theme_economist()

#####################################################################################################################
ridge_test_error = data.frame(group = "Ridge Test", value = (y.test.hat.ridge[,50] - y.test))
en_test_error = data.frame(group = "EN Test", value = (y.test.hat.en[,50] - y.test))
lasso_test_error = data.frame(group = "LASSO Test", value = (y.test.hat.lasso[,50] - y.test))
rf_test_error = data.frame(group = "RF Test", value = (y.test.hat.rf - y.test))

plot.test.error.data = rbind(ridge_test_error, en_test_error, lasso_test_error, rf_test_error)

testing_error = ggplot(plot.test.error.data, aes(x=group, y=value, color=group)) + 
  labs(title="Testing Residuals",x="Models", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_economist()

Error_plots = grid.arrange(training_error, testing_error, ncol=1, nrow = 2)
#####################################################################################################################
