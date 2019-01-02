library(e1071) 
library(readxl)
churndata <- read_excel(file.choose())
View(churndata)
str(churndata)

# Changing column names for easy reference
colnames(churndata) <- c("ID","Customer Age","Churn","CHI Score Month 0","CHI Score Diff","Support Cases Month 0","Support Cases Diff","SP Month 0","SP Diff","Logins Diff","Blog Articles Diff","Views Diff","Days Since Last Login Diff")
### Univariate analysis ###
churndata$Churn<-factor(churndata$Churn)
#check for normal distribution
plot(density(churndata$`Customer Age`), lwd = 2.5, main = "Age")
skewness(churndata$`Customer Age`);kurtosis(churndata$`Customer Age`)
# right skew

plot(density(churndata$`CHI Score Month 0`), lwd = 2.5, main = "Scoremonth0")
skewness(churndata$`CHI Score Month 0`);kurtosis(churndata$`CHI Score Month 0`)
# multimodal

plot(density(churndata$`CHI Score Diff`), lwd = 2.5, main = "Chi score diff")
skewness(churndata$`CHI Score Diff`);kurtosis(churndata$`CHI Score Diff`)
#normal

plot(density(churndata$`Support Cases Month 0`), lwd = 2.5, main = "Support Cases Month 0")
# multimodal 

table(churndata$`Support Cases Month 0`)
plot(density(churndata$`Support Cases Diff`), lwd = 2.5, main = "Support Cases diff")
skewness(churndata$`Support Cases Diff`);kurtosis(churndata$`Support Cases Diff`)
# normal

plot(density(churndata$`SP Month 0`), lwd = 2.5, main = "SP Month 0")
skewness(churndata$`SP Month 0`);kurtosis(churndata$`SP Month 0`)
# bimodal

plot(density(churndata$`SP Diff`), lwd = 2.5, main = "SP Diff")
skewness(churndata$`SP Diff`);kurtosis(churndata$`SP Diff`)
# multimodal

plot(density(churndata$`Logins Diff`), lwd = 2.5, main = "Logins Diff")
skewness(churndata$`Logins Diff`);kurtosis(churndata$`Logins Diff`)
#right skew

plot(density(churndata$`Blog Articles Diff`), lwd = 2.5, main = "Blog Articles Diff")
skewness(churndata$`Blog Articles Diff`);kurtosis(churndata$`Blog Articles Diff`)
# right skew

plot(density(churndata$`Views Diff`), lwd = 2.5, main = "Views Diff")
skewness(churndata$`Views Diff`);kurtosis(churndata$`Views Diff`)
# heavy right skew

plot(density(churndata$`Days Since Last Login Diff`), lwd = 2.5, main = "Days Since Last Login Diff")
skewness(churndata$`Days Since Last Login Diff`);kurtosis(churndata$`Days Since Last Login Diff`)
# heavy left skew 

### Check for multi collinearity ###

library(corrplot)
churnnum <- churndata[, c(2,4,5,6,7,8,9,10,11,12,13)]
cormat <-  cor(churnnum) 
round(cormat, 2)
corrplot(cormat,method = "circle", addCoef.col = "black", diag = F, type = "upper")

# Support Cases and Priority of Month December are highly correlated(0.65)

# Question 1: Is Wall's belief about the dependence of churn rates on customer age supported by the data?

boxplot(churndata$`Customer Age` ~ Churn, data=churndata, main="Churn vs age", 
        xlab="churn", ylab="age",
        col=c("orange", "lightblue4"))

#For better visualisation, Splitting the categories into different bins

hist(churndata$`Customer Age`)
cust_age = cut(churndata$`Customer Age`, c(0,6,14,68))
summary(cust_age)
# 0 to 6  2050
# 6 to 14  1902
# 14 to 68  2394

tab <- table(cust_age, churndata$Churn)
# Churn rate in each category 
# 0 to 6  2 % 
# 6 to 14  7.7%
# 14 to 68 5.5 % 
barplot(prop.table(tab,2),xlab="churn",ylab="age",col=c("darkgrey", "darkgoldenrod1","green"), main="Age vs churn")
legend("bottomright", legend = c("0 to 6", "6 to 14","14 to 68"), fill = c("darkgrey","darkgoldenrod1","green"))   


# The data supports Wall's fact that when the customers
# are between 6 to 14 age group they become the most riskiest compared to other groups

# Question 2: Construct the best model to predict customers churn.

table(churndata$Churn)
#   0    1   
# 6024  323

# Data should be balanced to remove the skewness. 
# Using Synthetic Data Generation (Balancing the data)

install.packages("ROSE")
library(ROSE)
View(churndata)

set.seed(123)
index<-sample(2,nrow(churndata),replace=TRUE,prob=c(0.7,0.3))
data_train<-churndata[index==1,]
data_test<-churndata[index==2,]

table(data_test$Churn)
table(data_train$Churn)
colnames(data_train)<-c("id","cust_age","churn","CHIsc","CHIsc_diff","supcase","supcase_diff","SP","SP_diff","login_diff","blog_diff","views_diff","logins_diff") 
colnames(data_test)<-c("id","cust_age","churn","CHIsc","CHIsc_diff","supcase","supcase_diff","SP","SP_diff","login_diff","blog_diff","views_diff","logins_diff") 
nrow(data_train)
table(data_train$churn)

datatrain_ROSE<-ROSE(churn ~ .,data = data_train,seed=123)$data
nrow(datatrain_ROSE)
View(datatrain_ROSE)
table(datatrain_ROSE$churn)
churnnum <- datatrain_ROSE[, c(2,4,5,6,7,8,9,10,11,12,13)]
cormat <-  cor(churnnum) 
corrplot(cormat, method = "circle", addCoef.col = "black", diag = F,type = "upper")

# There is no multi collinearity

# Modelling on Rose data
library(rpart)
library(rpart.plot)
library(caret)

tree.rose <- rpart(churn ~ ., data = datatrain_ROSE)
rpart.plot(tree.rose)
pred.tree.rose <- predict(tree.rose, newdata = data_test,type='class')
pred.tree.rose<-factor(pred.tree.rose)
class(pred.tree.rose)

# Model 01:
#  Logistic Regression
logit = glm(churn~., data= datatrain_ROSE[,-1], family = "binomial")
summary(logit)

# Stepwise Regression
null = glm(churn~1, data= datatrain_ROSE[,-1], family = "binomial") 
full = glm(churn~., data= datatrain_ROSE[,-1], family = "binomial")
step(null, scope=list(lower=null, upper=full), direction="both")

# Models through variable selection (Stepwise Regression)
logit1=glm(formula = churn ~ CHIsc + login_diff + CHIsc_diff + cust_age + 
             supcase_diff + supcase + views_diff, family = "binomial", 
           data = datatrain_ROSE[, -1])
summary(logit1)

# Cutoff probability: Logistic regression
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}

# Creating data frame with probabilities and labels
# Predict the values for the test data with response values

mod1_data<-data.frame(predict(logit1,data_test, type="response"),data_test$churn)
colnames(mod1_data)<-c("predictions","labels")

# Calculating values for ROC curve
library(ROCR)
pred<-prediction(mod1_data$predictions, mod1_data$labels)
perf<-performance(pred,"tpr","fpr")

# Plotting curve
plot(perf,col="black",lty=3,lwd=3)

# Calculating cutoff probability
print(opt.cut(perf, pred))
# cut-off probability: 0.528

# Prediction using cutoff probability
prediction<-predict(logit1, newdata=data_test, type="response")
prediction<-data.frame(prediction)

#If-else condition to assign the class of Churn rate on basis of response prediction
predicted<-ifelse(prediction>0.53,1,0)
predicted<-factor(predicted)

# Model evaluation using confusion matrix
confusionMatrix(predicted,data_test$churn,positive='1')

# Performance test
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"));auc
# AUC: 0.6371385

# Model 02:
# Decision Trees
library(rpart)

churn_rp <-rpart(churn~., data = datatrain_ROSE[,-1], control = rpart.control(minsplit = 10))
opt <-which.min(churn_rp$cptable[ ,"xerror"]) 

#Prunning with the best CP value
cp <-churn_rp$cptable[opt, "CP"]
churn_prune <-prune(churn_rp, cp = cp)
summary(churn_prune)
print(churn_prune)
rpart.plot(churn_prune)

#Prediction on test
prediction<-predict (churn_prune,data_test, type="class")

#Model evaluation using confusion matrix
confusionMatrix (prediction,data_test$churn,positive='1')
# Accuracy : 0.4101 
# Sensitivity : 0.80952         
# Specificity : 0.39159

# Performance test
library(ROCR)
mod2_data<-data.frame(predict(churn_prune,data_test, type="prob"),data_test$churn)
pred1<-prediction(mod2_data[,2],mod2_data[,3])
perf1<-performance(pred1,"tpr","fpr")

# Plotting curve
plot(perf1,col="black",lty=3,lwd=3)

#Performance test
auc1 <- performance(pred1, "auc")
auc1 <- unlist(slot(auc1, "y.values"));auc1
# 0.64

# Model 03:
# Neural Networks
library(nnet)
data_net<-datatrain_ROSE

# Convert the variables on the max-min scale for the Neural Network
maxs <- sapply(data_net[,-c(1,3)], max) 
mins <- sapply(data_net[,-c(1,3)], min)

scaled <- as.data.frame(scale(data_net[,-c(1,3)], center = mins, scale = maxs - mins))
scaled <- cbind(Churn_Rate=data_net$churn, scaled)

nn<- nnet(Churn_Rate ~ .,data=scaled,linout=F,size = 10,decay =0.01,maxit=1000)
summary(nn)

# Test data
maxs <- sapply(data_test[,-c(1,3)], max) 
mins <- sapply(data_test[,-c(1,3)], min)
test_scaled <- as.data.frame(scale(data_test[,-c(1,3)], center = mins, scale = maxs - mins))
test_scaled <- cbind(Churn_Rate=data_test$churn, test_scaled)
nn.preds = predict(nn,test_scaled,type = 'class')
table(nn.preds)
table(test_scaled$Churn_Rate,nn.preds)
nn.preds<-factor(nn.preds)

# Model evaluation using confusion matrix
confusionMatrix(nn.preds,test_scaled$Churn_Rate,positive='1')

# Accuracy : 0.9556  

table(test_scaled$Churn)
table(nn.preds)
#ROC Curve and Performance test
roc.curve(nn.preds,test_scaled$Churn)
# auc: 0.525
# Since the AUC value is approximately 0.5, this is worst model.

# Model 04:
# Random forest
install.packages("randomForest")
library(randomForest)
set.seed(123)
rf = randomForest(churn~.,data=datatrain_ROSE[,-1], ntree = 500, proximity = TRUE, replace = TRUE, sampsize = ceiling(0.65*nrow(datatrain_ROSE)), importance = TRUE, mtry = sqrt(ncol(datatrain_ROSE)-1));
rf$importance
plot(rf)

# To get optimal value of mtry
tuneRF(datatrain_ROSE[,-c(1,3)],datatrain_ROSE[,3],stepFactor = 0.5)

# It gives mtry=3
rf1 = randomForest(churn~.,data=datatrain_ROSE[,-1], ntree = 500, proximity = TRUE, replace = TRUE, sampsize = ceiling(0.65*nrow(datatrain_ROSE)), importance = TRUE, mtry = 3);
rf1$importance
plot(rf1)
legend("topright", legend = colnames(rf$err.rate), cex=0.5, lty=c(1,2,3,4), col=c(1,2,3,4), horiz=T)
# OOB error rate and class 0 and 1 error is lower
varImpPlot(rf1)


#Test Data
pred = predict(rf1,newdata = data_test)
pred
table(pred, data_test$churn)
confusionMatrix(pred,data_test$churn,positive='1')

# Model Selection
#Among all the above models we created, Model 2 (Decision trees) is best. 
#Reason: Model 2 has the highest sensitivity. In given case, predicting correct churn rate '1' is more important. So, we are considering Recall as important model selection parameter. 

#Sensitivity
#Model 1: Logistic Regression -> 0.78779
#Model 2: Decision trees -> 0.88043 
#Model 3: Neural Network -> 0.00000
#Model 4: Random Forest -> 0.77174        

# Question 3 : provide the list of 100 customers with highest churn probabilities and the top three drivers of churn for each customer
# Choosing Decision trees as the final model
pred_out<-predict(churn_prune,data_test, type="prob")
output <- (1:nrow(data_test))[order(pred_out[,2], decreasing=T)[1:100]]
output
summary(churn_prune)
# Taking the variable importance, we get the blog_diff as the most important variable followed by login_days and views_diff
# Hence, we can take these variables to predict the churn rate


















