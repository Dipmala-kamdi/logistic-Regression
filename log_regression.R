# Choose the Data set
affair <- read.csv(file.choose()) 
View(affair)
sum(is.na(affair))
affair <- na.omit(affair) 
# Omitting NA values from the Data 
# omit the rows which has atleast 1 NA value
dim(affair)
colnames(affair)

model_affair <- lm(affairs~.,data=affair)
pred <- predict(model_affair,affair)
pred
plot(pred)
plot(affair$affairs,pred)

install.packages('ISLR')
require(ISLR)


affair$affairs<- as.factor(affair$affairs)
affair$age <- as.factor(affair$age)
affair$yearsmarried<- as.factor(affair$yearsmarried)
affair$religiousness<- as.factor(affair$religiousness)
affair$education<- as.factor(affair$education)
affair$occupation<- as.factor(affair$occupation)
affair$rating<- as.factor(affair$rating)

model<- glm(affairs~.,data=affair, family = binomial)
exp(coef(model))
summary(model)

pred_1<- predict(model, affair, type = "response")

#considet threshold value as 0.5
confusion<-table(pred_1>0.5,affair$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 71.54

#creating empty vectors
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(pred_1>=0.5,1,0)
yes_no <- ifelse(pred_1>=0.5,"yes","no")

# Creating new column
affair[,"prob"] <- pred_1
affair[,"pred_values"] <- pred_values
affair[,"yes_no"] <- yes_no

View(affair)

table(affair$affairs,affair$pred_values)

#ROC curve
install.packages('ROCR')
library(ROCR)
install.packages('caret')
library(caret)
class(pred_1)
pred_1<- as.factor(pred_1)
x<- as.factor(affair$prob)
y<- as.factor(affair$affairs)
label<- ifelse(y==0,"0","1")
label

#label<- c(rep(1,length(y)>0),rep(0,length(y)<1))

p_pred<-prediction(x,label)



P_perf<-performance(p_pred,'tpr','fpr')
#########################################################################

library(pROC)
library(randomForest) 
rrr<- sample(2,nrow(affair))
y
