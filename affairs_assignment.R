mydata<- read.csv(file.choose())
View(mydata)
summary(mydata)
plot(mydata$affairs,mydata$gender)


sum(is.na(mydata))
mydata <- na.omit(mydata) 
# Omitting NA values from the Data 
# omit the rows which has atleast 1 NA value
dim(mydata)
colnames(mydata)

model_affair <- lm(affairs~.,data=mydata)
pred <- predict(model_affair,mydata)
pred
plot(pred)
plot(mydata$affairs,pred)

mydata$affairs<- as.factor(mydata$affairs)
mydata$age <- as.factor(mydata$age)
mydata$yearsmarried<- as.factor(mydata$yearsmarried)
mydata$religiousness<- as.factor(mydata$religiousness)
mydata$education<- as.factor(mydata$education)
mydata$occupation<- as.factor(mydata$occupation)
mydata$rating<- as.factor(mydata$rating)

require(ISLR)

y<- as.factor(mydata$affairs)
mydata$affairs<- ifelse(y==0,"no","yes")
mydata$affairs<- as.factor(mydata$affairs)
mydata$affairs

mymodel<- glm(affairs~.,mydata, family ="binomial")
summary(mymodel)

model_1<- glm(affairs~age, mydata, family = binomial)
summary(model_1)



pred_1<- predict(mymodel, mydata, type = "response")
pred_1

#considet threshold value as 0.5
confusion<-table(pred_1>0.5,mydata$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 71.54 when affirs has its original values #after converting 77.20

#creating empty vectors
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(pred_1>=0.5,1,0)
yes_no <- ifelse(pred_1>=0.5,"yes","no")

# Creating new column
mydata[,"prob"] <- pred_1
mydata[,"pred_values"] <- pred_values
mydata[,"yes_no"] <- yes_no

View(mydata)

table(mydata$affairs,mydata$pred_values)

library(ROCR)
library(caret)

x<- as.factor(mydata$prob)
mydata$affairs<- as.factor(mydata$affairs)
pred_1<- as.factor(pred_1)
#prediction(as.factor(pred_1), mydata$affairs)
prediction(pred_1, mydata$affairs)

rocrpred<- prediction(pred_1, mydata$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

#true positive and false positive value
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6) 
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
