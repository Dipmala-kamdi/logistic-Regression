db <- read.csv(file.choose()) 
View(db)
sum(is.na(db))
db <- na.omit(db) 
# Omitting NA values from the Data 
# omit the rows which has atleast 1 NA value
dim(db)
colnames(db)
model<- glm(affairs~rating,  data = db,family = "binomial")
class(db$affairs)
class(db$rating)
#levels(db$gender)<- c(1,0) #0 indicates  male
str(db)

summary(model)

db$affairs<- as.factor(db$affairs)
db$age <- as.factor(db$age)
db$yearsmarried<- as.factor(db$yearsmarried)
db$religiousness<- as.factor(db$religiousness)
db$education<- as.factor(db$education)
db$occupation<- as.factor(db$occupation)
db$rating<- as.factor(db$rating)

exp(coef(model))


# Confusion matrix table 
prob <- predict(model,db,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,db$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 68.71

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
db[,"prob"] <- prob
db[,"pred_values"] <- pred_values
db[,"yes_no"] <- yes_no

View(db[,c(1,10:12)])

table(db$affairs,db$pred_values)

library(ROCR)
install.packages("ROCR", dependencies=TRUE)
rocrpred<-prediction(prob_1,db$affairs)
r_pred <- prediction(prob_1,db$affairs)
rocr<- prediction(as.factor(prob_1),as.factor(db$affairs))


class(prob)
prob_1<- as.factor(prob)
class(db$affairs)


rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

########################################################
predict.data<- data.frame(probability.of.)