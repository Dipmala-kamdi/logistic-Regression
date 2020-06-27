View(bank.full)
bank<- bank.full
View(bank)
dim(bank)
colnames(bank)

model <- glm(y~.,data=bank,family = "binomial")
class(bank$y)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
summary(model)

# Confusion matrix table 
prob <- predict(model,bank,type="response")

#considering trashhold value 0.5
confusion<-table(prob>0.5,bank$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 90.18

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

View(bank)

table(bank$y,bank$pred_values)

#ROC curve
library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#plot(rocrperf,colorize=T)

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)


