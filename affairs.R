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
plot(affair$affairs,pred)
plot(pred)

#model <- glm(affairs~.,data=affair,family ="integer")
#model<- glm(formula=affairs~.,family ="integer", data =affair)
model_1<- glm(affairs~., data = affair)

#to calculate odd ratio
exp(coef(model_1))
summary(model_1)
#predict(model_1,affair,type = "responce")
pred_1<- predict(model_1,affair)
pred_1

##confusion metrix by considering threshold value as 0.5 
#confusion<-table(pred_1>0.5,affair$affairs)
#confusion
## Model Accuracy 
#Accuracy<-sum(diag(confusion)/sum(confusion))
#Accuracy # 23.79

#confusion metrix by considering threshold value as 3
confusion1<-table(pred_1>3,affair$affairs)
confusion1
# Model Accuracy 
Accuracy1<-sum(diag(confusion1)/sum(confusion1))
Accuracy1 # 70.71

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(pred_1>=3,12,0)
yes_no <- ifelse(pred_1>=3,"yes","no")

# Creating new column to store the above values
affair[,"pred_1"] <- pred_1
affair[,"pred_values"] <- pred_values
affair[,"yes_no"] <- yes_no

#View(affair[,c(1,7:9)])

table(affair$affairs,affair$pred_values)
max(affair$affairs)

summary(model_1)
