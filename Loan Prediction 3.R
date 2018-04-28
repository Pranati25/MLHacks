loan<-read.csv("D:/Loan Prediction/train.csv",na.strings=c("","NA"))
summary(loan)
colSums(is.na(loan))

loan$LoanAmount<-ifelse(is.na(loan$LoanAmount),mean(loan$LoanAmount,na.rm = TRUE),loan$LoanAmount)
loan$Loan_Amount_Term<-ifelse(is.na(loan$Loan_Amount_Term),median(loan$Loan_Amount_Term,na.rm = TRUE),loan$Loan_Amount_Term)
loan$Credit_History[is.na(loan$Credit_History)]<-1
loan$Married[is.na(loan$Married)]<-'Yes'
loan$Self_Employed[is.na(loan$Self_Employed)]<-'No'
loan$Gender[is.na(loan$Gender)]<-'Male'
loan$Dependents[is.na(loan$Dependents)]<-0
summary(loan)
str(loan)
colSums(is.na(loan))

loantest<-read.csv("D:/Loan Prediction/test.csv",na.strings=c("","NA"))
summary(loantest)
colSums(is.na(loantest))
loantest$LoanAmount<-ifelse(is.na(loantest$LoanAmount),mean(loantest$LoanAmount,na.rm = TRUE),loantest$LoanAmount)
loantest$Loan_Amount_Term<-ifelse(is.na(loantest$Loan_Amount_Term),median(loantest$Loan_Amount_Term,na.rm = TRUE),loantest$Loan_Amount_Term)
loantest$Credit_History[is.na(loantest$Credit_History)]<-1
loantest$Married[is.na(loantest$Married)]<-'Yes'
loantest$Self_Employed[is.na(loantest$Self_Employed)]<-'No'
loantest$Gender[is.na(loantest$Gender)]<-'Male'
loantest$Dependents[is.na(loantest$Dependents)]<-0
summary(loantest)
colSums(is.na(loantest))

#-------------------------------------------------------EDA----------------------------------------------------
library(ggplot2)
library(GGally)

ggplot(loan,aes(x=ApplicantIncome,y=Credit_History,col=ApplicantIncome))+geom_jitter()+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=CoapplicantIncome,y=Credit_History,col=ApplicantIncome))+geom_jitter()+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=LoanAmount,fill=Loan_Status))+geom_bar(width = 3,position = "dodge")+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=Dependents,fill=Loan_Status))+geom_bar(position = "dodge")+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=Gender,fill=Loan_Status))+geom_bar(position = "dodge")+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=Married,fill=Loan_Status))+geom_bar(position = "dodge")+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=Property_Area,fill=Loan_Status))+geom_bar(position = "dodge")+facet_grid(".~Loan_Status")
ggplot(loan,aes(x=Education,fill=Loan_Status))+geom_bar(position = "dodge")+facet_grid(".~Loan_Status")
ggpairs(loan[,c(7:11)])

#-------------------------------------------------model Fitting-----------------------------------------
#generaring test set for accuracy check

id<-sample(2,nrow(loan),prob = c(0.8,0.2),replace = TRUE)
loan_training_new<-loan[id==1,]
loan_test_new<-loan[id==2,]



library(randomForest)
library(caret)
library(pROC)

modeltrain<-randomForest(Loan_Status~Credit_History+Gender+Property_Area+CoapplicantIncome+Dependents+Married+Education+Self_Employed+ApplicantIncome+LoanAmount+Loan_Amount_Term,loan_training_new,ntree=500,importance=TRUE)
modeltrain
importance(modeltrain)
varImpPlot(modeltrain)

modeltrain2<-randomForest(Loan_Status~Credit_History+ApplicantIncome+LoanAmount+Property_Area+CoapplicantIncome+Dependents+Loan_Amount_Term,loan_training_new,ntree=500,importance=TRUE)
modeltrain2
modeltrain2$importance
modelpred<-predict(modeltrain2,loan_test_new)
prop.table(table(modelpred))

confusionMatrix(modelpred,loan_test_new$Loan_Status)
plot.roc(as.numeric(modelpred),as.numeric(loan_test_new$Loan_Status),print.auc=TRUE,col="green",type="b",lwd=3)

#----------------------------Prediction on test dataset--------------------------------------------------

loanpred<-predict(modeltrain2,loantest)
loanpred
prop.table(table(loanpred))