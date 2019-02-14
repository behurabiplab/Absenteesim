rm(list=ls())
getwd()
setwd("C:/Users/user/Desktop/Project/Project")
library(readxl)
absenteesim= read_xls("Absenteeism_at_work_Project.xls", col_names=T)
absenteesim=as.data.frame(absenteesim)

#Removing space in the column
colnames(absenteesim)=tolower(gsub(' ','_',colnames(absenteesim)))
colnames(absenteesim)=gsub('/','_',colnames(absenteesim))
colnames(absenteesim)

table(absenteesim$reason_for_absence)
table(absenteesim$absenteeism_time_in_hours)

#Removing the id column
absenteesim=absenteesim[,-1]


#there is no month as 0 so removing month 0
absenteesim=absenteesim[!absenteesim$month_of_absence==0,]
#making it as a range of factor 
absenteesim$absenteeism_time_in_hours=ifelse(absenteesim$absenteeism_time_in_hours <=2,'le2',
                                             ifelse(absenteesim$absenteeism_time_in_hours >=3 & absenteesim$absenteeism_time_in_hours <=7,'bet3&7','gt8'))
table(absenteesim$absenteeism_time_in_hours)

#converting the varible to factor variable
factor_var=c("reason_for_absence","month_of_absence","day_of_the_week","seasons","disciplinary_failure","education",
             "son","social_drinker","social_smoker","pet","absenteeism_time_in_hours")
for (i in factor_var){
  absenteesim[,which(colnames(absenteesim)==i)]=as.factor(absenteesim[,which(colnames(absenteesim)==i)])
}

str(absenteesim)

#numeric variable
numeric_var=colnames(absenteesim[sapply(absenteesim, is.numeric)])
numeric_var



#train and test split
set.seed(342)
index=sample(nrow(absenteesim),0.8*nrow(absenteesim))
train=absenteesim[index,]
test=absenteesim[-index,]

colSums(is.na(train))

#for random forest
train=train[!is.na(train$absenteeism_time_in_hours),]

prop.table(table(train$absenteeism_time_in_hours))

library(ggplot2)

colSums(is.na(absenteesim))
library(DMwR)
train=knnImputation(absenteesim)

library(ggplot2)
ggplot(data = train,aes(x = absenteeism_time_in_hours,fill =absenteeism_time_in_hours))+
  geom_bar() +  labs(y='absenteesim count', title = 'absenteesim frequency')
#there are more no of employees in who have absenteesim less than 2 hours

#plotting absenteesim on basis of reason of absense
ggplot(data = train,aes(x=train$reason_for_absence,fill = absenteeism_time_in_hours))+
  geom_bar() +  labs(y='absenteesim inhours count', title = 'absenteesim for reason of absense')


#the people having Pregnancy, childbirth and the puerperium have taken more absenteesim more than 8 hours
# the pepple having Congenital malformations, deformations and chromosomal abnormalities have taken absenteesim more than 8 hours
#who is having certain infectious disease have less than 2 hours of absenteesim

ggplot(data = train,aes(x=son,fill=absenteeism_time_in_hours))+geom_bar()+labs(y='absentessim in hour',
                                                                               title='absenteesim for son')



ggplot(data = train,aes(x=social_drinker,fill=absenteeism_time_in_hours))+geom_bar()+labs(y='absentessim in hour',
                                                                                          title='absenteesim for social drinker')


ggplot(data=train,aes(x=month_of_absence,fill=absenteeism_time_in_hours))+geom_bar()
table(train$social_drinker)



#numeric data
par(mfrow=c(3,3))
for (i in numeric_var){
  boxplot(train[,which(colnames(train)==i)],main=paste0(i))
}


for (i in numeric_var){
  out=boxplot.stats(train[,which(colnames(train)==i)])$out
  train[train[,which(colnames(train)==i)] %in% out,which(colnames(train)==i)]=NA
}

sum(is.na(train))

train=knnImputation(train,k=3)

library(caret)
correlation=cor(train[,numeric_var])
correlation
library(corrplot)
corrplot.mixed(correlation,tl.offset=0.01,tl.cex=0.01)
drop_var=findCorrelation(correlation,cutoff = 0.7)
drop_var
drop_var=numeric_var[9]
drop_var


for (i in factor_var){
  print(i)
  print(chisq.test(train$absenteeism_time_in_hours,train[,which(colnames(train)==i)]))
}

drop_var=append(drop_var,c('seasons','education'))

par(mfrow=c(3,3))
numeric_var=colnames(train[sapply(train, is.numeric)])
numeric_var

for (i in numeric_var){
  hist(train[,which(colnames(train)==i)],main=paste0(i))
}


minmax=function(x){
  return((x-min(x))/(max(x)-min(x)))
}


numeric_var

minmax(train$weight)
for (i in numeric_var){
  train[,which(colnames(train)==i)]=minmax(train[,which(colnames(train)==i)])
}


View(train)
str(test)


train=train[,-which(colnames(train) %in% drop_var)]
test=test[,-which(colnames(test) %in% drop_var)]
colnames(train)


colnames(train)
View(train)

##multiple logistic regression
install.packages('nnet')
library(nnet)
ml=multinom(absenteeism_time_in_hours~.,data = train)
summary(ml)

confusion_calculator=function(x){
  for (i in c('bet3&7','gt8','le2')){
    if (i=="bet3&7"){
      TP=x[1,1]
      FP=x[2,1]+x[3,1]
      FN=x[1,2]+x[1,3]
      TN=x[2,2]+x[3,3]
      Recall=TP/(TP+FN)
      specificity=TN/(TN+FP)
      Precission=TP/(TP+FP)
      Accuracy1=(TP+TN)/(TP+TN+FP+FN)
      print('For class between 3 and 7')
      print(paste('Recall is',Recall))
      print(paste('Specificity is ',specificity))
      print(paste('Precission is',Precission))
      print(paste('Accuracy is' , Accuracy1))
    }else if(i=="gt8"){
      TP=x[2,2]
      FP=x[1,2]+x[3,2]
      FN=x[2,1]+x[2,3]
      TN=x[1,1]+x[3,3]
      Recall=TP/(TP+FN)
      specificity=TN/(TN+FP)
      Precission=TP/(TP+FP)
      Accuracy2=(TP+TN)/(TP+TN+FP+FN)
      print("for class greater than 8")
      print(paste('Recall is',Recall))
      print(paste('Specificity is ',specificity))
      print(paste('Precission is',Precission))
      print(paste('Accuracy is' , Accuracy2)) 
    }else if(i=='le2'){
      TP=x[3,3]
      FP=x[1,3]+x[2,3]
      FN=x[3,1]+x[3,2]
      TN=x[1,1]+x[2,2]
      Recall=TP/(TP+FN)
      specificity=TN/(TN+FP)
      Precission=TP/(TP+FP)
      Accuracy3=(TP+TN)/(TP+TN+FP+FN)
      print('For class less than 2')
      print(paste('Recall is',Recall))
      print(paste('Specificity is ',specificity))
      print(paste('Precission is',Precission))
      print(paste('Accuracy is' , Accuracy3))
    }
  }
  Total_Accuracy=(Accuracy1+Accuracy2+Accuracy3)/3
  print(paste('Total accuracy is',Total_Accuracy))
}

###Decission tree
library(rpart)
dt=rpart(absenteeism_time_in_hours~.,data = train,method = 'class',control = rpart.control(minsplit = 20,
                                                                minbucket = 7,maxdepth = 10,xval = 10,usesurrogate = 2))

#method=class means it will not give the probability
#minsplit=minimum number of observation in a node before splitting 
#minbucket=minmum no of node in leaf node
#xval=no of cross validation
#to find the accuracy, precission and recall

predicted=predict(dt,newdata=test,type='class')
predicted
table(test$absenteeism_time_in_hours,predicted)
confusion_calculator(table(test$absenteeism_time_in_hours,predicted))

#Now pruning the tree
printcp(dt)
bestcp=dt$cptable[which.min(dt$cptable[,'xerror']),'CP']
dt_new=prune(dt,cp=bestcp)
predicted=predict(dt_new,test,type='class')
predicted

confusion_calculator(table(test$absenteeism_time_in_hours,predicted))

#Accuracy of the model is 78


##################################Knn###############################

View(train)
newtest=knnImputation(test,k=3)
library(caret)
library(class)
k_train=train[,-which(colnames(train)=='absenteeism_time_in_hours')]
k_test=newtest[,-which(colnames(newtest)=='absenteeism_time_in_hours')]
k_train_labels=train[,which(colnames(train)=='absenteeism_time_in_hours')]
k_test_labels=newtest[,which(colnames(newtest)=='absenteeism_time_in_hours')]
model1=knn(k_train,k_test,k_train_labels,k=5)
confusion_calculator(table(k_test_labels,model1))
#Apllying 10 fold cross validation 
train_ctrl=trainControl(method = "cv",number = 10)
set.seed(3333)
knn_fit = train(absenteeism_time_in_hours ~., data = train, method = "knn",trControl=train_ctrl)
knn_fit                 
 #we are getting less accuracy so it is not correct model

##random forest##################
library(randomForest)
set.seed(123)
rf=randomForest(absenteeism_time_in_hours~.,data = train,ntree=500)
predict=predict(rf,newdata = test)
table(test$absenteeism_time_in_hours,predict)
confusion_calculator(table(test$absenteeism_time_in_hours,predict))
modelLookup('rf')

mtry <- tuneRF(train[-(ncol(train))],train$absenteeism_time_in_hours, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

mtry
best_mtry=mtry[mtry[,2]==min(mtry[,2]),1]
best_mtry
rf=randomForest(absenteeism_time_in_hours~.,data = train,ntree=500,mtry=best_mtry)
confusion_calculator(table(test$absenteeism_time_in_hours,predict))

# #creating automatic tuning parameter
# set.seed(342)
# automate_tune=train(absenteeism_time_in_hours~.,data=train,method='rf')
# automate_tune
# predict=predict(automate_tune,train)
# table(train$absenteeism_time_in_hours,predict)
# 
# predict=predict(automate_tune,test)
# length(predict)
# length(test$absenteeism_time_in_hours)





library(caret)
train=knnImputation(train,k=3)
ctrl=trainControl(method='repeatedcv',number = 10,repeats = 10)
grid=expand.grid(.mtry=c(2,4,6,8,10,12,14,16))
set.seed(32423)
rf1=train(absenteeism_time_in_hours~.,data=train,method='rf',trControl=ctrl,tuneGrid=grid)
rf1

install.packages("inTrees")
library(inTrees)
treelist=RF2List(rf)
exec=extractRules(treelist,train[,-17])
exec[1:2,]
readableRules=presentRules(exec,colnames(train))
readableRules[1:2,]
rulemetric=getRuleMetric(exec,train[,-14],train$absenteeism_time_in_hours)
rulemetric[1:2,]
save.image()

#######################naive bayes#########################
library(e1071)
model1=naiveBayes(absenteeism_time_in_hours~.,data = train,type='class')
predict=predict(model1,test,type = 'class')
confusion_calculator(table(test$absenteeism_time_in_hours,predict))


#so among all the model random forest is giving the best accuracy


