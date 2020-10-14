
suppressMessages(library(tidyverse))
suppressMessages(library(caret))
suppressMessages(library(ggplot2))

#Make sure to set the correct  working directory where your files are saved in a csv format.
setwd("C:/Users/User/Desktop/Kaggle")
titanic=read.csv("titanic_train.csv") 
titanic_testing=read.csv("titanic_test.csv")


# Make a colum for testing, NA.
titanic_testing$Survived=NA #This is what we want to predict eventually
titanic_testing=titanic_testing[ ,c(12,1:11)] #Simple reorder of the columns

titanic_full=rbind(titanic,titanic_testing) 
# Create a full data set to use for imputing missing values and or feature engineering. 


print(paste("There are:",sum(titanic$Sex=="male"),"Men"))
print(paste("There are:",sum(titanic$Sex=="female"),"Females"))

ggplot(data=titanic)+
  geom_bar(mapping=aes(x=Survived,fill=Sex))


#Prepare data for the plots 
Class=as.factor(titanic$Pclass)

ggplot(data=titanic)+
  geom_bar(mapping=aes(x=Survived,fill=Class))


print(paste("There are:",sum(titanic$Pclass==1),"First class passengers"))
print(paste("There are:",sum(titanic$Pclass==2),"Second class passengers"))
print(paste("There are:",sum(titanic$Pclass==3),"Third class passengers"))



ggplot(data=titanic)+
  geom_bar(mapping=aes(x=Survived,fill=Class))+
  facet_wrap(~Sex)


titanic_full[1044,'Fare']=mean(titanic_full[titanic_full$Pclass==3,]$Fare,na.rm=T)
#Impute the one missing value of Fare based on their class.
titanic_testing[153,'Fare']=mean(titanic_full[titanic_full$Pclass==3,]$Fare,na.rm=T)

titanic_full$AgeF=cut(titanic_full$Age,seq(0,max(titanic_full$Age,na.rm=T),5)) 
#We will consider age as a factor split, the groups will be 
#0-5,5-10,. Plus a NA category that will represent missing age.


titanic_full$AgeF=addNA(titanic_full$AgeF) 
# Important! Need to add NA as an actual factor level.


titanic$AgeF=titanic_full$AgeF[1:nrow(titanic)]

titanic_testing$AgeF=titanic_full$AgeF[892:1309]


#Make new column, Title
titanic_full$Title=NA


titanic_full$Title <- gsub('(.*, )|(\\..*)','',titanic_full$Name)
#This trick is from Huijun Zhao, leaves everything but the skeleton of the titles. 
#The reason this works is because all possible titles are sorrounded by either 
#of the two above regular expressions. 
#It could also be done manually using grep.



titanic_full$Title[titanic_full$Title %in% c('Miss','Ms','Mlle')]<-"Miss"
titanic_full$Title[titanic_full$Title %in% c("Capt","Dr","Rev","Jonkheer","Major","Col","Sir","Don")]<-
  "Special men"
titanic_full$Title[titanic_full$Title %in% c('Master')]<-'Master'
titanic_full$Title[titanic_full$Title %in% c("Mr")]<-"Mr"
titanic_full$Title[titanic_full$Title %in% c("Dona","Lady","the Countess")]<-"Special women"
titanic_full$Title[titanic_full$Title %in% c("Mrs","Mme")]<-"Mrs"



#Turn Title into factor

titanic_full$Title=as.factor(titanic_full$Title)

titanic$Title=titanic_full$Title[1:nrow(titanic)] 

titanic_testing$Title=titanic_full$Title[892:1309]

titanic$Survived=as.factor(titanic$Survived) #Make sure survived is a factor not a numeric.


titanic.n=titanic %>% #For modeling we remove columns which don't seem useful. 
  select(-c(PassengerId,Name,Ticket,Cabin,Age,Embarked))
#Age we have as a factor, we used title not name, etc.

fitControl <- trainControl(method = "cv", #We will do 5 cross validation to improve accuracy
                           number = 5)



model_titanic_logistic=caret::train(Survived~.,data=titanic.n,method='glm',
                                    family='binomial',trControl=fitControl) 



logistic_pred=predict(model_titanic_logistic,titanic_testing)


#Prepare to submit for kaggle
pid=titanic_testing$PassengerId # Get a vector of passenger Ids for the testing set
df.sub=data.frame(pid,logistic_pred)
names(df.sub)=c("PassengerId","Survived")