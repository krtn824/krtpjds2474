library(dplyr)  ## For Data Manipulation
library(mltools)  ## For One  encoding (Converting factor variable to a numerical variable)
library(data.table)  ## For Data manipulation
library(psych)# For addditional summary Statistics
glimpse(Hospital)
colnames(Hospital)
rownames(Hospital)


#Descriptive Statistics
dim(Hospital)
summary(Hospital)
str(Hospital)
describe(Hospital)
table(Hospital$AGE)

cor(Hospital$TOTCHG,Hospital$APRDRG)
cor(Hospital$RACE,Hospital$TOTCHG)
sum(is.na(Hospital))
head(Hospital)
tail(Hospital)




#Q1
hist(Hospital$AGE,col="blue",title="age category of people who frequently visit the hospital ")
# This shows that the frequency of thr age of patient visiting hospital is lying between 0-2 years 
Expense = aggregate(TOTCHG ~ AGE, FUN=sum, data=Hospital)
which.max(tapply(Expense$TOTCHG, Expense$TOTCHG, FUN=sum))


#Q2
hist(Hospital$APRDRG,col="green",title="frequency of diagnosis related group that has maximum hospitalization and expenditure")
#frequency of All Patient Refined Diagnosis Related Groups 
table(Hospital$APRDRG)

max(table(Hospital$APRDRG))

diagnosiscost<-aggregate(TOTCHG~APRDRG,FUN = sum,data=Hospital)
diagnosiscost
diagnosiscost[which.max(diagnosiscost$TOTCHG),]

#Q3
#To check if any missing values are present or not 
Hospital<-na.omit(Hospital)
anyNA(Hospital)

summary(as.factor(Hospital$RACE))
#To observe if the Race of Patient is related to the hospital costs 
model<-lm(TOTCHG~RACE,data=Hospital)
model
summary(model)

riaov <- aov(TOTCHG ~ RACE, data=Hospital)
riaov
summary.aov(riaov)

#Q4 Creating a linear regression model

model1<-lm(TOTCHG~AGE+FEMALE,data = Hospital)
model1
summary(model1)
#Conclusion which can be drawn from the model can be written as follows
# As Age is an important factor in Hospital Costs as we can see  from the 
#p values and level of signifiicance Gender seems to have an impact  on the
#given model and the negative coefficient values shown that females inccur
# less cost than males .
#Q5

riaov1=aov(LOS~AGE+FEMALE+RACE,data=Hospital)
riaov1
summary.aov(riaov1)




model2<-lm(LOS~AGE+FEMALE+RACE,data=Hospital)
model2
summary(model2)
table(Hospital$FEMALE)
#The conclusion which is drawn from the model is that the p values of  Age Female Race is very high 
#There is no relationship between the variables Hence we cannot predict the length of stay on the basis of 
#the variables present in the model
#Q6
model3=lm(TOTCHG~.,data=Hospital)
model3
summary(model3)

#Based on the model output it shows that the Age and LOS shows that both of the variables will be 
#affecting the cost And cost is directly proportional to the  length of the stay And we can observe that   
#for increase of 1LOS Unit=743.1521  units of it  also the increase of 1 Unit of Age is 134.6949 units



model4=lm(TOTCHG~AGE+LOS+APRDRG,data=Hospital)
model4
summary(model4)


model5=lm(TOTCHG~AGE+FEMALE+RACE+LOS,data=Hospital)
model5
summary(model5)

model6=lm(TOTCHG~AGE+RACE,data=Hospital)
model6
summary(model6)

model8=lm(TOTCHG~AGE+LOS,data=Hospital)
model8
summary(model8)

#The conclusions which can be drawn from the model are as follows 
#As it is apparent from the coefficient values, Age, Length of stay (LOS) and patient refined diagnosis related groups(APRDRG) have three stars (***) next to it. So they are the ones with statistical significance
#Also, RACE is the least significant. build a model after removing RACE 
#We observe that APRDRG has t values which is negative value and hence we decided to drop it 
#And in the model 6 it shows that the Standard Error Increases to 3865 on 496 degrees of freedom
