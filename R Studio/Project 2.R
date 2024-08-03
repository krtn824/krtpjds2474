#We will import the dataset And we will find the summary & str of the dataset
head(internet)
tail(internet)
summary(internet)
str(internet)
library(psych)
describe(internet)

# To find if there is missing values in the dataset
sum(is.na(internet))
#There is no missing values present in the dataset

#Q2 As mentioned earlier, a unique page view represents the number of sessions during which that page was viewed one or more times. 
#A visit counts all instances, no matter how many times the same visitor may have been to your site. 
#So the team needs to know whether the unique page view value depends on visits.

cor(internet$Bounces,internet$Uniquepageviews)
cor(internet$Visits,internet$Uniquepageviews)
#We find out the regression model between Visits & Uniquepageviews
model=lm(Visits~Uniquepageviews,data=internet)
summary(model)

ano<-aov(Uniquepageviews~Visits, data=internet)
ano
summary(ano)  

#Q3 Find out the probable factors from the dataset, which could affect the exits. 
#Exit Page Analysis is usually required to get an idea about why a user leaves the website for a session and moves on to another one.
#Please keep in mind that exits should not be confused with bounces

model1<-lm(Exits~.,data=internet)
model1
summary(model1)

anoo<-aov(Exits~.,data = internet)
summary(anoo)

#From the results of anova we find out that Bounces,Uniquepageviews,Timeinpage
# have large amount of significant views & Visits have least amount of significance
#Hence we can say that exit from the site is affected by the factors of source group,
#bounces, and unique.pageviews.

#Q4 Every site wants to increase the time on page for a visitor. 
#This increases the chances of the visitor understanding the site content better and h
#hence there are more chances of a transaction taking place. 
#Find the variables which possibly have an effect on the time on page
anova<-aov(Timeinpage~.,data=internet)
anova
summary(anova)

model2<-lm(Timeinpage~.,data=internet)
model2
summary(model2)

#We found out from the summary of anova that only source group is not affecting the time in page views 
#And rest all are significantly afecting the timein page views

#Q5 A high bounce rate is a cause of alarm for websites which depend on visitor engagement. 
#Help the team in determining the factors that are impacting the bounce.


a<-lm(Bounces~.,data=internet)
a
summary(a)


internet$Bounces=internet$Bounces*0.01
rmm<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = internet,family = "binomial")
rmm
summary(rmm)

