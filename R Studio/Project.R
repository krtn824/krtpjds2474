#Loading the required Libraries
library(dplyr)  ## For Data Manipulation
library(ggplot2)  # For Data visualization
library(mltools)  ## For One hot encoding (Converting factor variable to a numerical variable)
library(data.table)  ## For Data manipulation
library(psych)# For Addtional summary Statistics

#Descriptive Statistics
dim(Insurance_factor_identification)
str(Insurance_factor_identification)
summary(Insurance_factor_identification)
library(psych)
describe(Insurance_factor_identification)
head(Insurance_factor_identification)
glimpse(Insurance_factor_identification)
colnames(Insurance_factor_identification)
rownames(Insurance_factor_identification)

#To observe if there is missing observation in DataSet
sum(is.na(Insurance_factor_identification))

#as we see we observe that there is no missing values in the dataset

#Visualisation of the DataSet
## Visualizing the data
ggplot(data=Insurance_factor_identification, aes(x=Claims, y=Insured)) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE)

## Visualizing the data

plot(Insurance_factor_identification$Payment,Insurance_factor_identification$Insured)
plot(Insurance_factor_identification$Payment,Insurance_factor_identification$Claims)


#We found the correlation Coefficient between Insured and Claims and we found out there is high amount of correlation coefficient 
# & there is very strong relationship between Payment v/s Insured,& Payment v/sClaims
cor(Insurance_factor_identification$Insured,Insurance_factor_identification$Claims)
cor(Insurance_factor_identification$Payment,Insurance_factor_identification$Claims)
cor(Insurance_factor_identification$Payment,Insurance_factor_identification$Insured)


boxplot(Insurance_factor_identification$Insured)
boxplot(Insurance_factor_identification$Claims)
plot(x=Insurance_factor_identification$Insured, y=Insurance_factor_identification$Claims, xlab='Insured',ylab='Claims')
abline(lm(Insurance_factor_identification$Insured ~ Insurance_factor_identification$Claims))


#Linear Regression Model
model=lm(Insurance_factor_identification$Payment~Insurance_factor_identification$Insured+Insurance_factor_identification$Claims)
summary(model)
model$coefficients
model$residuals
model$fitted.values
model$df.residual

# After Creating the Linear Regression Model we found out that the value of R Squared is 0.9951 which is very high ,
# And Insured,Claims are having the p value which is less than 5% and shows that there is siginificant among the payment

model1=lm(Insurance_factor_identification$Payment~Insurance_factor_identification$Insured+Insurance_factor_identification$Claims+Insurance_factor_identification$Bonus+Insurance_factor_identification$Zone+Insurance_factor_identification$Make+Insurance_factor_identification$Kilometres)
model1
summary(model1)

# We can observe that except 2 variables bonus and make ,the variables Kilometers,Insured,Claims are strongly affecting the model 

#Q4 The insurance company is planning to establish a new branch office, so they are interested to find at what location, kilometre, and bonus level their insured amount, claims, and payment gets increased

grupzone<-apply(Insurance_factor_identification[,c(5,6,7)], 2, function(x) tapply(x,Insurance_factor_identification$Zone, mean)) 
grupzone

# We have observed that Zone 4 has the highest number of claims,& the no of payment as well. 
# Top 4 Zones (1-4) have more insured years, claims, and payments as compared to the last third  
grupiycp<-apply(Insurance_factor_identification[,c(5,6,7)],2,function(x)tapply(x,Insurance_factor_identification$Kilometres,mean))
grupiycp


# Kilometer group 2 has the maximum payments. Though the insured number of years is lesser than kilometre 1, the claims and payments are higher for group 2
grupyckp<-apply(Insurance_factor_identification[,c(5,6,7)],2,function(x)tapply(x,Insurance_factor_identification$Bonus,mean))
grupyckp



# Q5 The committee wants to understand what affects their claim rates so as to decide the right
#premiums for a certain set of situations. Hence, they need to find whether the insured 
#amount, zone, kilometer, bonus, or make affects the claim rates and to what extent. 
slrn<-lm(Claims~Kilometres+Zone+Bonus+Make+Insured,data=Insurance_factor_identification)
summary(slrn)
slrn

#The conclusion of this shows that the p value of Insured,Make,Bonus,Zone,As well as Kilometers

