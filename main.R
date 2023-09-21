#binary LOGISTIC REGRESSION MODEL IN R   ###########


#SAMPLE DATA
mydata <- bank
View(mydata)


## lessen the categories for Job & Month
unique(mydata$job)
unique(mydata$month)

df<-as.data.frame(mydata)

df$month[df$month=='jan'] <- 'First6months'
df$month[df$month=='feb'] <- 'First6months'
df$month[df$month=='mar'] <- 'First6months'
df$month[df$month=='apr'] <- 'First6months'
df$month[df$month=='may'] <- 'First6months'
df$month[df$month=='jun'] <- 'First6months'
df$month[df$month=='jul'] <- 'Last6months'
df$month[df$month=='aug'] <- 'Last6months'
df$month[df$month=='sep'] <- 'Last6months'
df$month[df$month=='oct'] <- 'Last6months'
df$month[df$month=='nov'] <- 'Last6months'
df$month[df$month=='dec'] <- 'Last6months'


df$job[df$job =='services'] <- 'job'
df$job[df$job =='admin'] <- 'job'
df$job[df$job =='technician'] <- 'job'
df$job[df$job =='management'] <- 'job'
df$job[df$job =='student'] <- 'Unemployed'
df$job[df$job =='retired'] <- 'Unemployed'
df$job[df$job =='unemployed'] <- 'Unemployed'
df$job[df$job =='entrepreneur'] <- 'business'
df$job[df$job =='self-employed'] <- 'business'
df$job[df$job =='housemaid'] <- 'low income group'
df$job[df$job =='blue-collar'] <- 'low income group'


df$contact[df$contact=='unknown'] <- 'NA'
df$poutcome[df$poutcome=='unknown'] <- 'NA'
df$education[df$education=='unknown'] <- 'NA'


df[df=="NA"] <- NA  ####remove all NAs


df <- na.omit(df)  #removes missing variables, if any
View(df)



str(df) #check
###STEP1: convert to factors
df$y<-as.factor(df$y)



###step2: conduct hypothesis

table1<-table(df$y, df$job)
table1
prop.table(table1, 1)
prop.table(table1, 2)    
chisq.test(table1)

table2<-table(df$y, df$marital)
table2
prop.table(table2, 1)
prop.table(table2, 2)    
chisq.test(table2)

table3<-table(df$y, df$education)
table3
prop.table(table3, 1)
prop.table(table3, 2)    
chisq.test(table3)


table4<-table(df$y, df$default)
table4
prop.table(table4, 1)
prop.table(table4, 2)    
chisq.test(table4)

table5<-table(df$y, df$housing)
table5
prop.table(table5, 1)
prop.table(table5, 2)    
chisq.test(table5)

table6<-table(df$y, df$loan)
table6
prop.table(table6, 1)
prop.table(table6, 2)   
chisq.test(table6)



table7<-table(df$y, df$contact)
table7
prop.table(table7, 1)
prop.table(table7, 2)    
chisq.test(table7)


table8<-table(df$y, df$month)
table8
prop.table(table8, 1)
prop.table(table8, 2)   
chisq.test(table8)

table9<-table(df$y, df$poutcome)
table9
prop.table(table9, 1)
prop.table(table9, 2)    
chisq.test(table9)





View(df)


t.test(age~y, var.equal=T, data=df)
t.test(balance~y, var.equal=T, data=df)
t.test(day~y, var.equal=T, data=df)
t.test(duration~y, var.equal=T, data=df)
t.test(campaign~y, var.equal=T, data=df)
t.test(pdays~y, var.equal=T, data=df)
t.test(previous~y, var.equal=T, data=df)





##default, contact, marital, education xx (chi sq test)
##day, campaign, previous,balance  (t test)



##check for outliers
###no outliers are present.....
install.packages("ggplot2")
install.packages("olsrr")
install.packages("plotly")

library(ggplot2)
library(plotly)

r<-ggplot(df, aes(x= age, y=y, color=age))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= job, y=y, color=job))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= marital, y=y, color=marital))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= education, y=y, color=education))+
  geom_boxplot()
ggplotly(r)


r<-ggplot(df, aes(x= default, y=y, color=default))+
  geom_boxplot()
ggplotly(r)


r<-ggplot(df, aes(x= balance, y=y, color=balance))+
  geom_boxplot()
ggplotly(r)


r<-ggplot(df, aes(x= housing, y=y, color=housing))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= loan, y=y, color=loan))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= contact, y=y, color=contact))+
  geom_boxplot()
ggplotly(r)


r<-ggplot(df, aes(x= day, y=y, color=day))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= months, y=y, color=months))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= duration, y=y, color=duration))+
  geom_boxplot()
ggplotly(r)

r<-ggplot(df, aes(x= campaign, y=y, color=campaign))+
  geom_boxplot()
ggplotly(r)


####since dependent variable is binary, it has no outliers....





#step3: Develop full model
model<-glm(y~.,family = binomial(), df)
summary(model)


#common
#day, previous, campaign, contact, balance, def, edu, marital


#step4: check multicollinearity
library(performance)
check_collinearity(model)

#vif < 5 ok




#pseudo r square
install.packages("rcompanion")
library(rcompanion)
nagelkerke(final_model)



#Step5: stepwise model development


install.packages("MASS")
library(MASS)
final_model <- stepAIC(model)
summary(final_model)


model<-glm(y ~ age + job + housing + 
                   loan  + month + duration + pdays + poutcome,family = binomial(), df)
summary(model)


final_model <- stepAIC(model)
summary(final_model)




#step6: evaluate model performance
View(df)
pred<- predict(final_model, df, type = "response")
x1<-data.frame(pred)
View(x1)


library(dplyr)
new_data<-bind_cols(df,x1)  #merge
View(new_data)

new_data<-mutate(new_data, pred=ifelse(pred>=0.5,"yes","no"))  #ifelse
table(new_data$y, new_data$pred)

#Accuracy
(554+83)/(554+40+91+83)
###83% accuracy

#misclassification rate
(40+91)/(554+40+91+83)
###17%




#step7: ODD RATIO
coef(final_model)
exp(coef(final_model))




#log (p/(1-p)) = 0.072+ 0.44 * housing + 1.99 * month + 1.003 * duration + 1.47 * potcomeother + 8.827 * poutcomesuccess
#Where p = probability of customer subscription, housing = has housing loan? (categorical: 'no','yes'), month = contacted in first 6 months of year or last six months of the year (categorical: 'Firstsixmonths’ ‘Lastsixmonths’), duration = last contact duration, in seconds (numeric), poutcome = outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success').
