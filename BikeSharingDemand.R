train =read.csv("train.csv")
test = read.csv("test.csv")
test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)
str(data)
table(is.na(data))
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

prop.table(table(data$weather))
data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$hour=substr(data$datetime,12,13)
data$hour=as.factor(data$hour)


train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

boxplot(train$count~train$hour,xlab="hour", ylab="count of users")

boxplot(train$casual~train$hour,xlab="hour", ylab="count of users")
boxplot(train$registered~train$hour,xlab="hour", ylab="count of users")

boxplot(log(train$count)~train$hour,xlab="hour",ylab="log(count)")

date=substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day=days

boxplot(data$casual~data$day,xlab="hour", ylab="count of users")
boxplot(data$registered~data$day,xlab="hour", ylab="count of users")

boxplot(data$casual~data$weather,xlab="hour", ylab="count of users")
boxplot(data$registered~data$weather,xlab="hour", ylab="count of users")

#Continuous variable
sub=data.frame(train$registered,train$casual,train$count,train$temp,
train$humidity,train$atemp,train$windspeed)
cor(sub)

# Variable temp is positively correlated with dependent variables
# (casual is more compare to registered)
# Variable atemp is highly correlated with temp.
# Windspeed has lower correlation as compared to temp and humidity


data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year,xlab="year", ylab="count")

#feature engineering
train$hour=as.integer(train$hour) # convert hour to integer
test$hour=as.integer(test$hour) # modifying in both train and test data set

library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(RColorBrewer)
d=rpart(registered~hour,data=train)
fancyRpartPlot(d)

#looking at the tree e can create buckets for hour
data=rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>=22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==19 | data$hour==18]=7

# We had a hypothesis that bike demand will increase over time 
# and we have proved it also. Here I have created 8 bins (quarterly) 
# for two years. Jan-Mar 2011 as 1 ...Oct-Dec2012 as 8.

data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

#Created a variable having categories like "weekday", "weekend" and "holiday".
data$day_type=""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"
data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1


train$hour=as.factor(train$hour)
test$hour=as.factor(test$hour)
#As we know that dependent variables have natural outliers so we 
#will predict log of dependent variables.

#predicting the log of registered users.
set.seed(415)
fit1 <- randomForest(logreg ~ hour +workingday+day+holiday+ day_type 
+temp_reg+humidity+atemp+windspeed+season+weather
+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
test$logreg=pred1

#predicting the log of casual users.
set.seed(415)
fit2 <- randomForest(logcas ~hour + day_type+day+humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+dp_cas+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2


test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$count=test$casual+test$registered
s<-data.frame(datetime=test$datetime,count=test$count)
write.csv(s,file="submit.csv",row.names=FALSE)