#Analyzing cereal dataset

rm(list = ls())

cereal_data <- read.csv(file.choose(), header=T)

install.packages("tidyverse")
library("tidyverse")

dim(cereal_data)
head(cereal_data)
#tail(cereal_data)

#Using Histogram to examine our variables of interest
ggplot(data=cereal_data, aes(x=calories)) + geom_histogram()

ggplot(data=cereal_data, aes(x=protein)) + geom_histogram() 

ggplot(data=cereal_data, aes(x=fat)) + geom_histogram() #Skewed to the right(positively skewed)

ggplot(data=cereal_data, aes(x=sodium)) + geom_histogram() #Normal distribution

ggplot(data=cereal_data, aes(x=fiber)) + geom_histogram() #Skewed to the right 

ggplot(data=cereal_data, aes(x=carbo)) +  geom_histogram()

ggplot(data=cereal_data, aes(x=sugars)) +  geom_histogram()

ggplot(data=cereal_data, aes(x=potass)) + geom_histogram() 

ggplot(data=cereal_data, aes(x=vitamins)) + geom_histogram()

ggplot(data=cereal_data, aes(x=weight)) + geom_histogram()

ggplot(data=cereal_data, aes(x=rating)) + geom_histogram()

#First remove variables not needed
cereal_data1<-cereal_data[  , -c(1:3,13,15)]
head(cereal_data1)

#log transformation for fat and fiber because they were skewed to the right  
cereal_data1$fat_log=log(cereal_data1$fat+1)
cereal_data1$fiber_log=log(cereal_data1$fiber+1)

#histogram plot for the transformed variables
ggplot(data=cereal_data1, aes(x=fat_log)) + geom_histogram()
ggplot(data=cereal_data1, aes(x=fiber_log)) + geom_histogram()

#Remove fat and fiber and replace with its log columns
cereal_data1<-cereal_data1[  , -c(3,5)]

which(is.na(cereal_data1)) #checking for missing values

pairs(cereal_data1) 

#Finding variables that have a significant relationship to response variable (rating) 
ggplot(data= cereal_data1, aes(y=rating, x=calories)) + geom_point()+geom_smooth(method=lm) #negative correlation

ggplot(data= cereal_data1, aes(y=rating, x=protein)) + geom_point()+geom_smooth(method=lm) #positive correlation

ggplot(data= cereal_data1, aes(y=rating, x=fat_log,protein)) + geom_point()+geom_smooth(method=lm)

ggplot(data= cereal_data1, aes(y=rating, x=sodium)) +  geom_point()+geom_smooth(method=lm)

ggplot(data= cereal_data1, aes(y=rating, x=fiber_log)) + geom_point()+geom_smooth(method=lm)

ggplot(data= cereal_data1, aes(y=rating, x=carbo)) +  geom_point()+geom_smooth(method=lm)

ggplot(data= cereal_data1, aes(y=rating, x=sugars)) +  geom_point()+geom_smooth(method=lm) #negative correlation

ggplot(data= cereal_data1, aes(y=rating, x=potass)) + geom_point()+geom_smooth(method=lm)

ggplot(data= cereal_data1, aes(y=rating, x=vitamins)) +  geom_point()+geom_smooth(method=lm)

#-- Building a predictive model for nutritional rating 

cereal_data2 <- cereal_data[,-c(1:3)]
head(cereal_data2) #removing columns we don't need

#Divide to test 20% of data and training 80% of data
set.seed(10)
test <- sample(x=nrow(cereal_data2), size = 0.20*nrow(cereal_data2))
testing <- cereal_data2[test, ]
training <- cereal_data2[-test, ]
ytrain = training$rating
ytest = testing$rating

#Fit a linear model
fit_training <- lm(rating~., data = training) 
names(fit_training)
summary(fit_training)
fit <- predict(fit_training,newdata = testing)

#Mean square error
MSE = mean((testing$rating - fit)^2)
