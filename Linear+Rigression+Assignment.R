

# load the car company data
car_data <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)

#check categorical varible 
summary(as.factor(car_data$doornumber))

# let's see the structure of data
str(car_data)

#Exploratory data analysis
#carname has no significance with price
car_data <- car_data[ ,-3]

#to get some insight about the data
plot(car_data$price,car_data$horsepower)
plot(car_data$price,car_data$highwaympg)

#Dummy variable creation
#converting categorical variables in to numerical variables.
dummy_2 <- data.frame(model.matrix(price ~ fueltype + aspiration + doornumber + enginelocation, data=car_data))

# store numerical variables in the same variable 

dummy_2 <- dummy_2[ ,-1]
car_data <-cbind(car_data[ ,c(-3,-4,-5,-8)],dummy_2)

##categorical variable with more than 2 levels
summary(as.factor(car_data$enginetype))
summary(as.factor(car_data$carbody))

#converting categorical variables in to numerical variables.
dummy_3 <- data.frame(model.matrix(price ~ enginetype + carbody + drivewheel + cylindernumber + fuelsystem, data = car_data))
dummy_3 <- dummy_3[ ,-1]
car_data <-cbind(car_data[ ,c(-3,-4,-10,-11,-13)],dummy_3)

#divide data set into train data and test data
# set seed to 200
set.seed(200)
# randomly generate row indices for train dataset
trainindices <- sample(1:nrow(car_data), 0.7*nrow(car_data))
# generate the train data set
train <- car_data[trainindices, ]
#Similarly store the rest of the observations into an object "test".
test <- car_data[-trainindices, ]

#next we will build the model with all parameters.

model_1 <- lm(price ~. , data = train)
summary(model_1)

#we need to remove cylindernumbertwo,fuelsystemidi, as they are insignificant which can seen in model_1 
train <-train[ ,c(-38,-41)]
# create a next model by removing the insignificant parameters
model_2 <- lm(price ~. ,data = train)
summary(model_2)


# we will remove all variable which has p-value >0.05 and high vif 

#for VIF function, we need install 'car' package.

install.packages("car")
library(car)

vif(model_2)


#as there are so many variables we use stepAIC technique to remove variables

install.packages("MASS")
library(MASS)

step <- stepAIC(model_2, direction = "both")
step
#after the stepAIC function, we iterated some variables, remaining variable will reduce by using VIF and p-value
model_3 <- lm(formula = price ~ car_ID + wheelbase + carwidth + carheight + 
                curbweight + enginesize + stroke + peakrpm + highwaympg + 
                aspirationturbo + enginelocationrear + enginetypedohcv + 
                enginetypeohc + enginetypeohcv + enginetyperotor + carbodyhatchback + 
                carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + cylindernumbertwelve + 
                fuelsystemmpfi, data = train)
summary(model_3)

vif(model_3)
#eniginerotor,carbodyhatchback,wheelbase  these variables has p-value >0.05 and high VIF
#we will build next model by removing the these variables

model_4 <-lm(formula = price ~ car_ID + carwidth + carheight + 
               curbweight + enginesize + stroke + peakrpm + highwaympg + 
               aspirationturbo + enginelocationrear + enginetypedohcv + 
               enginetypeohc + enginetypeohcv + carbodyhatchback + 
               carbodywagon + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + cylindernumbertwelve + 
               fuelsystemmpfi, data = train)
summary(model_4)

vif(model_4)
#cylindernumberthree,carbodyhatchback,carheight  these variables has p-value >0.05
#we will build next model by removing the these variables


model_5 <- lm(formula = price ~ car_ID + carwidth +
                curbweight + enginesize + stroke + peakrpm + highwaympg + 
                aspirationturbo + enginelocationrear + enginetypedohcv + 
                enginetypeohc + enginetypeohcv + carbodywagon + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumbertwelve + 
                fuelsystemmpfi, data = train)

summary(model_5)

vif(model_5)


#carbodywagon has high p-value and high VIF Values.
#we will build next model by removing the these variables

model_6 <- lm(formula = price ~ car_ID + carwidth +
                curbweight + enginesize + stroke + peakrpm + highwaympg + 
                aspirationturbo + enginelocationrear + enginetypedohcv + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumbertwelve + 
                fuelsystemmpfi, data = train)

summary(model_6)
vif(model_6)

#highwaympg,fuelsystemmpfi has high p-value and high VIF Values.
#we will build next model by removing the these variables

model_7 <- lm(formula = price ~ car_ID + carwidth + enginesize + stroke + peakrpm + 
                aspirationturbo + enginelocationrear + enginetypedohcv + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumbertwelve , data = train)
summary(model_7)
vif(model_7)

#car_ID,enginetypedohcv has high p-value and high VIF Values.
#we will build next model by removing the these variables

model_8<- lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
     aspirationturbo + enginelocationrear + 
     enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
     cylindernumbersix + cylindernumbertwelve , data = train)

summary(model_8)
vif(model_8)
#enginesize,cylindernumberfour,cylindernumbersix,carwidth has VIF>2 , so we will make a model by removing these varibles

model_9<- lm(formula = price ~ stroke + peakrpm + 
               aspirationturbo + enginelocationrear + 
               enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumbertwelve , data = train)
summary(model_9)
vif(model_9)

# if we see the R-squared:  0.4004, and Adjusted R-squared:  0.3646 is suddenly drop into very low value.
# low r-squared is not a good model, so we should select model_8 as stable model to prediction.
#model_8 has R-squared:  0.9256 and Adjusted R-squared:  0.9188, seems like its a pretty good model.
# predicting the results in test dataset


Predict_1 <- predict(model_8,test[,-1])
test$test_price <- Predict_1


# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#from test data we got rsquared:0.8557629

#there not much difference in train and test data. so we can easily predict the usa car market price using this model.




###########
model_9 <- lm(formula = price ~ stroke + peakrpm + 
                aspirationturbo + enginelocationrear +
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumbertwelve , data = train)
summary(model_9)
vif(model_9)

# Look at summary of the model again to see the P values
summary(car_data)