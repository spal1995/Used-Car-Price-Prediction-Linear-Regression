library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(car)
library(stringr)
library(psych)
library(rpart)
library(rpart.plot)
library(ipred)
library(caret)
library(GGally)
library(RColorBrewer)
library(corrgram)
library(corrplot)
library(caTools)

# Reading the csv file and storing in the dataframe
car_df <- read.csv(file="/Users/shubhrajitpal/Desktop/Business Intelligence/Project/CAR DETAILS FROM CAR DEKHO.csv")

# checking the records in the dataframe
head(car_df)

# checking the column names
colnames(car_df)




#################################################



# DATA PRE-PROCESSING

# Dividing the data present in the 'name' column into 'name' and 'model' using the extract function from library(dplyr)
car_used_df <- extract(car_df,name, into = c("name","model"),regex ="([^ ]+) (.*)" )
head(car_used_df)
View(car_used_df)

# correcting 3 car names in the 'name' column : Land, Daewoo, and OpelCorsa to Land Rover, Daewood, Opel Corsa
car_used_df[car_used_df[,"name"]=="Land","name"] <- "Land Rover"
car_used_df[car_used_df[,"name"]=="Daewoo","name"] <- "Daewood"
car_used_df[car_used_df[,"name"]=="OpelCorsa","name"] <- "Opel Corsa"

# checking the total null values in each columns of the dataset
colSums(is.na(car_used_df)) # no NULL values present

# checking duplicate rows
num_duplicates <- sum(duplicated(car_used_df))
print(num_duplicates)

# removing the duplicate rows
car_used_df_unique <- unique(car_used_df)
View(car_used_df_unique)

#copy of the original df : car_used_df_unique
car_used_df_unique_check <- unique(car_used_df)
head(car_used_df_unique_check)




#################################################



# DATA DISTRIBUTION : Plotting in Graph (ggplot, histograms, boxplots)

# Frequency of cars with respect to the year it was first bought
Buy_year_freq <- car_used_df_unique %>%
  group_by(year) %>%
  summarize(freq_car = n(), .groups = 'drop') %>%
  as.data.frame()

head(Buy_year_freq)

# Plotting the Frequency of the car first bought with respect to the year
ggplot(data = Buy_year_freq, aes(x = year, y = freq_car)) +
  geom_line(color = "#ABB2B9", size = 2) + 
  geom_point(size = 4, color = "#0097A7") +
  geom_label(aes(label = paste("(", year, ", ", round(freq_car, 1), ")", sep = "")), 
             vjust = -0.5, size = 2) +
  labs(x = "Year Bought", y = "Number of cars (Frequency)", title = "Frequency of Car Vs Year Bought") +
  theme(plot.title = element_text(color = "black", size = 15, face = "bold", lineheight = 0.8),
        axis.text.x = element_text())

# Plotting of Car Seller Types vs the years the cars were first bought
ggplot(car_used_df_unique, aes(x=seller_type, y=year, fill=seller_type)) + 
  geom_boxplot()+
  labs(x="Seller Type",y="Year Bought",fill = "Seller Type",
       title="Boxplot of Car Seller Types")+
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8),
        axis.text.x = element_text())+
  scale_fill_brewer(palette = "PRGn")

#CHECKING FOR FINAL PROJECT PDF
# Plotting of Car Seller Types vs the years the cars were first bought
ggplot(car_used_df_unique_check, aes(x=seller_type, y=year, fill=seller_type)) + 
  geom_boxplot()+
  labs(x="Seller Type",y="Year Bought",fill = "Seller Type",
       title="Boxplot of Car Seller Types")+
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8),
        axis.text.x = element_text())+
  scale_fill_brewer(palette = "PRGn")
######### CHECKING END

# Frequency of the car first bought with respect to the Car Brand

Car_Brand_Freq <- car_used_df_unique %>%
  group_by(name) %>%
  summarise(brand_freq = n()) %>%
  as.data.frame()

head(Car_Brand_Freq)

#Plotting the Frequency of the car first bought with respect to the Car Brand
ggplot(data=Car_Brand_Freq, aes(x=name, y=brand_freq,fill=name))+
  geom_bar(stat="identity")+
  labs(x="Car Brand",y="Number of Cars(Frequency)", fill = "Car Brands",
       title=" Bar Plot of Car Brand Count.")+
  geom_text(aes(label = round(brand_freq)), vjust = -0.5, color = "black", size = 2) +
  theme(plot.title =element_text(color="black",
                                 size=12,face="bold",lineheight = 0.8))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

# Frequency of the car first bought with respect to the Car Brand
Fuel_Type_Freq <- car_used_df_unique %>%
  group_by(fuel) %>%
  summarise(fuel_type_count = n()) %>%
  as.data.frame()

head(Fuel_Type_Freq)


#Plotting the Frequency of the car first bought with respect to the Fuel Type
ggplot(data=Fuel_Type_Freq, aes(x=fuel, y=fuel_type_count))+
  geom_bar(stat="identity",fill="#0097A7")+
  geom_text(aes(label = round(fuel_type_count)), vjust = -0.5, color = "black", size = 4) +
  theme_minimal()+
  labs(x="Fuel Type",y="Number of Cars",
       title="Bar Plot of The Type of Fuels utilized by Used Cars")+
  theme(plot.title =element_text(color="black",
                                 size=12,face="bold",               
                                 lineheight = 0.8))


# Plotting the distribution of the car Prices according to their makes
ggplot(car_used_df_unique, aes(x = name, y = selling_price)) +
  geom_boxplot(fill = "#0097A7")+
  labs(
    title = "Box plot of Car Brand prices",
    x = "Car Brand",
    y = "Selling Price")+
  coord_flip()

# #CHECKING FOR FINAL PROJECT PDF
ggplot(car_used_df_unique_check, aes(x = name, y = selling_price)) +
  geom_boxplot(fill = "#0097A7")+
  labs(
    title = "Box plot of Car Brand prices",
    x = "Car Brand",
    y = "Selling Price")+
  coord_flip()
######### CHECKING END

#Frequency of the Mean Price with respect to the Car Brand
options(scipen=999)
mean_car_prices <- car_used_df_unique %>%
  group_by(name) %>%
  summarize(mean_car_price =mean(selling_price)) %>%
  as.data.frame()

head(mean_car_prices)

# Plotting the Mean Price with respect to the Car Brand
ggplot(data=mean_car_prices, aes(x=name, y=mean_car_price)) +
  geom_col(fill="#0097A7") +
  geom_text(aes(label = round(mean_car_price,2),angle = 90),hjust = 0.3, vjust = 0, color = "black", size = 1.5) +
  labs(x="Car Brand",y="Mean Car prices",title=" Bar Plot of Car Mean prices")+
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8),axis.text.x = element_text())+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

#CHECKING FOR FINAL PROJECT PDF
options(scipen=999)
mean_car_prices <- car_used_df_unique_check %>%
  group_by(name) %>%
  summarize(mean_car_price =mean(selling_price)) %>%
  as.data.frame()

ggplot(data=mean_car_prices, aes(x=name, y=mean_car_price)) +
  geom_col(fill="#0097A7") +
  geom_text(aes(label = round(mean_car_price,2),angle = 90),hjust = 0.3, vjust = 0, color = "black", size = 1.5) +
  labs(x="Car Brand",y="Mean Car prices",title=" Bar Plot of Car Mean prices")+
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8),axis.text.x = element_text())+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
########## CHECKING END

# Frequency of cars mileage with a histogram plot
options(scipen=999)
ggplot(car_used_df_unique, aes(x=km_driven)) + 
  geom_histogram(color="grey", fill="#0097A7", bins = 160) +
  labs(x='Car Mileage ',y='Number of Cars(Frequency)',title = "Histogram Plot of Car Mileage Distribution")  +
  scale_x_continuous(trans='log10')



# Frequency of the type of used car sellers
Seller_Type_Count <- car_used_df_unique %>%
  group_by(seller_type) %>%
  summarise(seller_type_count = n()) %>%
  as.data.frame()

head(Seller_Type_Count)

# Plotting the type of used car sellers with respect to the Frequency
ggplot(data=Seller_Type_Count, aes(x=seller_type, y=seller_type_count))+
  geom_bar(stat="identity",fill="#0097A7")+
  theme_dark()+
  labs(x="Type of Car Sellers",y="Seller Type(Freq)",
       title="Bar Plot of the Type of Car Seller")+
  geom_text(aes(label = round(seller_type_count)), vjust = -0.5, color = "black", size = 3) +
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8))

# Frequency of Car Ownership
Number_of_owners <- car_used_df_unique %>%
  group_by(owner) %>%
  summarise(owner_count = n()) %>%
  as.data.frame()

head(Number_of_owners,5)

# Plotting the Car Ownership
ggplot(data=Number_of_owners, aes(x=owner, y=owner_count))+
  geom_bar(stat="identity",fill="#0097A7")+
  labs(x="Type of Car Ownership",y="Car Ownership(Freq)",
       title=" Bar Plot of the type of Car Ownership")+
  geom_text(aes(label = round(owner_count)), vjust = -0.5, color = "black", size = 3) +
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8),axis.text.x = element_text())+
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5))

# Plotting the Distribution of Price
ggplot(car_used_df_unique, aes(x=selling_price)) + 
  labs(x='Price of Used cars') +
  labs(title = "Histogram Graph of the Prices of used Cars") +
  geom_histogram(aes(y=..density..), colour="grey", fill="#0097A7")+
  geom_density() +
  scale_x_continuous(trans='log2')



#################################################



#CHECKING OUTLIER FOR NUMERICAL COLUMNS

# Boxplot for Numerical feature "selling_price" to check Outlier
ggplot(data = car_used_df_unique, aes(x = 1, y = selling_price)) +
  geom_boxplot() +
  labs(x = NULL, y = "Price", title = "Box Plot of Price")

# Boxplot for Numerical feature "km_driven" to check Outlier
ggplot(data = car_used_df_unique, aes(x = 1, y = km_driven)) +
  geom_boxplot() +
  labs(x = NULL, y = "Mileage", title = "Box Plot of Mileage")

# Boxplot for Numerical feature "year" to check Outlier
ggplot(data = car_used_df_unique, aes(x = 1, y = year)) +
  geom_boxplot() +
  labs(x = NULL, y = "year", title = "Box Plot of year")



#################################################



# DATA TRANSFORMATION : Encoding the categorical Columns, Dropping unnecessary Columns

#Label encoding for the Car brands
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Daewood', '0')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'OpelCorsa', '1')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Ambassador', '2')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Chevrolet', '3')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Fiat', '4')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Tata', '5')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Datsun', '6')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Maruti', '7')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Force', '8')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Renault', '9')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Hyundai', '10')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Nissan', '11')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Volkswagen', '12')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Skoda', '13')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Honda', '14')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Ford', '15')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Mahindra', '16')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Mitsubishi', '17')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Toyota', '18')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Kia', '19')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Isuzu', '20')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Jeep', '21')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'MG', '22')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Audi', '23')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Jaguar', '24')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Volvo', '25')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'mercedes-Benz','26')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'BMW', '27')
carmake_ordinal <- car_used_df_unique$name <- str_replace(car_used_df_unique$name, 'Land Rover', '28')
carmake_ordinal <- car_used_df_unique$name <- as.numeric(car_used_df_unique$name)
table(carmake_ordinal)

#Label encoding for the Seller Type
seller <- car_used_df_unique$seller_type <- str_replace(car_used_df_unique$seller_type,'Trustmark Dealer', "0")
seller <- car_used_df_unique$seller_type <- str_replace(car_used_df_unique$seller_type, 'Dealer', "1")
seller <- car_used_df_unique$seller_type <- str_replace(car_used_df_unique$seller_type, 'Individual', "2")
seller <- car_used_df_unique$seller_type <- as.numeric(car_used_df_unique$seller_type)
table(seller)

#Label encoding for the Fuel
fuel_usage <- car_used_df_unique$fuel <- str_replace(car_used_df_unique$fuel, 'Diesel', "0")
fuel_usage <- car_used_df_unique$fuel <- str_replace(car_used_df_unique$fuel, 'Petrol', "1")
fuel_usage <- car_used_df_unique$fuel <- str_replace(car_used_df_unique$fuel,'CNG',"2")
fuel_usage <- car_used_df_unique$fuel <- str_replace(car_used_df_unique$fuel, 'LPG', "3")
fuel_usage <- car_used_df_unique$fuel <- as.numeric(car_used_df_unique$fuel)
table(fuel_usage)

#Label encoding for Owner
car_ownership <- car_used_df_unique$owner <- str_replace(car_used_df_unique$owner, 'First Owner', "0")
car_ownership <- car_used_df_unique$owner <- str_replace(car_used_df_unique$owner, 'Second Owner', "1")
car_ownership <- car_used_df_unique$owner <- str_replace(car_used_df_unique$owner, 'Third Owner', "2")
car_ownership <- car_used_df_unique$owner <-str_replace(car_used_df_unique$owner,'Fourth & Above Owner', "3")
car_ownership <- car_used_df_unique$owner <- str_replace(car_used_df_unique$owner, 'Test Drive Car', "4")
car_ownership <- car_used_df_unique$owner <- as.numeric(car_used_df_unique$owner)
table(car_ownership)

#Label encoding for transmission
transmission_mode <- car_used_df_unique$transmission <- str_replace(car_used_df_unique$transmission, 'Manual', "0")
transmission_mode <- car_used_df_unique$transmission <- str_replace(car_used_df_unique$transmission, 'Automatic', "1")
transmission_mode <- car_used_df_unique$transmission <- as.numeric(car_used_df_unique$transmission)
table(transmission_mode)

# Dataset after Label encoding
car_used_df_transformed <- car_used_df_unique
head(car_used_df_transformed,5)

#Dropping the "model" Column/Feature
car_used_df_transformed <- subset(car_used_df_transformed, select = -model)
head(car_used_df_transformed)

# Dropping the NA which got entered due to label encoding
car_used_df_transformed <- car_used_df_transformed %>% drop_na()
dim(car_used_df_transformed)

# Summary Statistics of the transformed data set
psych::describe(car_used_df_transformed)
summary(car_used_df_transformed)


#################################################


# CORRELATION
#Exploring the Correlation of the independent variables with selling price(dependent variable)
correlationMatrixCar=cor(car_used_df_transformed)
corrplot(correlationMatrixCar,method="number",type="lower")
corrplot(correlationMatrixCar,type="lower")


#################################################


#SAMPLING OF TRAIN AND TEST DATA
# Sampling the data into Train and Test dataset to tain the model and predict
car_used_df_transformed1 <- car_used_df_transformed
sample=sample.split(car_used_df_transformed1$selling_price,SplitRatio = 2/3)
train_car = subset(car_used_df_transformed1,sample==TRUE)
test_car = subset(car_used_df_transformed1, sample==FALSE)
dim(train_car)
dim(test_car)

# Checking Null values in the test and train dataset
colSums(is.na(train_car))  # Zero Null values
colSums(is.na(test_car))   # Zero Null values




#################################################

#MODEL FITTING AND PREDICTION


############ ML MODEL 1 ##################


# Linear Regression Model to train the Training DataSet

set.seed(120) # to eliminate randomness

Simple_Linear_Regression_1 <- lm(selling_price ~ year + name + km_driven + fuel 
                               + seller_type + transmission + owner,data = train_car)

Simple_Linear_Regression_1
summary(Simple_Linear_Regression_1)

# Mean Square Error on training set (original train set value - predicted train set value)^2/n
set.seed(120)
SLR_train_predict <- predict(Simple_Linear_Regression_1,data = train_car)
SLR_MSE <- mean((train_car$selling_price - SLR_train_predict)^2)
print(paste("Mean Square Error of Train Set : ", SLR_MSE))

# Root Mean Square Error on training set
RMSE_train = sqrt(SLR_MSE)
print(paste("Root Mean Square Error of Train Set : ", RMSE_train))



# PREDICTION BY THE TRAINED SIMPLE LINEAR REGRESSION MODEL
set.seed(120)
test_car$Predicted_Selling_Price <- predict(Simple_Linear_Regression_1, test_car)

# Computing R Square value from testing data
actual_price <- test_car$selling_price
predicted_price <- test_car$Predicted_Selling_Price
rs_calc <- sum((predicted_price - actual_price) ^ 2)
ts_calc <- sum((actual_price - mean(actual_price)) ^ 2)
r_squared <- 1 - rs_calc/ts_calc
print(paste("R square value of the Predicted Model : ", r_squared))

# Computing RMSE from testing data
model_error <- actual_price - predicted_price
RMSE_model <- sqrt(mean(model_error^2))
print(paste("RMSE of the Predicted Model : ", RMSE_model))

#Simple Linear Regression : Plotting predicted Price vs. actual price Using the Predicted Model
par(mfrow=c(1,1))
plot(actual_price,predicted_price, main="Simple Linear Regression : Plot of Actual Selling Price vs Predicted Selling Price", 
     col = c("#0097A7","#7B1FA2"), 
     xlab = "Actual Car Selling Price", 
     ylab = "Predicted Car Selling Price")

# Four Graphs of the Simple Linear Regression
par(mfrow=c(2,2))
plot(Simple_Linear_Regression_1)
summary(Simple_Linear_Regression_1)






############ MODEL 2.1 ##################


# PREDICTION USING BASIC REGRESSION TREE
set.seed(120)
Regression_tree_model1 <- rpart(selling_price ~ year + name + km_driven + fuel 
                    + seller_type + transmission + owner,data = train_car, method = "anova")

summary(Regression_tree_model1)

rpart.plot(Regression_tree_model1)


# Computing R Square value from testing data of the regression tree(without cross validation)
actual_price0 <- test_car$selling_price
predicted_price0 <- test_car$Predicted_SP_Regression_no_val
rs_calc0 <- sum((predicted_price0 - actual_price0) ^ 2)
ts_calc0 <- sum((actual_price0 - mean(actual_price0)) ^ 2)
r_squared0 <- 1 - rs_calc0/ts_calc0
print(paste("R square value of the Predicted Model : ", r_squared0))


RMSE_Regression_tree_model1_RMSE <- RMSE(pred_test, test_car$selling_price)
print(paste("RMSE of the Predicted Regression Tree Model : ", RMSE_Regression_tree_model1_RMSE))

#Regression Tree : Plotting predicted Price vs. actual price Using the Predicted Model
par(mfrow=c(1,1))
plot(actual_price0,predicted_price0, main="Regression Tree : Plot of Actual Selling Price vs Predicted Selling Price", 
     col = c("#0097A7","#7B1FA2"), 
     xlab = "Actual Car Selling Price", 
     ylab = "Predicted Car Selling Price")





############ MODEL 2.2 ##################


# PREDICTION USING BOOTSTRAP AGGREGATION REGRESSION TREE

set.seed(120)
ctrl <- trainControl(method = "cv", number=10) # 10 cross validations

crossValidated_baggingModel <- train(
  selling_price ~ year + name + km_driven + fuel 
  + seller_type + transmission + owner,data = train_car,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE) #Rank the predictor variable importance in the model

summary(crossValidated_baggingModel)
crossValidated_baggingModel
par(mfrow=c(1,1))
plot(varImp(crossValidated_baggingModel)) #predictor importance of the seven variables.

test_car$Predicted_SP_Regression_val <- predict(crossValidated_baggingModel, test_car)

RMSE_Regression_tree_model2_RMSE <- RMSE(pred_test, test_car$Predicted_SP_Regression_val)
print(paste("RMSE of the Predicted Regression Tree Model : ", RMSE_Regression_tree_model2_RMSE))

# Computing R Square value from testing data of the cross validation regression tree
actual_price1 <- test_car$selling_price
predicted_price1 <- test_car$Predicted_SP_Regression_val
rs_calc1 <- sum((predicted_price1 - actual_price1) ^ 2)
ts_calc1 <- sum((actual_price1 - mean(actual_price1)) ^ 2)
r_squared1 <- 1 - rs_calc1/ts_calc1
print(paste("R square value of the Predicted Model : ", r_squared1))

#Bagging Regression Tree : Plotting predicted Price vs. actual price Using the Predicted Model
par(mfrow=c(1,1))
plot(actual_price1,predicted_price1, main="Bagging Regression Tree : Plot of Actual Selling Price vs Predicted Selling Price", 
     col = c("#0097A7","#7B1FA2"), 
     xlab = "Actual Car Selling Price", 
     ylab = "Predicted Car Selling Price")



#################################################

# COMPARING THE THREE METHODS BY R SQUARE VALUES

# Simple Linear Regression 
print(paste("R square value of the Predicted Model(Simple Linear Regression) : ", r_squared))


# Regression Tree without Cross Validation
print(paste("R square value of the Predicted Model(Regression Tree without Cross Validation) : ", r_squared0))

# Regression Tree with Cross Validation
print(paste("R square value of the Predicted Model(Regression Tree with Cross Validation) : ", r_squared1))

test_car_transformed <- subset(test_car, select = -c(name , year , km_driven , fuel , seller_type , transmission , owner))
head(test_car_transformed)

View(test_car_transformed)
