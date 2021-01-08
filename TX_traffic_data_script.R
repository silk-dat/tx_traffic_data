#downloading and loading necessary packages

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!require(dslabs)) install.packages('dslabs')
library(dslabs)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(stringr)) install.packages('stringr')
library(stringr)
if (!require(knitr)) install.packages('knitr')
library(knitr)
if (!require(readr)) install.packages('readr')
library(readr)
if (!require(grDevices)) install.packages('grDevices')
library(grDevices)
if (!require(stats)) install.packages('stats')
library(stats)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(corrplot)) install.packages('corrplot')
library(corrplot)
if (!require(matrixStats)) install.packages('matrixStats')
library(matrixStats)
if (!require(ModelMetrics)) install.packages('ModelMetrics')
library(ModelMetrics)
if (!require(xgboost)) install.packages('xgboost')
library(xgboost)

#filtered to only include accidents in TX

#download required packages
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")

#download zip file from author's github repo and unzip file
temp <- tempfile()
download.file("https://github.com/silk-dat/us_traffic_data/raw/main/TX_car_accidents.csv.zip",temp)
my_data <- read_csv(unz(temp, "TX_car_accidents.csv"))
unlink(temp)
remove(temp)

data <- my_data

my_data <- data


#change from tibble to data frame
my_data <- as.data.frame(my_data)

#show and change column names
colnames(my_data)

names(my_data)[names(my_data) == "Temperature(F)"] <- "Temperature"
names(my_data)[names(my_data) == "Humidity(%)"] <- "Humidity"
names(my_data)[names(my_data) == "Pressure(in)"] <- "Pressure"
names(my_data)[names(my_data) == "Visibility(mi)"] <- "Visibility"


#select relevant columns
my_data <- my_data %>% select(Start_Time, Start_Lat, Start_Lng, Zipcode, Temperature,
                              Pressure, Visibility,
                              Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, 
                              Roundabout, Stop, Severity, Traffic_Signal)

#summary
summary(my_data)

#remove missing values; explain that we removed 2,6% of the data
nrow(my_data)
my_data <- na.omit(my_data)
nrow(my_data)

#clean zip code column

my_data$Zipcode <- gsub(my_data$Zipcode, pattern="-.*", replacement = "")
all(str_length(my_data$Zipcode) == 5)
my_data$Zipcode <- as.numeric(my_data$Zipcode)


#change Start_Time column into months, weeks, days and hours

my_data <- my_data %>% mutate(Month = month(as.Date(Start_Time)))
my_data <- my_data %>% mutate(Week = week(as.Date(Start_Time)))
my_data <- my_data %>% mutate(Day = day(as.Date(Start_Time)))
my_data <- my_data %>% mutate(Hour = hour(my_data$Start_Time))

my_data <- my_data[,-1]


#summary of the data
summary(my_data)

#remove rows with too high temperatures 
#check the unique values 
unique(my_data$Temperature)

#how many rows will need to be removed.
my_data %>% filter(Temperature > 112) %>% nrow()

#remove the rows
r <- with(my_data, which(Temperature > 112, arr.ind = TRUE))
my_data <- my_data[-r,]



#remove rows with too low temperatures
#how many rows will be removed
my_data %>% filter(Temperature == -40) %>% nrow()

#remove the rows
r <- with(my_data, which(Temperature == -40, arr.ind = TRUE))
my_data <- my_data[-r,]

#clean Pressure, everything below 27
unique(my_data$Pressure)
#how many rows will be removed
my_data %>% filter(Pressure < 27) %>% nrow()
#remove the rows
r <- with(my_data, which(Pressure < 27, arr.ind = TRUE))
my_data <- my_data[-r,]

#clean visibility.
summary(my_data$Visibility)
#show unique readings
unique(my_data$Visibility)
#how many rows will be removed
my_data %>% filter(Visibility > 13) %>% nrow()
#remove rows
r <- with(my_data, which(Visibility > 13, arr.ind = TRUE))
my_data <- my_data[-r,]

#move the label as firts column
my_data <- my_data %>% relocate(Severity)


#overview
head(my_data)
nrow(my_data)
ncol(my_data)

#severity disribution
my_data %>% group_by(Severity) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Severity, Count)) +
  geom_bar(stat = "identity") +
  ggtitle("Severity") 


#Start_Lat, Start_lng relation to Severity

my_data %>% 
  ggplot(aes(Start_Lat, Start_Lng, color = Severity)) +
  geom_point(size = 1) +
  facet_wrap(~ Severity, nrow = 2)

#Zipcode boxplot
my_data %>% 
  ggplot(aes(Severity, Zipcode)) +
  geom_boxplot(aes(fill = factor(Severity, levels = c(1, 2, 3, 4)))) +
  labs(fill = "Severity")

#cor matrix weather 
#select wather variables
weather <- my_data %>% select(Temperature, Visibility, Severity)
#create correlation matrix
weather_cor <- cor(weather)
#display matrix
corrplot(abs(cor(weather_cor)), method="color", tl.pos="lt", cl.lim = c(0,1))

#cor matrix date and time
#select variables
datetime <- my_data %>% select(Month, Week, Day, Hour, Severity)
#create correlation matrix
datetime_cor <- cor(datetime)
#display matrix
corrplot(abs(cor(datetime_cor)), method="color", tl.pos="lt", cl.lim = c(0,1))

#cor matrix road signs
#select variables
road <- my_data %>% select(Amenity, Bump, Crossing, Give_Way, Junction,
                           No_Exit, Railway, Roundabout, Stop, Traffic_Signal, Severity)
#create correlation matrix
road_cor <- cor(road)
#display matrix
corrplot(abs(cor(road_cor)), method="color", tl.pos="lt", cl.lim = c(0,1))


#reproducible data
set.seed(1, sample.kind="Rounding")

#split the data
test_index <- createDataPartition(factor(my_data$Severity, levels = c(1, 2, 3, 4)), times = 1, p = 0.7, list = FALSE)
train_set <- my_data[test_index, ]
test_set <- my_data[-test_index, ]

#separate features and label for preprocessing
x_train <- train_set[,-1]
y_train <- factor(train_set[,1])

x_test <- test_set[,-1]
y_test <- factor(test_set[,1])

#preprocessing

#create a matrix of the data
x <- as.matrix(x_train)

#calculates the standard deviation for each column
sds <- colSds(x)

#calculate the variables, which have zero or near-zero variability
nzv <- nearZeroVar(x)

#show the column names of zero or near-zero variablity
colnames(as.data.frame(x)[nzv])

#show the column names of the columns we will work with
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
colnames(as.data.frame(x)[col_index])

#change x_train and x_test to inlcude only useful variables

x_train <- x_train[col_index]
x_test <- x_test[col_index]


#train with qda model
fit_qda <- train(x_train, y_train, method = "qda")
#predict with  model
y_hat <- predict(fit_qda, x_test)
#show accuracy
qda_rmse <- rmse(y_test, y_hat)
qda_rmse 
#create a table for storing results
rmse_results <- tibble(method = "QDA", RMSE = qda_rmse)


#train with decision tree model and tune hyperparameters; 
fit_rpart <- train(x_train, as.numeric(y_train),
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)))
#plot rmse on each parameter; best CP
plot(fit_rpart)

#predict with  model
y_hat <- predict(fit_rpart, x_test)
#show accuracy
rpart_rmse <- rmse(y_test, y_hat)
rpart_rmse 
#add result to the table
rmse_results <- add_row(rmse_results, method = "RPART", RMSE = rpart_rmse)

#Stochastic gradient boosting, xgbTree; http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/139-gradient-boosting-essentials-in-r-using-xgboost/
#with cross-validation
set.seed(123, sample.kind = "Rounding")

xbg_fit <- train(x_test, as.numeric(y_test),
                 method = "xgbTree",
                 trControl = trainControl("cv", number = 10))
#show best tune
xbg_fit$bestTune
#predict with  model
y_hat <- predict(xbg_fit, x_test)
#show result
xbg_rmse <- rmse(y_test, y_hat)
xbg_rmse 
#add result to the table
rmse_results <- add_row(rmse_results, method = "XBG", RMSE = xbg_rmse)

#Stochastic gradient boosting, gblinear; https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
#with cross-validation

set.seed(123, sample.kind = "Rounding")
# specify cross-validation method and number of folds. Also enable parallel computation
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xbg_l_fit <- train(x_test, as.numeric(y_test),
                   method = "xgbLinear",
                   trControl = xgb_trcontrol)

y_hat <- predict(xbg_l_fit, x_test)

xbg_l_rmse <- rmse(y_test, y_hat)
xbg_l_rmse 

rmse_results <- add_row(rmse_results, method = "XBG_L", RMSE = xbg_l_rmse)

rmse_results