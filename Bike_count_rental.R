#loading required packages
library(dplyr)
library(readr)
library(DMwR)
library(stringr)
library(ggplot2)
library(forcats)
library(lubridate)
#setwd("E:/Data-Science/R")
#loading data
bike_df <- read_csv("day.csv", na = c("NA", "", " "))
dim(bike_df)
#Glimplse into data 
glimpse(bike_df)
summary(bike_df)

#Missing value anaylsis
missing_val <- data.frame(apply(bike_df, 2, function(x){sum(is.na(x))}))
names(missing_val)[1] <- "Missing_val"
missing_val$attributes <- row.names(missing_val)
missing_val %>% filter(Missing_val > 0) # 0 rows returned
#Visualize missing values
ggplot(missing_val,aes(attributes, Missing_val)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10,1), limits = c(0,10))

 
#Assigning meaningfull names to some variables
bike_df <- bike_df %>%
  rename(date = dteday, year = yr, month = mnth, weather_condition = weathersit, 
                   real_temp = atemp, humidity = hum, total_count = cnt)

#Typecasting required variables
bike_df$date <- as.Date(parse_date_time(bike_df$date, "%m/%d/%Y"))
bike_df$year <-as.factor(bike_df$year)
bike_df$month <-as.factor(bike_df$month)
bike_df$season <- as.factor(bike_df$season)
bike_df$holiday <- as.factor(bike_df$holiday)
bike_df$weekday <- as.factor(bike_df$weekday)
bike_df$workingday <- as.factor(bike_df$workingday)
bike_df$weather_condition <- as.factor(bike_df$weather_condition)

#Yearwise distribution of count
ggplot(bike_df, aes(year, total_count, fill = year)) +
  geom_bar(stat = "identity") + labs(title = "Yearwise distribution of count")

#Yearwise distribution of count faceted by season
ggplot(bike_df, aes(year, total_count, fill = year)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~season) +
  labs(title = "Yearwise distribution of count faceted by season")

#Seasonwise proportion of count in a year
ggplot(bike_df, aes(year, total_count, fill = season)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Seasonwise proportion of count in a year")

#monthwise distribution of count
ggplot(bike_df, aes(month, total_count, fill = month)) +
  geom_bar(stat = "identity") + labs(title = "monthwise distribution of count")

#monthwise distribution of count faceted by season
ggplot(bike_df, aes(month, total_count, fill = month)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~season) +
  labs(title = "monthwise distribution of count faceted by season")

#Seasonwise proportion of count in a month
ggplot(bike_df, aes(month, total_count, fill = season)) +
  geom_bar(stat = "identity", position = "fill") + labs(title = "Seasonwise proportion of count in a month")
   
#Weekday distribution of count faceted by months
ggplot(bike_df, aes(weekday, total_count, fill = weekday)) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(~month) +
  labs(title = "Weekday distribution of count faceted by months")

#Holidaywise distribution of count
ggplot(bike_df, aes(holiday, total_count, fill = holiday)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(title = "monthwise distribution of count")
  
#Workingdaywise distribution of count
ggplot(bike_df, aes(workingday, total_count, fill = workingday)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(title = "Workingdaywise distribution of count")

#Weather_condition wise distribution of count
ggplot(bike_df, aes(weather_condition, total_count, fill = weather_condition)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(title = "Weather_condition wise distribution of count")

#Outlier anaylsis
bike_df <- select(bike_df, -c("instant"))
bike_df_numeric <- colnames(bike_df[,sapply(bike_df, is.numeric)])
bike_df_numeric
#temp boxplot graph, no outliers present
ggplot(bike_df, aes(y=temp)) +
  geom_boxplot() + ggtitle("temp")

#real_temp boxplot graph, no outliers present
ggplot(bike_df, aes(y=real_temp)) +
  geom_boxplot() + ggtitle("real_temp")

#humidity boxplot graph, outliers present
ggplot(bike_df, aes(y=humidity)) +
  geom_boxplot() + ggtitle("humidity")

#windspeed boxplot graph, outliers present
ggplot(bike_df, aes(y=windspeed)) +
  geom_boxplot() + ggtitle("windspeed")

#casual boxplot graph, outliers present
ggplot(bike_df, aes(y=casual)) +
  geom_boxplot() + ggtitle("casual")

#registered boxplot graph, no outliers present
ggplot(bike_df, aes(y=registered)) +
  geom_boxplot() + ggtitle("registered")

#total_count boxplot graph, no outliers present
ggplot(bike_df, aes(y=total_count)) +
  geom_boxplot() + ggtitle("total_count")

# Casual can be neglected, we take subset for windspeed and humidity
bike_df <- bike_df %>% 
  mutate(windspeed = ifelse(windspeed %in% boxplot.stats(windspeed)$out, NA, windspeed),
            humidity = ifelse(humidity %in% boxplot.stats(humidity)$out, NA, humidity))

# Imputing with median
bike_df$windspeed[is.na(bike_df$windspeed)] = median(bike_df$windspeed, na.rm = TRUE)
bike_df$humidity[is.na(bike_df$humidity)] = median(bike_df$humidity, na.rm = TRUE)

bike_df %>% filter(is.na(windspeed))
#windspeed boxplot graph, 2 outliers present
ggplot(bike_df, aes(y=windspeed)) +
  geom_boxplot() + ggtitle("windspeed")

#humidity boxplot graph, no outliers present
ggplot(bike_df, aes(y=humidity)) +
  geom_boxplot() + ggtitle("humidity")

#Correlatation analysis
#load the corrgram for correlation
library(corrgram)
#Correlation plot
corrgram(bike_df[,bike_df_numeric],order=F,upper.panel=panel.pie,text.panel=panel.txt,main='Correlation Plot')


#Drop real_temp as it has same positive correlation as temp, also drop casual and registered as they are only components of total_count
bike_df <- bike_df %>% select(-c(real_temp, casual, registered, date))

#Cut into train and test data
train_index <- sample(nrow(bike_df),0.75*nrow(bike_df))
train_data<-bike_df[train_index,]
test_data<-bike_df[-train_index,]

head(train_data)
head(test_data)


#Model Evaluation
model_eval <- function(x){
  print(summary(x))
  test_data$pred<- predict(x,test_data)
  print(paste("RMSE: ",rmse<-RMSE(test_data$pred, test_data$total_count)))
  print(paste("MAPE: ",mape<-MAPE(test_data$pred, test_data$total_count)))
  #Visualizing accuracy of model
  ggplot(test_data, aes(pred, total_count)) +
    geom_point() + 
    geom_abline(color = "blue")
}

RMSE <- function(x,y){
  res = x - y
  return(sqrt(mean(res^2)))
}

MAPE <- function(x,y){
  res = x - y
  return(mean(abs(res/y))*100)
}

#linear regression
set.seed(123)
fmla = as.formula("total_count ~ .")

lm_model = lm(fmla, train_data)

model_eval(lm_model)


#Gam model
library(mgcv)
gam_model = gam(total_count ~ season + year + month + holiday + 
                 weekday + workingday + weather_condition + s(temp) + s(humidity) +s(windspeed),
               train_data, family = gaussian)

model_eval(gam_model)

#Decission tree
library(rpart)
library(rpart.plot)
tree_model = rpart(fmla, train_data, method = "anova")
rpart.plot(tree_model)
model_eval(tree_model)

#Random forest
library(ranger)
rf_model = ranger(fmla, train_data, num.trees = 500, respect.unordered.factors = "order", seed = 123)

summary(rf_model)
test_data$pred<- predict(rf_model,test_data)$predictions
print(paste("RMSE: ",rmse<-RMSE(test_data$pred, test_data$total_count)))
print(paste("MAPE: ",mape<-MAPE(test_data$pred, test_data$total_count)))
#Visualizing accuracy of model
ggplot(test_data, aes(pred, total_count)) +
  geom_point() + 
  geom_abline(color = "blue")



