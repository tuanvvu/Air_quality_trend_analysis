### Any question, please contact: v.vu@bham.ac.uk
setwd("...")
workingDirectory<<-"..."

library (openair)
library(lubridate)
library(normalweatherr)
# Install normalweatherr
# install_github("davidcarslaw/normalweatherr")
####Normalweatherr usage examples

data_PM2.5<-import("data_PM2.5_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_PM2.5<-selectByDate(data_PM2.5, start="1/1/2013", end="31/12/2017")
# data_PM2.5<-add_date_variables(data_PM2.5)
data_PM2.5$value<-data_PM2.5$PM2.5
list_input_data <- split_input_data(data_PM2.5, fraction=0.7)
variables <- c("wd","ws", "temp","RH","date_unix","day_julian", "weekday","hour")
set.seed(123)
model_rf_PM2.5 <- calculate_model(   ###### BUILD THE MODEL
  list_input_data, 
  variables = variables, 
  mtry = 4,
  nodesize = 3,
  ntree=200,
  model = "rf")
model_rf_PM2.5$model                     ##### CHECK THE MODEL PERFORMANCE FOR TRAINING 

testing<-list_input_data$testing
testing_model<- normalise_for_meteorology(      ##### RUN MODEL WITH THE testing data set
  model_rf_PM2.5$model, 
  testing, 
  variables = setdiff(variables,variables),n = 1)
testing$predictions<-testing_model$value_predict
testing$observations<-testing$value

scatterPlot(testing, x = "observations", y = "predictions", col= "jet" ,method = "density") ### scatter plot
timePlot(testing, pollutant=c("observations","predictions"), group=TRUE) ### time series plot
modStats(testing, mod = "predictions", obs = "observations", statistic = c("n", "FAC2",
              "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA"),
         type = "default", rank.name = NULL)

fit_PM2.5 <- lm(predictions ~ observations, data = testing) ### linear regression between predictions and observations
summary(fit_PM2.5)  ### for slop and intercept

