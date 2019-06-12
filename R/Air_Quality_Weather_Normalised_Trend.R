### This code is based on the "normalweatherr" package from Grange et al. (2018):https://github.com/skgrange/normalweatherr
### Any question, please contact Tuan, v.vu@bham.ac.uk
setwd("F:/Air Quality Trends/ Data analysis/")
workingDirectory<<-"F:/Air Quality Trends/ Data analysis/"

library(normalweatherr) ### Random Forest/ Weather Normalization from Grange Github
### Reference: https://github.com/skgrange/normalweatherr
library (openair) ### For Theil-sen analysis

###01. Import the data set which contains: date, PM2.5 and MET weathers
data_PM2.5<-import("data_PM2.5_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_PM2.5<-add_date_variables(data_PM2.5)
data_PM2.5$value<-data_PM2.5$PM2.5

# names(data_PM2.5)  
#Variables in the data set: date, date_unix, day_julian, week, weekday, hour
#MET variable: temperature (temp), Relative Humidity (RH), wind speed (ws), wind direction (wd), atmospheric pressure (pressure), back-trajectories (cluster).
set.seet(123) 
list_input_data <- split_input_data(data_PM2.5, fraction =0.7) # split data set into training data set and testing data set

variables <- c("date_unix","day_julian","weekday","hour","wd","ws", "temp","RH","pressure")

###02.  Build RF model: 

model_random_forest_PM2.5 <- calculate_model(  
  list_input_data, 
  variables = variables, 
  mtry = 4,
  nodesize = 3,
  ntree=200,
  model = "rf")

model_random_forest_PM2.5$model  # Model performance on the Training data set

### 03. Predict the level of a pollutant in different weather condition

# Initial MET data 
MET_1988_2017<-import("MET_1988_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
MET_2013_2017<-import("PM2.5_MET_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
# Initial_nomarlised_data<-import("data_normalisation_initial_1000.csv", date="date", date.format = "%d/%m/%Y %H:%M")### A blank matrix, nrow= nrow(PM2.5_MET_2013_2017), ncol=1000
nomarlised_prediction<- data_PM2.5[,1]

#Predict the level of a pollutant in different weather condition
Pollutant_prediction <-function (n){    ###n is the number of re-sample MET data set
  for (j in 1:n){
    for (i in 1:43824){           ### 43824 is number of hourly obserbvation from 2013-2017                    
      hour_1<-MET_2013_2017[i,5] ### "hour" variable is in the 5th column in the data set  
      week_1<-MET_2013_2017[i,3] ### "week" variable is in the 3rd column in the data set 
      ### Randomly sample weather data from 1988-2017
      if(week_1==1){
        MET_sample<-MET_1988_2017 %>% filter(hour==hour_1)  %>% filter(week>=52|week <= 3)  %>% sample_n(10)}
      if(week_1==2){
        MET_sample<-MET_1988_2017 %>% filter(hour==hour_1)  %>% filter(week>=53|week <= 4)  %>% sample_n(10)}
      if(week_1==52) {
        MET_sample<-MET_1988_2017 %>% filter(hour==hour_1)  %>% filter(week>=50|week <= 1)  %>% sample_n(10)}
      if(week_1==53) {
        MET_sample<-MET_1988_2017 %>% filter(hour==hour_1)  %>% filter(week>=51|week <= 2)  %>% sample_n(10)}
      if(week_1>2 & week_1<51){
        MET_sample<-MET_1988_2017 %>% filter(hour==hour_1)  %>% filter(week>= week_1-2 & week <= week_1+2) %>% sample_n(10)}  
      ### Generate the new dataset of MET data from 2013-2017 by 1988-2017  
        MET_2013_2017[i,9:18]<-MET_sample[5,9:18]} # Generate the new data met for 2013-2017 by 1988-2017
    
    predict_PM2.5_level<- normalise_for_meteorology( ### RUN Random Forest model with new MET dataset
      model_rf_PM2.5$model, 
      MET_2013_2017, 
      variables = setdiff(variables,variables), n=1)
    
    nomarlised_prediction <-cbind(nomarlised_prediction, predict_PM2.5_level$value_predict)}
      }

### Final_weather_normalised_PM2.5 by aggregating 1000 single predictions.
Pollutant_prediction (1000) ### random by 1000 times
final_weather_nomarlised_PM2.5 %>% apply(nomarlised_prediction[,2:1001],1,mean, na.rm=TRUE) ### Mean value of 1000 predictions
