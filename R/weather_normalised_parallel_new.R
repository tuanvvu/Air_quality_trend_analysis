### A new code for re-sample weather & predict the concentration of a pollution using new re-samped weataherusing parallel on HPC for faster running
## Author: Tuan Vu; v.vu@bham.ac.uk
###############################################################################

library (openair)
library(plyr)
library(dplyr)
library(rmweather)

# Use Parallel computing: 
library(magrittr) # need to run every time you start R and want to use %>%
library("doFuture") ### Parallel running
  registerDoFuture()
  plan(multiprocess) # Using all core of computer

###01. Data preparation
setwd("C:/.../")
workingDirectory<<-("C:/.../")
# Import the data
data_Coarse_AUT<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_Coarse_AUT[9:21] <- lapply(data_Coarse_AUT[9:21], as.numeric) ### set the data as numeric values

# data preparation # remove the weataher
data_prepared_Coarse_AUT <- data_Coarse_AUT %>% 
  filter(!is.na(ws)) %>% 
  dplyr::rename(value = AUT.Coarse) %>% 
  rmw_prepare_data(na.rm = TRUE)
# MET variable: temperature (temp), Relative Humidity (RH), wind speed (ws), wind direction (wd), atmospheric pressure (pressure), back-trajectories (cluster).
set.seet(12345) 

###02.  Build RF model using rmweather packages
RF_Coarse_AUT_model <- rmw_do_all(
  data_prepared_Coarse_AUT,
  variables = c("date_unix", "day_julian", "weekday", "hour", "air_temp", "RH", "wd", "ws", "atmos_press"),
  n_trees = 300,
  n_samples = 300,
  verbose = TRUE
  )

###03. Predict the level of a pollutant in different weather condition

# Initial MET data 
data_MET<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
re_sample_MET<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
pred<- data_Coarse_AUT %>% select(1)
new_met<- data_MET %>%  slice(0)
      
#Function to generated the new- weather from original weather
  new_met<-function (i) {
    hour_1<-data_MET[i,5] ### "hour" variable is in the 5th column in the data set  
    day_1 <-data_MET[i,7] ### "day_julian" variable is in the 7th column in the dataset
    if(day_1 <= 14){
      MET_sample<-data_MET %>% filter(hour==hour_1)  %>% filter(day_julian>= 365 + day_1-14 |day_julian <= day_1 +14)  %>% sample_n (1)}
    if(day_1 > 14 & day_1<352){
      MET_sample<-data_MET %>% filter(hour==hour_1)  %>% filter(day_julian>=day_1-14 & day_julian <= day_1+14)  %>% sample_n (1)}
    if(day_1 >=352) {
      MET_sample<-data_MET %>% filter(hour==hour_1)  %>% filter(day_julian>= day_1-14|day_julian <= day_1 +14-365)  %>% sample_n (1)}
    new_met<-MET_sample
    return (new_met)}
  
#Function to predict the concentration of a pollutant using a re-sampled weather        
set.seed(12345)
prediction <- function (n) {
  for (i in 1:n) {
    re_MET <- ldply(1:43824, new_met, .parallel = TRUE) # Using parallel
    re_sample_MET[,8:27]<-re_MET[,8:27] ### Replaced old MET by generated MET
    prediction_value<- rmw_predict( ### RUN Random Forest model with new MET dataset
      RF_Coarse_AUT_model$model, 
      df= rmw_prepare_data(re_sample_MET, value = "AUT.Coarse"))
    pred<-cbind(pred,prediction_value)}
    pred}

final_weather_normalised <- prediction (1000)
write.csv(final_weather_normalised,paste(workingDirectory,"final_weather_normalised.csv",sep=""))
