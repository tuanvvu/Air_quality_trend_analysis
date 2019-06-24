library (openair)
library(lubridate)
library(latticeExtra)
library(ggplot2)
require(devtools)
library(worldmet)
library(dplyr)
library(plyr)
library(normalweatherr)
library(rmweather)

setwd("C:/00.data_analysis_UK/")
workingDirectory<<-("C:/00.data_analysis_UK/")

# traj <- importTraj(site = "london", year = 2014)

### COMBINE DATA WITH MET DATA
cluster<-import("London_cluster_2014_2018.csv", date="date", date.format = "%d/%m/%Y %H:%M")
MET<-import("London_MET_2014_2018.csv", date="date", date.format = "%d/%m/%Y %H:%M")
cluster_MET<-merge(MET, cluster, by="date", all.x=TRUE)
NO2_NK<-import("NO2_NK.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_NO2_NK<-merge(cluster_MET,NO2_NK, by="date", all.x=TRUE)
write.csv(data_NO2_NK,paste(workingDirectory,"data_NO2_NK.csv",sep=""))

###Glasgow
data_Glasgow<-import("data_Glasgow.csv", date="date", date.format = "%d/%m/%Y %H:%M")
NO2_GT<-import("NO2_GT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_Glasgow<-merge(data_Glasgow,NO2_GT, by="date", all.x=TRUE)
write.csv(data_Glasgow,paste(workingDirectory,"data_GT_glasgow.csv",sep=""))

##PAris
MET<-import("Paris_Montsouris_2014_2018.csv", date="date", date.format = "%d/%m/%Y %H:%M")
hourly_MET<-timeAverage(MET, avg.time="1 hour")
cluster<-import("Paris_cluster_2014_2018.csv", date="date", date.format = "%d/%m/%Y %H:%M")
cluster_MET<-merge(hourly_MET,cluster, by="date", all.x=TRUE)
AP_Vitry<-import("AP_Vitry.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_AP_Vitry<-merge(cluster_MET,AP_Vitry, by="date", all.x=TRUE)
write.csv(data_AP_Vitry,paste(workingDirectory,"data_AP_Vitry.csv",sep=""))

AP_Vitry<-import("data_AP_Vitry.csv", date="date", date.format = "%d/%m/%Y %H:%M")
AP_AUT<-import("AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_AP_Vitry<-merge(AP_Vitry,AP_AUT, by="date", all.x=TRUE)
write.csv(data_AP_Vitry,paste(workingDirectory,"data_AP_AUT.csv",sep=""))



### DATA ANALYSIS
data_Coarse_AUT<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
#data_Coarse_AUT<-selectByDate(data_Coarse_AUT, start="1/1/2013", end="31/12/2017")
data_Coarse_AUT$value<-data_Coarse_AUT$AUT.Coarse 
#data_Coarse_AUT<-add_date_variables(data_Coarse_AUT)
list_input_data <- split_input_data(data_Coarse_AUT)
#write.csv(data_Coarse_AUT,paste(workingDirectory,"data_MET_2014_2018.csv",sep=""))

variables <- c("wd","ws", "air_temp","atmos_press","RH","date_unix","day_julian","week", "weekday","hour","cluster")
model_rf_Coarse_AUT <- calculate_model(   ###### BUILD THE MODEL
  list_input_data, 
  variables = variables, 
  mtry = 3,
  nodesize = 5,
  ntree=300,
  model = "rf")

model_rf_Coarse_AUT$model 

#### APROACH II: MET RANDOMLY for 2 weeks between and after observed data
data_MET<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
re_MET<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
Normalized_Paris_data<-import("data_normalisation_initial_1000.csv", date="date", date.format = "%d/%m/%Y")

for (j in 1:300){
  for (i in 1:43824){                      ##### MET RANDOMLY PEAK UP
    h<-re_MET[i,5]
    w<-re_MET[i,3]
    
    if(w==1){
      t<-data_MET %>% filter(hour==h)  %>% filter(week>=52|week <= 3)  %>% sample_n(10)}
    if(w==2){
      t<-data_MET %>% filter(hour==h)  %>% filter(week>=53|week <= 4)  %>% sample_n(10)}
    if(w==52) {
      t<-data_MET %>% filter(hour==h)  %>% filter(week>=50|week <= 1)  %>% sample_n(10)}
    if(w==53) {
      t<-data_MET %>% filter(hour==h)  %>% filter(week>=51|week <= 2)  %>% sample_n(10)}
    if(w>2 & w<51){
      t<-data_MET %>% filter(hour==h)  %>% filter(week>= w-2 & week <= w+2)  %>% sample_n(10)}  
    
    re_MET[i,8:15]<-t[5,8:15]} ## 12:09 to 12:14
  
  rd<- normalise_for_meteorology(      ##### RUN MODEL WITH NEW MET
    model_rf_Coarse_AUT$model, 
    re_MET, 
    variables = setdiff(variables,variables),n = 1)
  
  Normalized_Paris_data[,j+1]<- rd$value_predict}

Normalized_Paris_data<-Normalized_Paris_data%>% mutate_if(is.numeric, round, digits = 2)
Normalized_Paris_data$n_value<-apply(Normalized_Paris_data[,2:1001],1,mean,na.rm=TRUE)
write.csv(Normalized_Paris_data,paste(workingDirectory,"Normalized_AUT.Coarse_300_raw.csv",sep=""))
write.csv(Normalized_Paris_data[,c(1,1002)],paste(workingDirectory,"Normalized_AUT.Coarse_300.csv",sep=""))
#
timePlot(data_NO2,pollutant=c("UB","TF","RR"),
         lwd=2.0, group=TRUE, lty=1,avg.time ="month",
         key.position="right", cols="jet",
         key.col = 1, main="Traffic and Urban Increment",
         ylab=expression("NO2 Increment"* " (" * mu * "g m" ^-3 * ")"))


a<-model_rf_PM10_NK$model 


Bowen<-import("Bowen.csv", date="date", date.format = "%d/%m/%Y %H:%M")
timePlot(Bowen,pollutant=c("PM2.5"),
         lwd=2.0, group=TRUE, lty=1,avg.time ="month")


library(rmweather) ### Random Forest/ rmweather from Grange Github

data_Coarse_AUT<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_Coarse_AUT[9:21] <- lapply(data_Coarse_AUT[9:21], as.numeric)

# data preparation
data_prepared_Coarse_AUT <- data_Coarse_AUT %>% 
  filter(!is.na(ws)) %>% 
  dplyr::rename(value = AUT.Coarse) %>% 
  rmw_prepare_data(na.rm = TRUE)

#MET variable: temperature (temp), Relative Humidity (RH), wind speed (ws), wind direction (wd), atmospheric pressure (pressure), back-trajectories (cluster).
set.seet(123) 

###02.  Build RF model: 

RF_Coarse_AUT_model <- rmw_do_all(
  data_prepared_Coarse_AUT,
  variables = c(
    "date_unix", "day_julian", "weekday", "hour", "air_temp", "RH", "wd", "ws", "atmos_press"
  ),
  n_trees = 300,
  n_samples = 300,
  verbose = TRUE
)

### 03. Predict the level of a pollutant in different weather condition

# Initial MET data 
data_MET<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
re_sample_MET<-import("data_AP_AUT.csv", date="date", date.format = "%d/%m/%Y %H:%M")
# Initial_nomarlised_data<-import("data_normalisation_initial_1000.csv", date="date", date.format = "%d/%m/%Y %H:%M")### A blank matrix, nrow= nrow(PM2.5_MET_2013_2017), ncol=1000
pred<- data_Coarse_AUT %>% select(1)
new_met<- data_MET %>%  slice(0)

# Use Parallel computing: 

library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
library("doFuture")
  registerDoFuture()
  plan(multiprocess)
      
# Function to re-sample the weather
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
  
# Function to predict the concentration of a pollutant using a re-sampled weather        
set.seed(10000)

prediction <- function (n) {
  for (i in 1:n) {
    re_MET <- ldply(1:43824, new_met, .parallel = TRUE)
    re_sample_MET[,8:27]<-re_MET[,8:27] 
    prediction_value<- rmw_predict( ### RUN Random Forest model with new MET dataset
      RF_Coarse_AUT_model$model, 
      df= rmw_prepare_data(re_sample_MET, value = "AUT.Coarse"))
    pred<-cbind(pred,prediction_value)}
    pred}

final_weather_normalised <- prediction (300)
write.csv(final_weather_normalised,paste(workingDirectory,"final_weather_normalised.csv",sep=""))








  
  













  re_MET <- ldply(1:43824, new_met, .parallel = TRUE)

  a<-laply(1:43824, new_met, .parallel = TRUE)



### Final_weather_normalised_PM2.5 by aggregating 1000 single predictions.
# Pollutant_prediction (1000) ### random by 1000 times
final_nomarlised_prediction <- Pollutant_prediction (10)

write.csv(final_nomarlised_prediction,paste(workingDirectory,"a_test.csv",sep=""))

final_weather_nomarlised_PM2.5 %>% apply(nomarlised_prediction[,2:1001],1,mean, na.rm=TRUE) ### Mean value of 1000 predictions

predict_PM2.5_level<- predict( ### RUN Random Forest model with new MET dataset
  RF_Coarse_AUT_model$model, 
  re_MET
)    














