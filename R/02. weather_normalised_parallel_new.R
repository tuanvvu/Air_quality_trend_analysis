### A new code for re-sample weather & predict the concentration of a pollution 
# using new re-samped weather using parallel on HPC for faster running based on R- "rmweatherr" package from Grange et al (2019). 
### Any comments, please feel free contact Tuan, v.vu@bham.ac.uk
###############################################################################

library (openair)
library(plyr)
library(dplyr)
library(rmweather)

##01. Download PM2.5 data from: https://github.com/tuanvvu/Air_quality_trend_analysis/tree/master/Data
#into your personal folders, i.e. "E:/NOAA/"

# Use Parallel computing: 
##02. Read the data which has been download
setwd("E:/NOAA/")  ### Set the working directory
workingDirectory<<-"E:/NOAA/"  ### Shortcut for the working directory
data_pm2.5<-import("data_PM2.5_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")

## Preprare the training data set
data_prepared_pm2.5 <- data_pm2.5%>% 
  filter(!is.na(ws)) %>% 
  dplyr::rename(value = PM2.5) %>% 
  rmw_prepare_data(na.rm = TRUE)
# MET variable: temperature (temp), Relative Humidity (RH), wind speed (ws), wind direction (wd), atmospheric pressure (pressure), back-trajectories (cluster).
set.seed(12345) 

## Build the model
RF_pm2.5_model <- rmw_do_all(
  data_prepared_pm2.5,
  variables = c("date_unix", "day_julian", "weekday", "hour", "temp", "RH", "wd", "ws", "pressure"),
  n_trees = 300,
  n_samples = 300,
  verbose = TRUE
)

## Initial MET data 
data_MET<-import("data_PM2.5_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
## Import data set for 5-year MET data. If you want 30-year, you can import the file"data_met_1988_2017.csv". Here is for 5-year for example.
re_sample_MET<-import("data_PM2.5_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
pred<- data_pm2.5 %>% select(1)
new_met<- data_MET %>%  slice(0)

##Function to generated the new- weather from original weather
## This algorithm will remove the effects of shor impact of weather between each year, but not normlaise the seasonal impact in each year. You can you theisel to remove. 
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
    re_MET <- ldply(1:nrow(data_MET), new_met, .parallel = TRUE) # Using parallel
    re_sample_MET[,8:15]<-re_MET[,8:15] ### Replaced old MET by generated MET
    prediction_value<- rmw_predict( ### RUN Random Forest model with new MET dataset
       RF_pm2.5_model$model, 
      df= rmw_prepare_data(re_sample_MET, value = "PM2.5"))
    pred<-cbind(pred,prediction_value)}
  pred}

final_weather_normalised <- prediction (100)  ## Randomly ran for 100 predictions. Increase the number of prediction until it is stable
final_weather_normalised$final <- apply(final_weather_normalised[,2:101],1,mean, na.rm=TRUE) ### Mean value of 100 predictions
### Save the prediction
write.csv(final_weather_normalised,paste(workingDirectory,"final_weather_normalised.csv",sep="")) ### Save 1000 predictions

### COMPARiSON obseved and nomarlised concentration. May correct with testing data.
data_compare<-merge(data_pm2.5,final_weather_normalised, by="date" ) 

data_compare <- data_compare %>% dplyr:: rename(Modeled.PM2.5=final,Observed.PM2.5=PM2.5) %>%
                                  select(date,Observed.PM2.5,Modeled.PM2.5)

timePlot(data_compare,pollutant=c("Observed.PM2.5", "Modeled.PM2.5"), 
         lwd=c(1,2), group=TRUE, lty=c(1,1),avg.time ="month",
        key.position="top",cols=c("darkgreen","firebrick4"),
          ylab=expression("PM2.5 concentration"* " (" * mu * "g m" ^-3 * ")"))
 
