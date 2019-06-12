setwd("F:/01. Article Writing/A00. Submitted papers/A08. Air Quality Trends 20180420/02. Data analysis/")
workingDirectory<<-"F:/01. Article Writing/A00. Submitted papers/A08. Air Quality Trends 20180420/02. Data analysis/"

library (openair)
library(lubridate)
library(latticeExtra)
library(ggplot2)
library(dplyr)
library(plyr)
library(devtools)
library(normalweatherr)
library(rmweather)
library(ranger)

# Install normalweatherr
# install_github("davidcarslaw/normalweatherr")
####Normalweatherr usage examples

data_PM10<-import("data_PM10_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_PM10<-selectByDate(data_PM10, start="1/1/2013", end="31/12/2017")
# data_PM10<-add_date_variables(data_PM10)
data_PM10$value<-data_PM10$PM10
list_input_data <- split_input_data(data_PM10, fraction=0.7)
#list_input_data_2 <- list_input_data %>% 
#  filter(!is.na(ws)) %>% 
#  filter(!is.na(value)) %>% 
#  rmw_prepare_data(na.rm = TRUE)
#write.csv(data_CO,paste(workingDirectory,"data_CO.csv",sep=""))

# variables <- c("wd","ws", "temp","RH", "industry", "heating","power","residential","transportation","date_unix", "weekday","hour")
# variables <- c("cluster","wd","ws", "temp","RH","date_unix","day_julian", "weekday","hour")
######NORMALIZATION for MET by randomly peak the MET data for month

variables <- c("wd","ws", "temp","RH","date_unix","day_julian", "weekday","hour")
model_rf_PM10 <- calculate_model(   ###### BUILD THE MODEL
  list_input_data, 
  variables = variables, 
  mtry = 4,
  nodesize = 3,
  ntree=200,
  model = "rf")
model_rf_PM10$model                     ##### CHECK THE MODEL PERFORMANCE

training<-list_input_data$training
m<-model_rf_CO$model
t<-m$predicted

#training$predictions<-m$predicted
testing<-list_input_data$testing
testing_model<- normalise_for_meteorology(      ##### RUN MODEL WITH NEW MET
  model_rf_PM10$model, 
  testing, 
  variables = setdiff(variables,variables),n = 1)
testing$predictions<-testing_model$value_predict
testing$observations<-testing$value
daily_testing<-timeAverage(testing, avg.time="24 hour")
scatterPlot(daily_testing, x = "observations", y = "predictions",
            xlab="observations (mg/m3)", ylab = "predictions (mg/m3)")
scatterPlot(testing, x = "observations", y = "predictions", col= "jet"
            ,method = "density")

timePlot(testing, pollutant=c("observations","predictions"), group=TRUE)

write.csv(testing,paste(workingDirectory,"testing_PM10.csv",sep=""))
modStats(testing, mod = "predictions", obs = "observations", statistic = c("n", "FAC2",
              "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA"),
         type = "default", rank.name = NULL)
fit_PM10 <- lm(predictions ~ observations, data = testing)
summary(fit_PM10)









all_model<- normalise_for_meteorology(      ##### RUN MODEL WITH NEW MET
  model_rf_PM10$model, 
  data_PM10, 
  variables = setdiff(variables,variables),n = 1)
data_PM10$predictions<-all_model$value_predict
write.csv(month_PM10,paste(workingDirectory,"month_PM10_predictions.csv",sep=""))
month_PM10<-timeAverage(data_PM10, avg.time="month")


weekly_testing<-timeAverage(testing, avg.time="month")
scatterPlot(weekly_testing, x = "value", y = "predictions")
timePlot(weekly_testing, pollutant=c("value","predictions"), group=TRUE)


#####REMOVE WEATHER
data_CO<-import("data_CO_2013_2017.csv", date="date", date.format = "%d/%m/%Y %H:%M")
data_CO<-selectByDate(data_CO, start="1/1/2013", end="31/12/2017")
# data_CO<-add_date_variables(data_CO)
# data_CO$value<-data_CO$CO
# list_input
#list_data <- split_input_data(data_CO)

data_CO_prepared <- data_CO %>% 
  filter(!is.na(ws)) %>% 
  filter(!is.na(CO)) %>% 
  dplyr::rename(value = CO) %>% 
  rmw_prepare_data(na.rm = FALSE, fraction=0.75)
            
remove_CO <- rmw_do_all(
    data_CO_prepared,
    variables = c("wd","ws", "temp","RH", "press",
                  "date_unix", "day_julian","weekday","hour"),
    n_trees = 200, mtry = 3, n_samples = 150,min_node_size = 3,
    verbose = TRUE)

# Plot variable importances
remove_CO$model %>% 
  rmw_model_importance() %>% 
  rmw_plot_importance()









rmw_model_statistics(remove_PM2.5$model)
# Check if model has suffered from overfitting
rmw_predict_the_test_set(
  model = remove_PM2.5$model,
  df = remove_PM2.5$observations
) %>% 
  rmw_plot_test_prediction()

obs<-remove_PM2.5$observations
model<-remove_PM2.5$model

testing<-subset(obs, set=="testing",select=c(set,date,value)) 
training<-subset(obs, set=="training",select=c(set,date,value))
training$predict<-predict
training$observation<-training$value

obs$predictions<-s$value_predict
obs$observations<-obs$value
testing<-subset(obs, set=="testing",select=c(set,observations,predictions)) 
training<-subset(obs, set=="training",select=c(set,observations,predictions)) 
scatterPlot(testing, x = "observations", y = "predictions", col= "jet"
            ,method = "density")



predict<-model$predictions

cor(data_PM2.5$value, data_normalised_PM2.5$value_predict, method = c("pearson"))
plot(data_PM2.5$value,  data_normalised_PM2.5$value_predict)
abline(lm(data_PM2.5$value~data_normalised_PM2.5$value_predict), col="red") # regression line (y~x)
abline(a = 0, b = 1, col = 2)

cor(x$value, t$predicted, method = c("pearson"))
cor(x$value, t$predicted, method = c("kendall"))
cor(x$value, t$predicted, method = c("spearman"))

plot(x$value,  t$predicted)
# Add fit lines
abline(lm(x$value~t$predicted), col="red") # regression line (y~x) 
abline(a = 0, b = 1, col = 2)

plot(k$value,  k$value_predict)
cor(k$value, k$value_predict, method = c("pearson"))
abline(a = 0, b = 1, col = 2)

t<-model_rf_PM2.5$model
k<-t$predicted
x<-model_rf_PM2.5$training
k<-predict_using_test_set(model_rf_PM2.5) 


## CREAT the THEME
library(tidyverse)
my_theme <- function(base_size =5, base_family = "sans"){
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA),
      strip.background = element_rect(fill = "#001d60", color = "#00113a", size =0.5),
      strip.text = element_text(face = "bold", size = 5, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank()
    )
}

theme_set(my_theme())

theme_bare <- function(base_size=8,base_family="sans"){theme_bw(base_size = base_size, base_family = base_family)+
    theme(
      axis.line = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.ticks = element_blank(), 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      legend.position = "bottom", 
      panel.background = element_rect(fill = NA), 
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.margin = unit(c(0,0,0,0), "lines")
    )
}

#df=read.csv("http://vincentarelbundock.github.io/Rdatasets/csv/MASS/biopsy.csv")%>%as_tibble()%>%.[,c(3:12)]%>%na.omit()

names(df)=c("clumpthickness",
            "SizeUniformity",
            "ShapeUniformity",
            "Margin_adhesion",
            "EpiCellSize",
            "Barenuclei",
            "BlandChromatin",
            "NormalNucleoli",
            "Mitoses",
            "Class"
)

library(caret)


data_PM2.5<-import("data_PM2.5_2013_2017_RF.csv", date="date", date.format = "%d/%m/%Y %H:%M")%>%as_tibble()%>%.[,c(1:13)]%>%na.omit()
df_PM2.5<-data_PM2.5[,2:13]
df_PM2.5<-df_PM2.5[,-2]

set.seed(1234)
#idtest=caret::createDataPartition(y=df$Class, p=30000/37462,list=FALSE)
idtest=caret::createDataPartition(y=df_PM2.5$PM2.5, p=30000/37462,list=FALSE)


trainset=df_PM2.5[-idtest,]
testset=df_PM2.5[idtest,]

library(randomForest)

rfmod=randomForest(PM2.5~.,
                   data=trainset,
                   ntree=5,
                   mtry=sqrt(10),
                   replace=TRUE,
                   localImp=TRUE
)

rfmod

tuned.mtry <- tuneRF(trainset, trainset$PM2.5, mtryStart = 2, ntreeTry = 500, stepFactor = 2, improve = 0)

testpred=predict(rfmod,testset)

confusionMatrix(as.vector(testpred),
                reference=testset$PM2.5,
                positive ="malignant",
                mode="everything")

plot(rfmod, 
     main = "RF Learning curve")
legend("topright", 
       c("error for 'malignant class'", 
         "misclassification error", 
         "error for 'benign class'"), 
       lty = c(1,1,1), 
       col = c("green", "black", "red"))

varImpPlot(rfmod)

varImp(rfmod)%>%as.data.frame()%>%
  mutate(Feature=rownames(.))%>%
  gather(benign,malignant,key="Label",value="Importance")%>%
  mutate(Importance=100*Importance/max(.$Importance))%>%
  ggplot(aes(x=reorder(Feature,Importance),
             y=Importance,
             fill=Importance,
             color=Importance))+
  geom_segment(aes(x=reorder(Feature,Importance),
                   xend=Feature,
                   y=0,
                   yend=Importance),
               size=1,
               show.legend = F)+
  geom_point(size=2,show.legend = F)+
  scale_x_discrete("Features")+
  coord_flip()+
  scale_fill_gradient(low="blue",high="red")+
  scale_color_gradient(low="blue",high="red")+
  geom_text(aes(label = round(Importance,1)),
            vjust=-0.5,size=3)+
  facet_wrap(~Label,ncol=2,scales="free")+
  theme_bw(base_family = "mono",8)

library(randomForestExplainer)

# 5 most Imp var

rfmod%>%
  important_variables(k=5,ties_action = "draw")


rfmod%>%measure_importance(mean_sample = "relevant_trees")%>%
  as_tibble()->vimpdf

vimpdf%>%head()

mindpint=min_depth_interactions(rfmod,mean_sample = "relevant_trees", 
                                uncond_mean_sample = "relevant_trees")

library(viridis)

mindpint%>%
  plot_min_depth_interactions()+
  scale_fill_viridis("A")

library(igraph)
library(ggraph)
library(ggplot2)
#remove.packages("ggraph")
#install.packages("ggraph")

gdf=filter(mindpint,
           occurrences>=180)

graph<-graph_from_data_frame(gdf[,c(1:3,4,6)])


ggraph(graph,
       layout = 'kk',
       circular=F)+
  geom_edge_link(aes(colour = occurrences),
                 show.legend = T,width=1,alpha=0.7)+
  geom_node_label(aes(label = name))+
  coord_fixed()+
  scale_edge_color_gradient(high="#ff003f",low="#0094ff")+
  scale_edge_width_continuous()+
  theme_bare()




