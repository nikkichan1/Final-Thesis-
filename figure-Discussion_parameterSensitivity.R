###############
###Load package
# This code is to test the parameter sensitive for two outlier removal methods.
# Then create a plot.
# The figure is used in the dicussion
#
#

rm(list=ls())
library(dplyr) 
library(extRemes)
library(readr)
library(reshape2)
library(GenSA)
library(gamlss) 
library(evd) 
library(lubridate)
library(nsRFA)
library(Rlof)
library(Lmoments)
library(ggplot2)
library(strucchange)
library(cowplot)
library(gridExtra)            
library(hydroGOF)
library(cowplot)


# You need to change the working directory
pwd <- getwd()
setwd(pwd)


# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")

#########################
##run all get the results
runALL <- function(csvdata,threshold_1,threshold_2){
  
  currentdata <- csvdata
  raw_data <- data.frame(year = seq(1850,2015),
                         maxVal = currentdata$random_numbers,
                         flow100 = currentdata$return_level)
  
  window_size <- 30
  return_periods <- c(30,50,100) # set the return periods
  
  dataTemp  <- raw_data
  df_ini <- Discharge_at_GivenReturnPeriodandSamples(dataTemp,window_size,return_periods)
  
  ##################
  ####Using method 1.
  dataTemp_1  <- raw_data 
  dataTemp_1 <- Reject_outliers_method1(dataTemp_1,threshold_returnPeriod=threshold_1,chunk=30)
  dataTemp_1$r <- NULL
  df_one <- Discharge_at_GivenReturnPeriodandSamples(dataTemp_1,window_size,return_periods)
  
  ##################
  ####Using method 2.
  dataTemp_2  <- raw_data 
  dataTemp_2 <- Reject_outliers_method2(dataTemp_2,window_size=30,threshold_times = threshold_2)
  dataTemp_3<-dataTemp_2
  dataTemp_3$frequency <- NULL
  df_two <- Discharge_at_GivenReturnPeriodandSamples(dataTemp_3,window_size,return_periods)
  
  
  ##################
  ####Using GAMLSS methods.
  data_second <- GamlssFigure1Data(raw_data,baseyear = 1950)
  
  
  
  ##################
  ####combine the  data
  na_indices <- which(is.na(dataTemp_1$maxVal))
  outlier_raw_1<- raw_data[na_indices, ]
  na_indices <- which(is.na(dataTemp_2$maxVal))
  outlier_raw_2<- raw_data[na_indices, ]
  
  
  mw<-df_ini$return_100
  gamlass<-data_second$flood_magnitude_100yr
  mw_outlier1<-df_one$return_100
  mw_outlier2<-df_two$return_100
  flow_100<-raw_data$flow100
  
  
  mwdf<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw)
  gamlassdf<-data.frame(raw_data$year, gamlass)
  origindf<-data.frame(raw_data$year, flow_100)
  outlierdf_1<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw_outlier1)
  outlierdf_2<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw_outlier2)
  
  
  outlier_1<-data.frame(outlier_raw_1$year, outlier_raw_1$maxVal)
  outlier_2<-data.frame(outlier_raw_2$year, outlier_raw_2$maxVal)
  
  
  names(mwdf)<-c("year","mw_100")
  names(gamlassdf)<-c("year","gamlass_100")
  names(origindf)<-c("year","orgin_100")
  names(outlierdf_1)<-c("year","outlier1_100")
  names(outlierdf_2)<-c("year","outlier2_100")
  
  names(outlier_1)<-c("year","outlier_1")
  names(outlier_2)<-c("year","outlier_2")
  
  
  reswl_all<- merge(gamlassdf,mwdf, by="year", all=TRUE)
  reswl_all<- merge(raw_data,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(origindf,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlierdf_1,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlierdf_2,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlier_1,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlier_2,by="year", all=TRUE)
  
  reswl_all2 <- reswl_all[,c("year","maxVal","orgin_100","gamlass_100","mw_100","outlier1_100","outlier2_100","outlier_1","outlier_2")]
  names(reswl_all2) <- c("year","rawdata","truth","gamlass","mw","mwOutlier1","mwOutlier2","Outlier1","Outlier2")
  
  return(reswl_all2)
}


##################
## evaluation
evaluationmetric <- function(reswl_all){
  evaluation_results <- data.frame(mae_outlier1 = mae(reswl_all$truth,
                                                      reswl_all$mwOutlier1),
                                   mae_outlier2 = mae(reswl_all$truth,
                                                      reswl_all$mwOutlier2),
                                   mae_mw = mae(reswl_all$truth,
                                                reswl_all$mw))
  return(evaluation_results)
}

####### Outlier1 performs best
fnames1 <- "./syntheticdataV2/ChangingLocation_Linear/Linear_4546_909.2_-0.0119584849289948_1_exp.csv"
fnames2 <- "./syntheticdataV2/ChangingLocation_Partial/Partial_12966_3889.8_-0.115845393337307_80_0.33_0.67_exp.csv"
fnames3 <- "./syntheticdataV2/ChangingLocation_Partial/Partial_25979_5195.8_-0.213318655675175_80_0.25_0.75_exp.csv"
fnames4 <- "./syntheticdataV2/ChangingLocation_Partial/Partial_76_22.8_-0.185561787469612_80_0.33_0.67_exp.csv"
fnames5 <- "./syntheticdataV2/ChangingLocation_Partial_1/Partial1_4546_1363.8_-0.0119584849289948_80_50_1000.2_0.6_exp.csv"
fnames6 <- "./syntheticdataV2/ChangingLocation_Power/Power_12966_2593.2_-0.115845393337307_0.75_exp.csv"
fnames7 <- "./syntheticdataV2/ChangingLocationScale_Power/Power_1950_390_-0.192943477412856_1_0.02_exp.csv"
fnames8 <- "./syntheticdataV2/ChangingScale_Partial/Partial_76_7.6_-0.185561787469612_80_0.33_0.67_exp.csv"
fnames9 <- "./syntheticdataV2/ChangingScale_Partial_1/Partial1_4546_1363.8_-0.0119584849289948_80_50_1000.33_0.67_exp.csv"
fnames10 <- "./syntheticdataV2/ChangingScale_Power/Power_25979_2597.9_-0.213318655675175_0.5_exp.csv"

####### Outlier2 performs best
ffnames1<-"./syntheticdataV2/ChangingLocation_Linear/Linear_4546_909.2_-0.0119584849289948_0.75_exp.csv"
ffnames1<-"./syntheticdataV2/ChangingLocation_Linear/Linear_4546_909.2_-0.0119584849289948_0.75_exp.csv"
ffnames2<-"./syntheticdataV2/ChangingLocation_Partial_1/Partial1_4546_909.2_-0.0119584849289948_80_50_1000.33_0.67_exp.csv"
ffnames3<-"./syntheticdataV2/ChangingLocationScale_Linear/Linear_4546_909.2_-0.0119584849289948_0.75_5.85_exp.csv"
ffnames4<-"./syntheticdataV2/ChangingScale_Linear/Linear_12966_3889.8_-0.115845393337307_1_exp.csv"
ffnames5<-"./syntheticdataV2/ChangingScale_Partial/Partial_4546_454.6_-0.0119584849289948_80_0.25_0.75_exp.csv"






thresollds_1 <- c(30, 40, 50, 60, 70, 80, 90, 100)
tt=0
for (j in 1:10){
  ttt <- paste0("fnames", "=fnames",j)
  eval(parse(text=ttt))
  
  datanew1 <- read.csv(fnames)
  
  for (i in 1:length(thresollds_1)){
    tt=tt+1
    resall <- runALL(datanew1,thresollds_1[i],2)
    
    resulttemp <- evaluationmetric(resall)
    
    
    ## tags information
    tempp <- data.frame(count_outlier1 = sum(!is.na(resall$Outlier1)),
                        MAE_outlierI = resulttemp$mae_outlier1,
                        rivers = j,
                        threshold = thresollds_1[i])
    
    if(tt==1){
      resultsALL <- tempp
    }else{
      resultsALL <- bind_rows(resultsALL,tempp)
    }
  }
}
resultsALLriver<-resultsALL

tt=0
thresollds_2 <- c(2,3,4,5,6,7,8,9)
for (j in 1){
  ttt <- paste0("fnames", "=ffnames",j)
  eval(parse(text=ttt))
  
  datanew2 <- read.csv(fnames)
  
  for (i in 1:length(thresollds_2)){
    tt=tt+1
    resall <- runALL(datanew2,50,thresollds_2[i])
    
    resulttemp <- evaluationmetric(resall)
    
    
    ## tags information
    tempp <- data.frame(count_outlier2 = sum(!is.na(resall$Outlier2)),
                        MAE_outlierII = resulttemp$mae_outlier2,
                        rivers = j,
                        threshold = thresollds_2[i])
    
    if(tt==1){
      resultsALL2 <- tempp
    }else{
      resultsALL2 <- bind_rows(resultsALL2,tempp)
    }
  }
}



library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# Most basic bubble plot
plot2 <- resultsALL2 %>%
  arrange(desc(count_outlier2)) %>%
  ggplot(aes(x=threshold, y=MAE_outlierII, size = count_outlier2, fill=threshold)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_fill_viridis(discrete=FALSE, guide=FALSE, option="A") +
  ylab("MAE") +
  xlab("Threshold Value") +
  scale_size(range = c(4, 10), name="Number of outliers")+
  labs(subtitle = "Sensitivity analysis for Outlier removal method II ")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    plot.subtitle = element_text(size = 14)  # Adjust subtitle font size here
    
  )

resultsALL <- subset(resultsALLriver, rivers ==1)

# Most basic bubble plot
plot1 <-resultsALL %>%
  arrange(desc(count_outlier1)) %>%
  ggplot(aes(x=threshold, y=MAE_outlierI, size = count_outlier1, fill=threshold)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_fill_viridis(discrete=FALSE, guide=FALSE, option="A") +
  ylab("MAE") +
  xlab("Threshold Value") +
  scale_size(range = c(4, 10), name="Number of outliers")+
  labs(subtitle = "Sensitivity analysis for Outlier removal method I ")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    plot.subtitle = element_text(size = 14)  # Adjust subtitle font size here
    
  )
plot1

library(patchwork)

# Combine plots
combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)

# Display the combined plot
combined_plot




