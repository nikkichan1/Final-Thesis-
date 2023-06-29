# s1: read the csv files in the directory of syntheticV2
# run the GAMLSS moving window methods (GAMLSS+GAMLSSMWoutlier1+GAMLSSMWoutlier2)
# Please note, we use the 1910-1939 to fit the GAMLSS.
# then evaluate the 1940-2015 simulated floods (to calculate the MAE)
# ---> save the metric results in the EvaluationResults_GAMLSSmethod1910-1939Fitted
# ----> save the simulated floods in the Results_100floods_GAMLSS
# ---> save the pngs in EvaluationResults_GAMLSSpngs
#

###Load package
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


mytheme <- theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, size = 10, hjust = 0.5, color = 'black'),
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.ticks = element_line(linetype = 1, color = 'black'),
    legend.text = element_text(size = 8),  # Adjust the legend text size
    axis.title = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    legend.title = element_blank(),
    axis.title.x=element_blank(),
    # legend.position = c(0.1, 0.03),  
    # Adjust the position of the legend to top left
    # Set the legend background
    legend.key.size = unit(0.15, "lines") 
    # Adjust the size of the legend key
  )


# You need to change the working directory
pwd <- getwd()
setwd(pwd)


# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")

#########################
# ##run all get the results
runALL <- function(csvdata){
  
  currentdata <- csvdata
  raw_data <- data.frame(year = seq(1850,2015),
                         maxVal = currentdata$random_numbers,
                         flow100 = currentdata$return_level)
  
  window_size <- 30
  return_periods <- c(30,50,100) # set the return periods
  
  
  
  # ##################
  # ####Using Movingwindow
  # dataTemp  <- raw_data
  # df_ini <- Discharge_at_GivenReturnPeriodandSamples(dataTemp,window_size,return_periods)
  # 
  # ##################
  # ####Using method 1.
  dataTemp_1  <- raw_data
  dataTemp_1 <- Reject_outliers_method1(dataTemp_1,threshold_returnPeriod=50,chunk=30)
  # dataTemp_1$r <- NULL
  # df_one <- Discharge_at_GivenReturnPeriodandSamples(dataTemp_1,window_size,return_periods)
  #
  # ##################
  # ####Using method 2.
  dataTemp_2  <- raw_data
  dataTemp_2 <- Reject_outliers_method2(dataTemp_2,window_size=30,threshold_times = 2)
  # dataTemp_3<-dataTemp_2
  # dataTemp_3$frequency <- NULL
  # df_two <- Discharge_at_GivenReturnPeriodandSamples(dataTemp_3,window_size,return_periods)

  
  ##################
  ####Using GAMLSS methods. ?? Please note using Non-stationary or Both
  data_second <- GamlssFigure1Data(raw_data,baseyear = 1950)
  
  
  
  ##################
  ####Using GAMLSS methods only 30 years.
  newraw <- raw_data
  # 1930-1959
  newraw$maxVal[newraw$year < 1910 | newraw$year > 1939] <- NA
  data_secondnew <- GamlssFigure1Data(newraw,baseyear = 1915)
  
  
  ##################
  ####Using GAMLSS methods only 30 years with outlier removal I and II
  ### find the locations of NAs in the    dataTemp_2$maxVal dataTemp_1$maxVal
  na_index1 <- which(is.na(dataTemp_1$maxVal))
  na_index2 <- which(is.na(dataTemp_2$maxVal))
  
  newraw2 <- newraw
  newraw1 <- newraw
  
  newraw1$maxVal[na_index1] <- NA
  newraw2$maxVal[na_index2] <- NA
  
  
  data_secondnew1 <- GamlssFigure1Data(newraw1,baseyear = 1915)
  data_secondnew2 <- GamlssFigure1Data(newraw2,baseyear = 1915)
  
  
  
  
  ##################
  ####combine the  data
  na_indices <- which(is.na(dataTemp_1$maxVal))
  outlier_raw_1<- raw_data[na_indices, ]
  na_indices <- which(is.na(dataTemp_2$maxVal))
  outlier_raw_2<- raw_data[na_indices, ]
  
  
  
  
  # mw<-df_ini$return_100
  gamlass<-data_second$flood_magnitude_100yr
  # mw_outlier1<-df_one$return_100
  # mw_outlier2<-df_two$return_100
  flow_100<-raw_data$flow100
  gamlss_30 <- data_secondnew$flood_magnitude_100yr
  gamlss_30outlier1 <- data_secondnew1$flood_magnitude_100yr
  gamlss_30outlier2 <- data_secondnew2$flood_magnitude_100yr
  
  
  
  
  # mwdf<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw)
  gamlassdf<-data.frame(raw_data$year, gamlass)
  # origindf<-data.frame(raw_data$year, flow_100)
  # outlierdf_1<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw_outlier1)
  # outlierdf_2<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw_outlier2)
  gamlassdf_30<-data.frame(raw_data$year, gamlss_30)
  gamlassdf_30outlier1<-data.frame(raw_data$year, gamlss_30outlier1)
  gamlassdf_30outlier2<-data.frame(raw_data$year, gamlss_30outlier2)
  
  
  
  outlier_1<-data.frame(outlier_raw_1$year, outlier_raw_1$maxVal)
  outlier_2<-data.frame(outlier_raw_2$year, outlier_raw_2$maxVal)
  
  
  
  # names(mwdf)<-c("year","mw_100")
  names(gamlassdf)<-c("year","gamlass_100")
  names(gamlassdf_30)<-c("year","gamlass30_100")
  names(gamlassdf_30outlier1)<-c("year","gamlass30outlier1_100")
  names(gamlassdf_30outlier2)<-c("year","gamlass30outlier2_100")
  # names(origindf)<-c("year","orgin_100")
  # names(outlierdf_1)<-c("year","outlier1_100")
  # names(outlierdf_2)<-c("year","outlier2_100")
  
  names(outlier_1)<-c("year","outlier_1")
  names(outlier_2)<-c("year","outlier_2")
  
  
  # reswl_all<- merge(gamlassdf,mwdf, by="year", all=TRUE)
  reswl_all<- merge(gamlassdf_30,gamlassdf, by="year", all=TRUE)
  reswl_all<- merge(gamlassdf_30outlier1,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(gamlassdf_30outlier2,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(raw_data,reswl_all, by="year", all=TRUE)
  # reswl_all<- merge(origindf,reswl_all, by="year", all=TRUE)
  # reswl_all<- merge(reswl_all,outlierdf_1,by="year", all=TRUE)
  # reswl_all<- merge(reswl_all,outlierdf_2,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlier_1,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlier_2,by="year", all=TRUE)
  
  
  
  reswl_all2 <- reswl_all[,c("year","maxVal","outlier_1","outlier_2","flow100",
                             "gamlass_100","gamlass30_100","gamlass30outlier1_100","gamlass30outlier2_100")]
  names(reswl_all2) <- c("year","Synthetic data","Outliers I","Outliers II","Truth",
                         "GAMLSS ALL","GAMLSS MW","GAMLSS MW Outlier I","GAMLSS MW Outlier II")
  
  return(reswl_all2)
}


########################
#### Main loop the csv files.

exp1_linear <- list.files("./syntheticdataV2/ChangingLocation_Linear/",pattern = ".csv") 
exp1_power <- list.files("./syntheticdataV2/ChangingLocation_Power/",pattern = ".csv") 
exp1_partial <- list.files("./syntheticdataV2/ChangingLocation_Partial/",pattern = ".csv") 
exp1_partial1 <- list.files("./syntheticdataV2/ChangingLocation_Partial_1/",pattern = ".csv") 


exp2_linear <- list.files("./syntheticdataV2/ChangingScale_Linear/",pattern = ".csv") 
exp2_power <- list.files("./syntheticdataV2/ChangingScale_Power/",pattern = ".csv") 
exp2_partial <- list.files("./syntheticdataV2/ChangingScale_Partial/",pattern = ".csv") 
exp2_partial1 <- list.files("./syntheticdataV2/ChangingScale_Partial_1/",pattern = ".csv") 


exp3_linear <- list.files("./syntheticdataV2/ChangingLocationScale_Linear/",pattern = ".csv") 
exp3_power <- list.files("./syntheticdataV2/ChangingLocationScale_Power/",pattern = ".csv") 
exp3_partial <- list.files("./syntheticdataV2/ChangingLocationScale_Partial/",pattern = ".csv")
exp3_partial1 <- list.files("./syntheticdataV2/ChangingLocationScale_Partial_1/",pattern = ".csv")





######
# You need to change some parameters, here

filesALL <- c("exp1_linear","exp1_power","exp1_partial","exp1_partial1","exp2_linear","exp2_power",
              "exp2_partial","exp2_partial1","exp3_linear","exp3_power","exp3_partial","exp3_partial1")

# 
for (iii in 1:length(filesALL)){
  tt=0
  if(iii==1){
    filesName <- exp1_linear
    LocationName <- "ChangingLocation_Linear"
    tagName <- "ChangingLocation"
    outputName <- "Evaluation_exp1_linear"
    functionName <- "Linear"
  }else if (iii==2){
    filesName <- exp1_power
    LocationName <- "ChangingLocation_Power"
    tagName <- "ChangingLocation"
    outputName <- "Evaluation_exp1_power"
    functionName <- "Quadratic"
  }else if (iii==3){
    filesName <- exp1_partial
    LocationName <- "ChangingLocation_Partial"
    tagName <- "ChangingLocation"
    outputName <- "Evaluation_exp1_partial"
    functionName <- "Piecewise"
    
  }else if (iii==4){
    filesName <- exp1_partial1
    LocationName <- "ChangingLocation_Partial_1"
    tagName <- "ChangingLocation"
    outputName <- "Evaluation_exp1_partial1"
    functionName <- "Piecewise"
    
  }else if (iii==5){
    filesName <- exp2_linear
    LocationName <- "ChangingScale_Linear"
    tagName <- "ChangingScale"
    outputName <- "Evaluation_exp2_linear"
    functionName <- "Linear"
    
  }else if (iii==6){
    filesName <- exp2_power
    LocationName <- "ChangingScale_Power"
    tagName <- "ChangingScale"
    outputName <- "Evaluation_exp2_power"
    functionName <- "Quadratic"
    
  }else if (iii==7){
    filesName <- exp2_partial
    LocationName <- "ChangingScale_Partial"
    tagName <- "ChangingScale"
    outputName <- "Evaluation_exp2_partial"
    functionName <- "Piecewise"
    
  }else if (iii==8){
    filesName <- exp2_partial1
    LocationName <- "ChangingScale_Partial_1"
    tagName <- "ChangingScale"
    outputName <- "Evaluation_exp2_partial1"
    functionName <- "Piecewise"
    
  }else if (iii==9){
    filesName <- exp3_linear
    LocationName <- "ChangingLocationScale_Linear"
    tagName <- "ChangingLocationScale"
    outputName <- "Evaluation_exp3_linear"
    functionName <- "Linear"
    
  }else if (iii==10){
    filesName <- exp3_power
    LocationName <- "ChangingLocationScale_Power"
    tagName <- "ChangingLocationScale"
    outputName <- "Evaluation_exp3_power"
    functionName <- "Quadratic"
    
  }else if (iii==11){
    filesName <- exp3_partial
    LocationName <- "ChangingLocationScale_Partial"
    tagName <- "ChangingLocationScale"
    outputName <- "Evaluation_exp3_partial"
    functionName <- "Piecewise"
    
  }else if (iii==12){
    filesName <- exp3_partial1
    LocationName <- "ChangingLocationScale_Partial_1"
    tagName <- "ChangingLocationScale"
    outputName <- "Evaluation_exp3_partial1"
    functionName <- "Piecewise"
    
  }
  
  
  for (i in 52:length(filesName)){
    
    fnames <- filesName[i]
    datanew <- read.csv(paste0("./syntheticdataV2/",LocationName,"/",fnames))
    
    tt=tt+1
    tryCatch({
      
      reswl_all2 <- runALL(datanew)
      
      ### 
      write.csv(reswl_all2,file=paste0("./syntheticdataV2/Results_100floods_GAMLSS/",LocationName, fnames),row.names = F)
      
      resall <- reswl_all2[91:nrow(reswl_all2),]
      
      
      resulttemp <- evaluationmetricGAMLSS(resall)
      
      data_melt2 <- melt(reswl_all2,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')
      
      metrics_text <- paste("MAE_GAMLSS ALL:", resulttemp$MAE_gamlss, "\n",
                            "MAE_GAMLSS MW:", resulttemp$MAE_gamlssMW, "\n",
                            "MAE_GAMLSS MW OutlierI:", resulttemp$MAE_gamlssMWOutlier1, "\n",
                            "MAE_GAMLSS MW OutlierII:", resulttemp$MAE_gamlssMWOutlier2, "\n")
      
      
      
      # Create the ggplot object
      # Create the ggplot object
      pll <- ggplot(data_melt2, aes(x = year, y = waterlevel, colour = DataSouces, linetype = DataSouces, shape = DataSouces)) +
        mytheme +
        scale_color_manual("waterlevel",
                           values = c("Synthetic data" = "gray80",
                                      "Outliers I" = "#e8490f",
                                      "Outliers II" = "#e4ce00",
                                      "Truth" = "black",
                                      "GAMLSS ALL" = "blue",
                                      "GAMLSS MW" = "#f18800",
                                      "GAMLSS MW Outlier I" = "#9ec417",
                                      "GAMLSS MW Outlier II" = "#44c1f0")) +
        scale_shape_manual("waterlevel", values = c(16, 16, 16, NA, NA, NA, NA, NA)) +
        scale_linetype_manual("waterlevel", values = c(0, 0, 0, 1, 1, 1, 1, 1)) +
        labs(y = expression(paste("AMAX [m"^3 * "/s]"))) +
        labs(x = "") +
        # scale_y_log10() +  # Set y-axis to log scale
        scale_x_continuous(breaks = seq(1910, 2015, by = 15), limits = c(1910, 2015)) +
        theme(legend.position = "bottom")+
        guides(color = guide_legend(nrow = 2, ncol = 4))  # Adjust the legend to have 1 column
      
      
      # Add shaded area
      pll <- pll +
        geom_rect(xmin = 1910, xmax = 1939, ymin = -Inf, ymax = Inf, fill = "gray", color = "NA", alpha = 0.05)
      
      # Add lines and points
      pll <- pll +
        geom_line(size = 1) +
        geom_point(alpha = 1, size = 2)
      
      # Determine the range of y values
      yrange <- range(data_melt2$waterlevel, na.rm = TRUE)
      
      # Calculate a suitable position for the text annotation
      ypos <- yrange[1] + 0.1 * diff(yrange)
      
      # Add text annotation
      pll <- pll +
        annotate("text", x = 1910, y = ypos, label = metrics_text, hjust = 0, size = 3)
      
      # Display the plot
      pll
      
    
      
      ggsave(filename=paste0(tagName,'_',i,".png",sep=""),path=paste0('./syntheticdataV2/EvaluationResults_GAMLSSpngs/',LocationName))
      
      
      ## tags information
      resulttemp$experiment <- tagName
      resulttemp$fname <- fnames
      resulttemp$functions <- functionName
      
      
      
      if(tt==1){
        resultsALL <- resulttemp
      }else{
        resultsALL <- bind_rows(resultsALL,resulttemp)
      }
      
      
      
    }, error = function(e) {
      # Handle the error (optional)
      print(paste("Error occurred for element", i, ":"))
      print(e)
      
      # Continue to the next iteration
      return()
    })
  }
  
  write.csv(resultsALL,file=paste0("./syntheticdataV2/EvaluationResults_GAMLSSmethod1910-1939Fitted/",outputName,
                                   ".csv"),row.names = F)
  
  
}

tt


