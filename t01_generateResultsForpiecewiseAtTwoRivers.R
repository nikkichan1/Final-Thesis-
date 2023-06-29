
##############
####This code is to generate the statitcial results with abrupt change points.
# for two rivers, and for further use.
# read the estimated floods in the directory of Results_100floods_location_picewiseII???
# save them as csv files.
# write.csv(resultsALL,file="./syntheticdataV2/RiverII_piecewise.csv",row.names = F)
# write.csv(resultsALL,file="./syntheticdataV2/RiverIII_piecewise.csv",row.names = F)

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


river1 <- list.files("./syntheticdataV2/Results_100floods_location_picewiseII/River1/",pattern = ".csv") 
river2 <- list.files("./syntheticdataV2/Results_100floods_location_picewiseII/River2/",pattern = ".csv") 



for (i in 1:length(river2)){
  
  fnames <- river2[i]
  datanew <- read.csv(paste0("./syntheticdataV2/Results_100floods_location_picewiseII/River2","/",fnames))
  

  
  pre_data <- datanew[30:79,]
  aft_data <- datanew[110:166,]
  
  
    
  resulttemp_pre <- evaluationmetric(pre_data)
  resulttemp_aft <- evaluationmetric(aft_data)
  
  ## tags information
  resulttemp <- data.frame(rivers = "River.II",
                           
                           MAE_mw_pre = round(resulttemp_pre$MAE_mw, digits=4),
                           MAE_mw_aft = round(resulttemp_aft$MAE_mw, digits=4),
                           
                           MAE_mwoutlier1_pre = round(resulttemp_pre$MAE_mwoutlier1, digits=4),
                           MAE_mwoutlier1_aft = round(resulttemp_aft$MAE_mwoutlier1, digits=4),
                           
                           MAE_mwoutlier2_pre = round(resulttemp_pre$MAE_mwoutlier2, digits=4),
                           MAE_mwoutlier2_aft = round(resulttemp_aft$MAE_mwoutlier2, digits=4),
                           
                           RMSE_mw_pre = round(resulttemp_pre$RMSE_mw, digits=4),
                           RMSE_mw_aft = round(resulttemp_aft$RMSE_mw, digits=4),
                           
                           RMSE_mwoutlier1_pre = round(resulttemp_pre$RMSE_mwoutlier1, digits=4),
                           RMSE_mwoutlier1_aft = round(resulttemp_aft$RMSE_mwoutlier1, digits=4),
                           
                           RMSE_mwoutlier2_pre = round(resulttemp_pre$RMSE_mwoutlier2, digits=4),
                           RMSE_mwoutlier2_aft = round(resulttemp_aft$RMSE_mwoutlier2, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

write.csv(resultsALL,file="./syntheticdataV2/RiverII_piecewise.csv",row.names = F)







####




river1 <- list.files("./syntheticdataV2/Results_100floods_location_picewiseII/River1/",pattern = ".csv") 
river2 <- list.files("./syntheticdataV2/Results_100floods_location_picewiseII/River2/",pattern = ".csv") 



for (i in 1:length(river1)){
  
  fnames <- river1[i]
  datanew <- read.csv(paste0("./syntheticdataV2/Results_100floods_location_picewiseII/River1","/",fnames))
  
  
  
  pre_data <- datanew[30:79,]
  aft_data <- datanew[110:166,]
  
  
  
  resulttemp_pre <- evaluationmetric(pre_data)
  resulttemp_aft <- evaluationmetric(aft_data)
  
  ## tags information
  resulttemp <- data.frame(rivers = "River.III",
                           
                           MAE_mw_pre = round(resulttemp_pre$MAE_mw, digits=4),
                           MAE_mw_aft = round(resulttemp_aft$MAE_mw, digits=4),
                           
                           MAE_mwoutlier1_pre = round(resulttemp_pre$MAE_mwoutlier1, digits=4),
                           MAE_mwoutlier1_aft = round(resulttemp_aft$MAE_mwoutlier1, digits=4),
                           
                           MAE_mwoutlier2_pre = round(resulttemp_pre$MAE_mwoutlier2, digits=4),
                           MAE_mwoutlier2_aft = round(resulttemp_aft$MAE_mwoutlier2, digits=4),
                           
                           RMSE_mw_pre = round(resulttemp_pre$RMSE_mw, digits=4),
                           RMSE_mw_aft = round(resulttemp_aft$RMSE_mw, digits=4),
                           
                           RMSE_mwoutlier1_pre = round(resulttemp_pre$RMSE_mwoutlier1, digits=4),
                           RMSE_mwoutlier1_aft = round(resulttemp_aft$RMSE_mwoutlier1, digits=4),
                           
                           RMSE_mwoutlier2_pre = round(resulttemp_pre$RMSE_mwoutlier2, digits=4),
                           RMSE_mwoutlier2_aft = round(resulttemp_aft$RMSE_mwoutlier2, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

write.csv(resultsALL,file="./syntheticdataV2/RiverIII_piecewise.csv",row.names = F)




