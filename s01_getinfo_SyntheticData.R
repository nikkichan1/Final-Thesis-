##############
####To get the information of synthetic data.
# extract the fname, slope, bmax, b, loc, scale, function etc attributes of
# synthetic data,
# save them in the directory of ./syntheticdataV2/info_syntheticData
#
###############
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
library(ggtrendline)


#change the working directory
pwd <- getwd()
setwd(pwd)



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




for (i in 1:length(exp3_linear)){
  fnames <- exp3_linear[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames))
  
  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5]) # b of bmax
  variable5 <- as.numeric(name_parts[6]) # bmax.or amax of scale
  
  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocationScale",
                           fname = fnames,
                           Function   = "Linear",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           bmaxcoefficient1 = round(variable1*0.7/165,digits = 2),
                           bmaxcoefficient2 = round(mean(datanew$location)*0.2/165,digits = 2),
                           bcoefficient = variable4,
                           SlopeLocation = round(variable1*0.7/165*variable4,digits = 3),
                           SlopeScale = round(mean(datanew$location)*0.2/165*variable4,digits = 3),
                           SlopeData = round(slope, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}
exp3_linear_result <- resultsALL




for (i in 1:length(exp3_power)){
  fnames <- exp3_power[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocationScale_Power/",fnames))
  
  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])
  variable5 <- as.numeric(name_parts[6])
  
  
  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocationScale",
                           fname = fnames,
                           Function   = "Power",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           amaxcoefficient1 = round(variable1*0.7/165/165,digits = 5),
                           amaxcoefficient2 = round(mean(datanew$location)*0.2/165/165,digits = 5),
                           acoefficient = variable4,
                           SlopeLocation = round(variable1*0.7/165/165*variable4,digits = 5),
                           SlopeScale = round(mean(datanew$location)*0.2/165/165*variable4,digits = 5),
                           SlopeData = round(slope, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

exp3_power_result <- resultsALL



for (i in 1:length(exp3_partial)){
  fnames <- exp3_partial[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocationScale_Partial/",fnames))
  
  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])
  variable5 <- as.numeric(name_parts[6])
  variable6 <- as.numeric(name_parts[7])
  
  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocationScale",
                           fname = fnames,
                           Function   = "Partial",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           changpointlocation = variable4,
                           bmaxcoefficient1 = round(variable1*0.7/165,digits = 5),
                           bmaxcoefficient2 = round(mean(datanew$location)*0.2/165,digits = 5),
                           b1coefficient = variable5,
                           b2coefficient = variable6,
                           SlopeLocation1 = round(variable1*0.7/165*variable5,digits = 5),
                           SlopeLocation2 = round(variable1*0.7/165*variable6,digits = 5),
                           SlopeScale1 = round(mean(datanew$location)*0.2/165*variable5,digits = 5),
                           SlopeScale2 = round(mean(datanew$location)*0.2/165*variable6,digits = 5),
                           SlopeData = round(slope, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

exp3_partial_result <- resultsALL






for (i in 1:length(exp2_linear)){

  fnames <- exp2_linear[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingScale_Linear/",fnames))


  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a



  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]

  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])

  ## tags information
  resulttemp <- data.frame(experiment = "ChangingScale",
                           fname = fnames,
                           Function   = "Linear",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           bmaxcoefficient = round(variable2*0.2/165,digits = 2),
                           bcoefficient = variable4,
                           SlopeScale = round(variable2*0.2/165*variable4,digits = 3),
                           SlopeData = round(slope, digits=4))


  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}
exp2_linear_result <- resultsALL




for (i in 1:length(exp2_power)){
  fnames <- exp2_power[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingScale_Power/",fnames))

  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a

  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]

  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])

  ## tags information
  resulttemp <- data.frame(experiment = "ChangingScale",
                           fname = fnames,
                           Function   = "Power",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           amaxcoefficient = round(variable2*0.2/165/165,digits = 5),
                           acoefficient = variable4,
                           SlopeScale = round(variable2*0.2/165/165*variable4,digits = 5),
                           SlopeData = round(slope, digits=4))


  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

exp2_power_result <- resultsALL



for (i in 1:length(exp2_partial)){
  fnames <- exp2_partial[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingScale_Partial/",fnames))

  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a

  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]

  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])
  variable5 <- as.numeric(name_parts[6])
  variable6 <- as.numeric(name_parts[7])

  ## tags information
  resulttemp <- data.frame(experiment = "ChangingScale",
                           fname = fnames,
                           Function   = "Partial",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           changpointlocation = variable4,
                           bmaxcoefficient = round(variable2*0.2/165,digits = 5),
                           b1coefficient = variable5,
                           b2coefficient = variable6,
                           SlopeScale1 = round(variable2*0.2/165*variable5,digits = 5),
                           SlopeScale2 = round(variable2*0.2/165*variable6,digits = 5),
                           SlopeData = round(slope, digits=4))


  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

exp2_partial_result <- resultsALL





for (i in 1:length(exp2_partial1)){
  fnames <- exp2_partial1[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingScale_Partial_1/",fnames))
  
  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])

  
  ## tags information
  resulttemp <- data.frame(experiment = "ChangingScale",
                           fname = fnames,
                           Function   = "Partial",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           changpointlocation_1 = 50,
                           changpointlocation_2 = 100,
                           bmaxcoefficient = round(variable2*0.2/165,digits = 5),
                           SlopeData = round(slope, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

exp2_partial1_result <- resultsALL



for (i in 1:length(exp1_linear)){
  fnames <- exp1_linear[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocation_Linear/",fnames))


  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a

  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]

  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])

  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocation",
                           fname = fnames,
                           Function   = "Linear",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           bmaxcoefficient = round(variable1*0.7/165,digits = 2),
                           bcoefficient = variable4,
                           SlopeLocation = round(variable1*0.7/165*variable4,digits = 2),
                           SlopeData = round(slope, digits=4))


  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}
exp1_linear_result <- resultsALL






for (i in 1:length(exp1_power)){
  fnames <- exp1_power[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocation_Power/",fnames))

  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a

  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]

  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])

  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocation",
                           fname = fnames,
                           Function   = "Power",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           amaxcoefficient = round(variable1*0.7/165/165,digits = 5),
                           acoefficient = variable4,
                           SlopeLocation = round(variable1*0.7/165/165*variable4,digits = 5),
                           SlopeData = round(slope, digits=4))


  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}
exp1_power_result <- resultsALL






for (i in 1:length(exp1_partial)){
  fnames <- exp1_partial[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocation_Partial/",fnames))

  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a

  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]

  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])
  variable5 <- as.numeric(name_parts[6])
  variable6 <- as.numeric(name_parts[7])

  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocation",
                           fname = fnames,
                           Function   = "Partial",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           changpointlocation = variable4,
                           bmaxcoefficient = round(variable1*0.7/165,digits = 5),
                           b1coefficient = variable5,
                           b2coefficient = variable6,
                           SlopeLocation1 = round(variable1*0.7/165*variable5,digits = 5),
                           SlopeLocation2 = round(variable1*0.7/165*variable6,digits = 5),
                           SlopeData = round(slope, digits=4))


  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}
exp1_partial_result <- resultsALL




for (i in 1:length(exp1_partial1)){
  fnames <- exp1_partial1[i]
  datanew <- read.csv(paste0("./syntheticdataV2/ChangingLocation_Partial_1/",fnames))
  
  slope <- trendline_sum(seq(0,165),datanew$random_numbers,summary=TRUE)
  slope <- slope$parameter$a
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  variable1 <- as.numeric(name_parts[2])
  variable2 <- as.numeric(name_parts[3])
  variable3 <- as.numeric(name_parts[4])
  variable4 <- as.numeric(name_parts[5])
  
  ## tags information
  resulttemp <- data.frame(experiment = "ChangingLocation",
                           fname = fnames,
                           Function   = "Partial",
                           initiallocation = variable1,
                           intialscale = variable2,
                           initialshape = variable3,
                           changpointlocation_1 = 50,
                           changpointlocation_2 = 100,
                           bmaxcoefficient = round(variable1*0.7/165,digits = 5),
                           SlopeData = round(slope, digits=4))
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}
exp1_partial1_result <- resultsALL






write.csv(exp1_linear_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp1_linear_result.csv"),row.names = F)
write.csv(exp1_power_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp1_power_result.csv"),row.names = F)
write.csv(exp1_partial_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp1_partial_result.csv"),row.names = F)
write.csv(exp1_partial1_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp1_partial1_result.csv"),row.names = F)



write.csv(exp2_linear_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp2_linear_result.csv"),row.names = F)
write.csv(exp2_power_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp2_power_result.csv"),row.names = F)
write.csv(exp2_partial_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp2_partial_result.csv"),row.names = F)
write.csv(exp2_partial1_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp2_partial1_result.csv"),row.names = F)


write.csv(exp3_linear_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp3_linear_result.csv"),row.names = F)
write.csv(exp3_power_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp3_power_result.csv"),row.names = F)
write.csv(exp3_partial_result,file=paste0("./syntheticdataV2/info_syntheticData/info_exp3_partial_result.csv"),row.names = F)






