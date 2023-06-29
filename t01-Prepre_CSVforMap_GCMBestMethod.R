##############
####Main code to run the code, getting the results_evaluation and pdfs.

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

# You need to change the working directory
pwd <- getwd()
setwd(pwd)

# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")





experiments <- c("EC-Earth3","GFDL-CM4","INM-CM4-8",
                 "IPSL-CM6A-LR","MIROC6","MPI-ESM1-2-HR",
                 "MRI-ESM2-0", "NorESM2-LM")

exp <- "EC-Earth3"

fnames <- paste0("./gcmdata/","allrivers_",exp, ".csv")
csvcurrentdata <- read.csv(fnames)
csvcurrentdata$X <- NULL

for (id in 1:47){
  anamx <- csvcurrentdata[,id]
  idriver <- paste0("id_",id)
  
  # 
  # Get the GEV location, scale, and shape
  
  fit_gev <- fevd(anamx)
  test<-fit_gev$results$par
  
  slope <- trendline_sum(seq(0,(166-1)),anamx,summary=TRUE)
  slope <- slope$parameter$a
  
  
  
  last30_data <- anamx[137:166]
  
  fit_gev2 <- fevd(anamx)
  test2<-fit_gev2$results$par
  
  slope2 <- trendline_sum(seq(0,(30-1)),last30_data,summary=TRUE)
  slope2 <- slope2$parameter$a
  
  
  resulttemp <- data.frame(id = id,
                        location = test[1],
                        scale = test[2],
                        shape =test[3],
                        slope = slope,
                        SlopeData = round(slope, digits=4),
                        StdData = sd(anamx),
                        
                        location_last30 =  test2[1],
                        scale_last30 = test2[2],
                        shape_last30 =test2[3],
                        SlopeData_last30 = round(slope2, digits=4),
                        StdData_last30 = sd(last30_data))
  
  if(id==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)}
}


rownames(resultsALL) <- NULL

gcm_refenrenced <- read_csv("~/Downloads/gcm_refenrenced.csv")

resultsALL <- merge(resultsALL,gcm_refenrenced,by="id")

write.csv(resultsALL,file=paste0("~/Downloads/gcmALLinfo.csv"),row.names = F)





breaks <- c(0, 76, 1950, 4546, 12966, 25979, Inf)
labels <- c("River.I", "River.II", "River.III", "River.IV", "River.V", "River.VI")

# Assign river names based on the ranges defined by breaks and labels
resultsALL$river <- cut(resultsALL$location, breaks = breaks, labels = labels, right = FALSE)


# Define the values for the new columns based on river names
mw_values <- c(11, 19, 15, 7, 13, 12)
mw1outlier1_values <- c(24, 27, 25, 34, 11, 39)
outlier1_values <- c(34, 24,23, 26, 26, 20)
outlier2_values <- c(32, 30, 38, 34, 50, 29)


# Define the values for the new columns based on river names GEVt
mw_values <- c(32, 42, 33, 36, 37, 34)
mw1outlier1_values <- c(29, 29, 34, 28, 27, 30)
outlier1_values <- c(9, 9,9, 9, 9, 8)
outlier2_values <- c(29, 20, 25, 26, 27, 27)



df <- gcmALLinfonew
# Create the new columns based on river names
df$GEVmw <- ifelse(df$river == "River.I", mw_values[1],
                ifelse(df$river == "River.II", mw_values[2],
                       ifelse(df$river == "River.III", mw_values[3],
                              ifelse(df$river == "River.IV", mw_values[4],
                                     ifelse(df$river == "River.V", mw_values[5], mw_values[6])))))

df$GEVmw1outlier1 <- ifelse(df$river == "River.I", mw1outlier1_values[1],
                         ifelse(df$river == "River.II", mw1outlier1_values[2],
                                ifelse(df$river == "River.III", mw1outlier1_values[3],
                                       ifelse(df$river == "River.IV", mw1outlier1_values[4],
                                              ifelse(df$river == "River.V", mw1outlier1_values[5], mw1outlier1_values[6])))))

df$GEVoutlier1 <- ifelse(df$river == "River.I", outlier1_values[1],
                      ifelse(df$river == "River.II", outlier1_values[2],
                             ifelse(df$river == "River.III", outlier1_values[3],
                                    ifelse(df$river == "River.IV", outlier1_values[4],
                                           ifelse(df$river == "River.V", outlier1_values[5], outlier1_values[6])))))

df$GEVoutlier2 <- ifelse(df$river == "River.I", outlier2_values[1],
                      ifelse(df$river == "River.II", outlier2_values[2],
                             ifelse(df$river == "River.III", outlier2_values[3],
                                    ifelse(df$river == "River.IV", outlier2_values[4],
                                           ifelse(df$river == "River.V", outlier2_values[5], outlier2_values[6])))))



write.csv(df,file=paste0("~/Downloads/gcmALLinfonew.csv"),row.names = F)


gcmALLinfonew$GEVmwnew <- gcmALLinfonew$GEVmw + gcmALLinfonew$GEVmw1outlier1
gcmALLinfonew$GEVoutlierInew <- gcmALLinfonew$GEVmw1outlier1 + gcmALLinfonew$GEVoutlier1
gcmALLinfonew$GEVoutlierIInew <- gcmALLinfonew$GEVoutlier2


gcmALLinfonew$GAMLSSmwnew <- gcmALLinfonew$mw + gcmALLinfonew$mw1outlier1
gcmALLinfonew$GAMLSSoutlierInew <- gcmALLinfonew$mw1outlier1 + gcmALLinfonew$outlier1
gcmALLinfonew$GAMLSSoutlierIInew <- gcmALLinfonew$outlier2

write.csv(gcmALLinfonew,file=paste0("~/Downloads/gcmALLinfonew.csv"),row.names = F)


riever1 <- subset(gcmALLinfonew, river == "River.I")
riever2 <- subset(gcmALLinfonew, river == "River.II")
riever3 <- subset(gcmALLinfonew, river == "River.III")
riever4 <- subset(gcmALLinfonew, river == "River.IV")
riever5 <- subset(gcmALLinfonew, river == "River.V")
riever6 <- subset(gcmALLinfonew, river == "River.VI")

write.csv(riever1,file=paste0("./gcmdata/BestMethodcsv/gcmALLinfonew1.csv"),row.names = F)
write.csv(riever2,file=paste0("./gcmdata/BestMethodcsv/gcmALLinfonew2.csv"),row.names = F)
write.csv(riever3,file=paste0("./gcmdata/BestMethodcsv/gcmALLinfonew3.csv"),row.names = F)
write.csv(riever4,file=paste0("./gcmdata/BestMethodcsv/gcmALLinfonew4.csv"),row.names = F)
write.csv(riever5,file=paste0("./gcmdata/BestMethodcsv/gcmALLinfonew5.csv"),row.names = F)
write.csv(riever6,file=paste0("./gcmdata/BestMethodcsv/gcmALLinfonew6.csv"),row.names = F)

