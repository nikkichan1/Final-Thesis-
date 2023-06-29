##############
####Main code to run the code, getting the statistical results
# Please Note. We used the GEV method for estimate 100yr floods for each year and
# save them in the directory of Results_100floods_GEV
# Then, we can read these estiamted 100yr flood for each window!! and can get the
# statitical metrics for each invidual window. That's to say, performance of GEV method
# only using 30yr data.
# We also extract various attribuets of 30yr data, such as slope, std, riverType, 
# locayion, scale, and shape etc. and MAE RMSE for each method
# ------> save them in the ./syntheticdataV2/GEV30yr_statiscal.csv

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



# You need to change the working directory
pwd <- getwd()
setwd(pwd)


# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")



stat_results <- list.files("./syntheticdataV2/Results_100floods_GEV/",pattern = ".csv") 

window_size<-30


riversid <- c(76, 1950, 4546, 12966, 25979, 32163)
rivers <- c("River.I","River.II","River.III","River.IV","River.V","River.VI")

functionid <- c("LinearLinear", "PowerPower", "PartialPartial", "Partial1Partial1")
funtions <- c("Linear","Quadratic","Piecewise","Piecewise")

ttt=0
for (i in 1:length(stat_results)){
  
  fnames <- stat_results[i]
  datanew <- read.csv(paste0("./syntheticdataV2/Results_100floods_GEV/",fnames))
  
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  experiment = name_parts[1]
  functios = name_parts[2]
  
  variable1 <- as.numeric(name_parts[3])
  
  index = which(riversid==variable1)
  
  index2 = which(functionid==functios)
  
  find_best_method <- function(mw, mw_outlier_I, mw_outlier_II, truth) {
    diffs <- abs(c(MW = mw, MWOutlier.I = mw_outlier_I, MWOutlier.II = mw_outlier_II) - truth)
    min_diff <- min(diffs, na.rm = TRUE)
    best_methods <- names(diffs)[diffs == min_diff]
    return(paste(best_methods, collapse=" and "))
  }
  
  datanew <- datanew %>%
    mutate(bestMethod = mapply(find_best_method, MW, MWOutlier.I, MWOutlier.II, Truth))
  
  
  for (idd in 1:(nrow(datanew)-window_size+1)){
    ttt=ttt+1
    current_data <- datanew[idd:(idd+window_size-1), , drop=FALSE]
    
    slope <- trendline_sum(seq(0,(window_size-1)),current_data$Synthetic.data,summary=TRUE)
    slope <- slope$parameter$a
    
    
    
    
    fit_gev <- fevd(current_data$Synthetic.data)
    test<-fit_gev$results$par
    
    ## tags information
    resulttemp <- data.frame(fname = fnames,
                             experiment = experiment,
                             Function   = funtions[index2],
                             
                             location = test[1],
                             scale = test[2],
                             shape = test[3],
                             
                             rivers = rivers[index],
                             year = current_data$year[window_size],
                             count_outlier1 = sum(!is.na(current_data$Outliers.I)),
                             count_outlier2 = sum(!is.na(current_data$Outliers.II)),
                             SlopeData = round(slope, digits=4),
                             StdData = sd(current_data$Synthetic.data),
                             bestMethod = current_data$bestMethod[window_size])
    
    
    if(ttt==1){
      resultsALL <- resulttemp
    }else{
      resultsALL <- bind_rows(resultsALL,resulttemp)
    }
  
  }
}

rownames(resultsALL) <- NULL


write.csv(resultsALL,file=paste0("./syntheticdataV2/GEV30yr_statiscal.csv"),row.names = F)


