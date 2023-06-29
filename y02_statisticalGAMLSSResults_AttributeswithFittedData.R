##############
####Main code to run the code, getting the statistical results
# Please Note. We used the GAMLSS method for estimate 100yr floods 
# 1910-1939 to fit the GAMLSS 1940-2015 to predict
# the estimate floods are saved in the Results_100floods_GAMLSS
# Then, we 
# to calculate the slope, location, std, and find the best method for each data point
# for fitted data and prediect data, respectively.
# ------> save them in the ./syntheticdataV2/GAMLSS_statiscal.csv
# somthing wrong for predicted, we should calcualte the slope, location, std of prediected data,
# rather than still using the fitted data!!!!!/ maybe we can skip the figure.
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
library(ggtrendline)




# You need to change the working directory
pwd <- getwd()
setwd(pwd)



# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")



stat_results <- list.files("./syntheticdataV2/Results_100floods_GAMLSS/",pattern = ".csv") 


window_size<-30


riversid <- c(76, 1950, 4546, 12966, 25979, 32163)
rivers <- c("River.I","River.II","River.III","River.IV","River.V","River.VI")

functionid <- c("LinearLinear", "PowerPower", "PartialPartial", "Partial1Partial1")
funtions <- c("Linear","Quadratic","Piecewise","Piecewise")

ttt=0


for (i in 1:length(stat_results)){
  
  fnames <- stat_results[i]
  datanew <- read.csv(paste0("./syntheticdataV2/Results_100floods_GAMLSS/",fnames))
  
  
  # Split the file name by underscores
  name_parts <- strsplit(fnames, "_")[[1]]
  
  # Extract the values and assign them to variables
  experiment = name_parts[1]
  functios = name_parts[2]
  
  variable1 <- as.numeric(name_parts[3])
  
  index = which(riversid==variable1)
  
  index2 = which(functionid==functios)
  

  
  # extarct the data from 1910-1930
  current_data <- datanew[61:(61+window_size-1), , drop=FALSE]
  
  slope <- trendline_sum(seq(0,(window_size-1)),current_data$Synthetic.data,summary=TRUE)
  slope <- slope$parameter$a
  
  # fitted data and evaluated data
  fitteddata <- current_data
  predicteddata <-  datanew[(61+window_size-1+1):nrow(datanew), , drop=FALSE]
  
  # Get the MAE and RMSE value.
  names(fitteddata) <- c("year","Synthetic data","Outliers I","Outliers II","Truth",
                         "GAMLSS ALL","GAMLSS MW","GAMLSS MW Outlier I","GAMLSS MW Outlier II")
  names(predicteddata) <- c("year","Synthetic data","Outliers I","Outliers II","Truth",
                         "GAMLSS ALL","GAMLSS MW","GAMLSS MW Outlier I","GAMLSS MW Outlier II")
  result_fitted <- evaluationmetricGAMLSS(fitteddata)
  result_predicted <- evaluationmetricGAMLSS(predicteddata)
  
  
  find_best_method <- function(rmsegamlssmw, rmsegamlssmwoutlierI, rmsegamlssoutlierII, truth) {
    diffs <- abs(c(GAMLSS.MW = rmsegamlssmw, GAMLSS.MW.Outlier.I = rmsegamlssmwoutlierI,
                   GAMLSS.MW.Outlier.II = rmsegamlssoutlierII) - truth)
    min_diff <- min(diffs, na.rm = TRUE)
    best_methods <- names(diffs)[diffs == min_diff]
    return(paste(best_methods, collapse=" and "))
  }

  # result_fitted <- result_fitted %>%
  #   mutate(bestMethod = mapply(find_best_method, RMSE_gamlssMW, RMSE_gamlssMWOutlier1, RMSE_gamlssMWOutlier2, 0))
  # result_predicted <- result_predicted %>%
  #   mutate(bestMethod = mapply(find_best_method, RMSE_gamlssMW, RMSE_gamlssMWOutlier1, RMSE_gamlssMWOutlier2, 0))
  # 
  result_fitted <- result_fitted %>%
    mutate(bestMethod = mapply(find_best_method, MAE_gamlssMW, MAE_gamlssMWOutlier1, MAE_gamlssMWOutlier2, 0))
  result_predicted <- result_predicted %>%
    mutate(bestMethod = mapply(find_best_method, MAE_gamlssMW, MAE_gamlssMWOutlier1, MAE_gamlssMWOutlier2, 0))
  
  
  
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
                           count_outlier1 = sum(!is.na(current_data$Outliers.I)),
                           count_outlier2 = sum(!is.na(current_data$Outliers.II)),
                           SlopeData = round(slope, digits=4),
                           StdData = sd(current_data$Synthetic.data),
                           bestMethod_fitted = result_fitted$bestMethod,
                           bestMethod_predicted = result_predicted$bestMethod)
  
  
  if(i==1){
    resultsALL <- resulttemp
  }else{
    resultsALL <- bind_rows(resultsALL,resulttemp)
  }
}

rownames(resultsALL) <- NULL


write.csv(resultsALL,file=paste0("./syntheticdataV2/GAMLSS_statiscal.csv"),row.names = F)


