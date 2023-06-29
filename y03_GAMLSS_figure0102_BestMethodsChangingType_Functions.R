##############
####Main code to run the code, getting the pngs
# 
# select some csv files to illustrate the best GAMLSS Method in different situations
# Two figures are saved!!
# gamlss-f01-BestMethod-ChangingTypes
# gamlss-f01-BestMethod-Functions
# in the directory of ./figures/GAMLSS_results

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
                         "GAMLSS ALL","GAMLSS 30yr","GAMLSS 30yr Outlier I","GAMLSS 30yr Outlier II")
  
  return(reswl_all2)
}


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



#### figure 1         GAMLSS MW,       GAMLSS MW Outlier I,    GAMLSS MW Outlier II
# changingLocation:   fname1               fname2                   fname3
# changingScale:      fname4               fname5                   fname6
# changingBoth:       fname7               fname8                   fname9

fnames <- exp1_linear[14]
fnames1  <- paste0("./syntheticdataV2/ChangingLocation_Linear/",fnames)
fnames <- exp1_partial1[8]
fnames2  <- paste0("./syntheticdataV2/ChangingLocation_Partial_1/",fnames)
fnames <- exp1_partial[14]
fnames3  <- paste0("./syntheticdataV2/ChangingLocation_Partial/",fnames)


fnames <- exp2_partial1[2]
fnames4  <- paste0("./syntheticdataV2/ChangingScale_Partial_1/",fnames)
fnames <- exp2_partial1[41]
fnames5  <- paste0("./syntheticdataV2/ChangingScale_Partial_1/",fnames)
fnames <- exp2_partial[47]
fnames6  <- paste0("./syntheticdataV2/ChangingScale_Partial/",fnames)


fnames <- exp3_linear[12]
fnames7  <- paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames)
fnames <- exp3_linear[10]
fnames8  <- paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames)
fnames <- exp3_linear[7]
fnames9  <- paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames)




#### figure 1         GAMLSS MW,       GAMLSS MW Outlier I,    GAMLSS MW Outlier II
# Linear:              fname1               fname2                   fname3
# Power:               fname4               fname5                   fname6
# Piecewise:           fname7               fname8                   fname9

fnames <- exp3_linear[12]
fnames1  <- paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames)
fnames <- exp3_linear[16]
fnames2  <- paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames)
fnames <- exp3_linear[7]
fnames3  <- paste0("./syntheticdataV2/ChangingLocationScale_Linear/",fnames)



fnames <- exp3_power[45]
fnames4  <- paste0("./syntheticdataV2/ChangingLocationScale_Power/",fnames)
fnames <- exp3_power[22]
fnames5  <- paste0("./syntheticdataV2/ChangingLocationScale_Power/",fnames)
fnames <- exp3_power[48]
fnames6  <- paste0("./syntheticdataV2/ChangingLocationScale_Power/",fnames)


fnames <- exp3_partial[53]
fnames7  <- paste0("./syntheticdataV2/ChangingLocationScale_Partial/",fnames)
fnames <- exp1_partial[12]
fnames8  <- paste0("./syntheticdataV2/ChangingLocation_Partial/",fnames)
fnames <- exp1_partial[14]
fnames9  <- paste0("./syntheticdataV2/ChangingLocation_Partial/",fnames)







for (i in 1:9){
  
  ttt <- paste0("newfname", "=fnames",i)
  eval(parse(text=ttt))
  
  
  datanew <- read.csv(newfname)
  
  
  reswl_all2 <- runALL(datanew)
  
  resall <- reswl_all2[91:nrow(reswl_all2),]
  
  
  resulttemp <- evaluationmetricGAMLSS(resall)
  
  data_melt2 <- melt(reswl_all2,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')
  
  metrics_text <- paste("MAE_GAMLSS ALL:", resulttemp$MAE_gamlss, "\n",
                        "MAE_GAMLSS 30yr:", resulttemp$MAE_gamlssMW, "\n",
                        "MAE_GAMLSS 30yr OutlierI:", resulttemp$MAE_gamlssMWOutlier1, "\n",
                        "MAE_GAMLSS 30yr OutlierII:", resulttemp$MAE_gamlssMWOutlier2, "\n")
  
  
  
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
                                  "GAMLSS 30yr" = "#f18800",
                                  "GAMLSS 30yr Outlier I" = "#9ec417",
                                  "GAMLSS 30yr Outlier II" = "#44c1f0")) +
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
  
  
  
  ttt <- paste0("a",i, "=pll")
  eval(parse(text=ttt))
}





pdfName <- "gamlss-f01-BestMethod-ChangingTypes" 

pdf(file = paste0('./figures/GAMLSS_results/',pdfName,'.pdf',sep = ""),  width = 15.9, height = 9.8)

ggdraw() +
  draw_plot(p1, x = 0, y = .6, width = .33, height = .3)+
  draw_plot(p2, x = 0.33, y = .6, width = .33, height = .3)+
  draw_plot(p3, x = 0.67, y = .6, width = .33, height = .3)+
  
  draw_plot(p4, x = 0, y = .3, width = .33, height = .3)+
  draw_plot(p5, x = 0.33, y = .3, width = .33, height = .3)+
  draw_plot(p6, x = 0.67, y = .3, width = .33, height = .3)+
  
  draw_plot(p7, x = 0, y = .0, width = .33, height = .3)+
  draw_plot(p8, x = 0.33, y = .0, width = .33, height = .3)+
  draw_plot(p9, x = 0.67, y = .0, width = .33, height = .3)+
  
  draw_plot_label(label = c("(a)","(b)","(c)",
                            "(d)","(e)","(f)",
                            "(g)","(h)","(i)"), size = 12,
                  x = c(0,0.33,0.66,
                        0,0.33,0.66,
                        0,0.33,0.66), y = c(0.9,0.9,0.9,
                                            0.6,0.6,0.6,
                                            0.3,0.3,0.3))+
  geom_text(data = data.frame(
    text = c("Best method - GAMLSS", "Best method - GAMLSS Outiler I", "Best method - GAMLSS Outiler II"),
    x = c(0.165, 0.495, 0.85),
    y = c(0.92, 0.92, 0.92)
  ),
  aes(x = x, y = y, label = text),
  size = 6) +
  theme_void()


dev.off()


pdfName <- "gamlss-f01-BestMethod-Functions" 

pdf(file = paste0('./figures/GAMLSS_results/',pdfName,'.pdf',sep = ""),  width = 15.9, height = 9.8)

ggdraw() +
  draw_plot(a1, x = 0, y = .6, width = .33, height = .3)+
  draw_plot(a2, x = 0.33, y = .6, width = .33, height = .3)+
  draw_plot(a3, x = 0.67, y = .6, width = .33, height = .3)+
  
  draw_plot(a4, x = 0, y = .3, width = .33, height = .3)+
  draw_plot(a5, x = 0.33, y = .3, width = .33, height = .3)+
  draw_plot(a6, x = 0.67, y = .3, width = .33, height = .3)+
  
  draw_plot(a7, x = 0, y = .0, width = .33, height = .3)+
  draw_plot(a8, x = 0.33, y = .0, width = .33, height = .3)+
  draw_plot(a9, x = 0.67, y = .0, width = .33, height = .3)+
  
  draw_plot_label(label = c("(a)","(b)","(c)",
                            "(d)","(e)","(f)",
                            "(g)","(h)","(i)"), size = 12,
                  x = c(0,0.33,0.66,
                        0,0.33,0.66,
                        0,0.33,0.66), y = c(0.9,0.9,0.9,
                                            0.6,0.6,0.6,
                                            0.3,0.3,0.3))+
  geom_text(data = data.frame(
    text = c("Best method - GAMLSS", "Best method - GAMLSS Outiler I", "Best method - GAMLSS Outiler II"),
    x = c(0.165, 0.495, 0.85),
    y = c(0.92, 0.92, 0.92)
  ),
  aes(x = x, y = y, label = text),
  size = 6) +
  theme_void()


dev.off()


