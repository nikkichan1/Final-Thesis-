##############
####Main code to run the code, getting the pngs
# 
# select some csv files to illustrate the best GEV Method in different situations
# Two figures are saved!!
# pdfName <- "166yr_BestMethod_ChangingTypes" 
# pdfName <- "166yr_BestMethod_Functions" 
# pdf(file = paste0('./figures/GEV_results/',pdfName,'.pdf',sep = ""),  width = 17.9, height = 9.8)
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


###################
##### ggplot scheme
mytheme <- theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1, color = 'black'),
    axis.text.y = element_text(size = 10, color = 'black'),
    axis.ticks = element_line(linetype = 1, color = 'black'),
    legend.text = element_text(size = 7),  # Adjust the legend text size
    axis.title = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.83),  
    # Adjust the position of the legend to top left
    legend.background = element_rect(color = "gray", fill = "white"),  
    # Set the legend background
    legend.key.size = unit(0.5, "lines") 
    # Adjust the size of the legend key
  )



# You need to change the working directory
pwd <- getwd()
setwd(pwd)

# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")

#########################
##run all get the results
runALL <- function(csvdata){
  
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
  dataTemp_1 <- Reject_outliers_method1(dataTemp_1,threshold_returnPeriod=50,chunk=30)
  dataTemp_1$r <- NULL
  df_one <- Discharge_at_GivenReturnPeriodandSamples(dataTemp_1,window_size,return_periods)
  
  ##################
  ####Using method 2.
  dataTemp_2  <- raw_data 
  dataTemp_2 <- Reject_outliers_method2(dataTemp_2,window_size=30,threshold_times = 2)
  dataTemp_3<-dataTemp_2
  dataTemp_3$frequency <- NULL
  df_two <- Discharge_at_GivenReturnPeriodandSamples(dataTemp_3,window_size,return_periods)
  
  
  ##################
  ####Using GAMLSS methods.
  # data_second <- GamlssFigure1Data(raw_data,baseyear = 1950)
  
  
  
  ##################
  ####combine the  data
  na_indices <- which(is.na(dataTemp_1$maxVal))
  outlier_raw_1<- raw_data[na_indices, ]
  na_indices <- which(is.na(dataTemp_2$maxVal))
  outlier_raw_2<- raw_data[na_indices, ]
  
  
  mw<-df_ini$return_100
  # gamlass<-data_second$flood_magnitude_100yr
  mw_outlier1<-df_one$return_100
  mw_outlier2<-df_two$return_100
  flow_100<-raw_data$flow100
  
  
  mwdf<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw)
  # gamlassdf<-data.frame(raw_data$year, gamlass)
  origindf<-data.frame(raw_data$year, flow_100)
  outlierdf_1<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw_outlier1)
  outlierdf_2<-data.frame(raw_data$year[window_size:nrow(raw_data)], mw_outlier2)
  
  
  outlier_1<-data.frame(outlier_raw_1$year, outlier_raw_1$maxVal)
  outlier_2<-data.frame(outlier_raw_2$year, outlier_raw_2$maxVal)
  
  
  names(mwdf)<-c("year","mw_100")
  # names(gamlassdf)<-c("year","gamlass_100")
  names(origindf)<-c("year","orgin_100")
  names(outlierdf_1)<-c("year","outlier1_100")
  names(outlierdf_2)<-c("year","outlier2_100")
  
  names(outlier_1)<-c("year","outlier_1")
  names(outlier_2)<-c("year","outlier_2")
  
  
  # reswl_all<- merge(gamlassdf,mwdf, by="year", all=TRUE)
  reswl_all<- merge(raw_data,mwdf, by="year", all=TRUE)
  reswl_all<- merge(origindf,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlierdf_1,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlierdf_2,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlier_1,by="year", all=TRUE)
  reswl_all<- merge(reswl_all,outlier_2,by="year", all=TRUE)
  
  reswl_all2 <- reswl_all[,c("year","maxVal","outlier_1","outlier_2","orgin_100","mw_100","outlier1_100","outlier2_100")]
  names(reswl_all2) <- c("year","Synthetic data","Outliers I","Outliers II","Truth","MW","MWOutlier I","MWOutlier II")
  
  return(reswl_all2)
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
# ffnames1<-"./syntheticdataV2/ChangingLocation_Linear/Linear_12966_1296.6_-0.115845393337307_0.75_exp.csv"
# ffnames2<-"./syntheticdataV2/ChangingLocation_Linear/Linear_1950_585_-0.192943477412856_0.25_exp.csv"
# ffnames3<-"./syntheticdataV2/ChangingLocation_Linear/Linear_32163_6432.6_-0.181686023116274_0.75_exp.csv"
# ffnames5<-"./syntheticdataV2/ChangingLocation_Partial/Partial_12966_2593.2_-0.115845393337307_80_0.25_0.75_exp.csv"
# ffnames7<-"./syntheticdataV2/ChangingLocation_Power/Power_25979_5195.8_-0.213318655675175_1_exp.csv"
# ffnames8<-"./syntheticdataV2/ChangingLocation_Power/Power_4546_1363.8_-0.0119584849289948_0.75_exp.csv"

ffnames1<-"./syntheticdataV2/ChangingLocation_Linear/Linear_4546_909.2_-0.0119584849289948_0.75_exp.csv"
ffnames2<-"./syntheticdataV2/ChangingLocation_Partial_1/Partial1_4546_909.2_-0.0119584849289948_80_50_1000.33_0.67_exp.csv"


ffnames3<-"./syntheticdataV2/ChangingLocationScale_Linear/Linear_4546_909.2_-0.0119584849289948_0.75_5.85_exp.csv"
# ffnames10<-"./syntheticdataV2/ChangingLocationScale_Linear/Linear_25979_5195.8_-0.213318655675175_0.5_30.7_exp.csv"
# ffnames11<-"./syntheticdataV2/ChangingLocationScale_Linear/Linear_1950_195_-0.192943477412856_0.75_2.51_exp.csv"
# ffnames12<-"./syntheticdataV2/ChangingLocationScale_Power/Power_4546_909.2_-0.0119584849289948_0.75_0.04_exp.csv"
# ffnames13<-"./syntheticdataV2/ChangingLocationScale_Power/Power_32163_9648.9_-0.181686023116274_0.25_0.21_exp.csv"



# ffnames14<-"./syntheticdataV2/ChangingScale_Linear/Linear_4546_909.2_-0.0119584849289948_0.25_exp.csv"
ffnames4<-"./syntheticdataV2/ChangingScale_Linear/Linear_12966_3889.8_-0.115845393337307_1_exp.csv"
ffnames5<-"./syntheticdataV2/ChangingScale_Partial/Partial_4546_454.6_-0.0119584849289948_80_0.25_0.75_exp.csv"
# ffnames17<-"./syntheticdataV2/ChangingScale_Partial/Partial_4546_454.6_-0.0119584849289948_80_0.2_0.6_exp.csv"
# ffnames18<-"./syntheticdataV2/ChangingScale_Partial_1/Partial1_4546_454.6_-0.0119584849289948_80_50_1000.33_0.67_exp.csv"
# ffnames19<-"./syntheticdataV2/ChangingScale_Power/Power_76_7.6_-0.185561787469612_0.5_exp.csv"



####### MW performs best


fffnames1 <- "./syntheticdataV2/ChangingLocation_Power/Power_25979_7793.7_-0.213318655675175_1_exp.csv"

fffnames2 <- "./syntheticdataV2/ChangingScale_Partial/Partial_1950_585_-0.192943477412856_80_0.25_0.75_exp.csv"

fffnames3<- "./syntheticdataV2/ChangingLocationScale_Partial/Partial_4546_454.6_-0.0119584849289948_80_0.2_0.6_exp.csv"
  




datanew <- read.csv("./syntheticdataV2/ChangingLocation_Power/Power_4546_1363.8_-0.0119584849289948_0.75_exp.csv")
resall <- runALL(datanew)

resulttemp <- evaluationmetric(resall)

data_melt2 <- melt(resall,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')

metrics_text <- paste("MAE_mwoutlier1:", resulttemp$MAE_mwoutlier1, "\n",
                      "MAE_mwoutlier2:", resulttemp$MAE_mwoutlier2, "\n",
                      "MAE_mw:", resulttemp$MAE_mw, "\n")

p20<-ggplot(data_melt2, aes(x=year, y=waterlevel, colour=DataSouces,linetype=DataSouces,shape=DataSouces))+
  mytheme+
  geom_line(size=1)+
  geom_point(alpha=1,size=2)+
  scale_color_manual("waterlevel",
                     values =  c("Synthetic data"  = "gray80", 
                                 "Outliers I" = "#e8490f",
                                 "Outliers II" = "#e4ce00",
                                 "Truth"       = "black",
                                 "MW"       = "#f18800",
                                 "MWOutlier I"  = "#9ec417",
                                 "MWOutlier II"  = "#44c1f0"))+
  scale_shape_manual("waterlevel"     ,values=c(16,16,16,NA,NA,NA,NA))+
  scale_linetype_manual("waterlevel"  ,values=c(0,0,0,1,1,1,1)) +
  labs(y = expression(paste("AMAX [m"^3*"/s]"))) +
  labs(x = "") +
  scale_x_continuous(breaks = seq(1850, 2015, by = 30)) +
  theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=1))
p20

# Determine the range of y values
yrange <- range(data_melt2$waterlevel,na.rm = TRUE)

# Calculate a suitable position, for example, 90% up the plot
ypos <- yrange[1] + 0.85 * diff(yrange)

p20 <- p20 + 
  annotate("text", x = 1850, y = ypos, label = metrics_text, hjust = 0, size = 3)
p20





datanew <- read.csv("./syntheticdataV2/ChangingLocation_Linear/Linear_4546_909.2_-0.0119584849289948_1_exp.csv")
resall <- runALL(datanew)

resulttemp <- evaluationmetric(resall)

data_melt2 <- melt(resall,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')

metrics_text <- paste("MAE_mwoutlier1:", resulttemp$MAE_mwoutlier1, "\n",
                      "MAE_mwoutlier2:", resulttemp$MAE_mwoutlier2, "\n",
                      "MAE_mw:", 1349.06, "\n")

p19<-ggplot(data_melt2, aes(x=year, y=waterlevel, colour=DataSouces,linetype=DataSouces,shape=DataSouces))+
  mytheme+
  geom_line(size=1)+
  geom_point(alpha=1,size=2)+
  scale_color_manual("waterlevel",
                     values =  c("Synthetic data"  = "gray80", 
                                 "Outliers I" = "#e8490f",
                                 "Outliers II" = "#e4ce00",
                                 "Truth"       = "black",
                                 "MW"       = "#f18800",
                                 "MWOutlier I"  = "#9ec417",
                                 "MWOutlier II"  = "#44c1f0"))+
  scale_shape_manual("waterlevel"     ,values=c(16,16,16,NA,NA,NA,NA))+
  scale_linetype_manual("waterlevel"  ,values=c(0,0,0,1,1,1,1)) +
  labs(y = expression(paste("AMAX [m"^3*"/s]"))) +
  labs(x = "") +
  scale_x_continuous(breaks = seq(1850, 2015, by = 30)) +
  theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=1))
p19

# Determine the range of y values
yrange <- range(data_melt2$waterlevel,na.rm = TRUE)

# Calculate a suitable position, for example, 90% up the plot
ypos <- yrange[1] + 0.85 * diff(yrange)

p19 <- p19 + 
  annotate("text", x = 1850, y = ypos, label = metrics_text, hjust = 0, size = 3)
p19





####### Moving window performs best

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

for (i in 1:10){

  ttt <- paste0("newfname", "=fnames",i)
  eval(parse(text=ttt))
  
  
  datanew <- read.csv(newfname)
  
  
  resall <- runALL(datanew)
  
  resulttemp <- evaluationmetric(resall)
  
  data_melt2 <- melt(resall,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')
  
  metrics_text <- paste("MAE_mwoutlier1:", resulttemp$MAE_mwoutlier1, "\n",
                        "MAE_mwoutlier2:", resulttemp$MAE_mwoutlier2, "\n",
                        "MAE_mw:", resulttemp$MAE_mw, "\n")
  
  pll<-ggplot(data_melt2, aes(x=year, y=waterlevel, colour=DataSouces,linetype=DataSouces,shape=DataSouces))+
    mytheme+
    geom_line(size=1)+
    geom_point(alpha=1,size=2)+
    scale_color_manual("waterlevel",
                       values =  c("Synthetic data"  = "gray80", 
                                   "Outliers I" = "#e8490f",
                                   "Outliers II" = "#e4ce00",
                                   "Truth"       = "black",
                                   "MW"       = "#f18800",
                                   "MWOutlier I"  = "#9ec417",
                                   "MWOutlier II"  = "#44c1f0"))+
    scale_shape_manual("waterlevel"     ,values=c(16,16,16,NA,NA,NA,NA))+
    scale_linetype_manual("waterlevel"  ,values=c(0,0,0,1,1,1,1)) +
    labs(y = expression(paste("AMAX [m"^3*"/s]"))) +
    labs(x = "") +
    scale_x_continuous(breaks = seq(1850, 2015, by = 30)) +
    theme(legend.position = "bottom")+
    guides(color=guide_legend(nrow=1))
  pll
  
  # Determine the range of y values
  yrange <- range(data_melt2$waterlevel,na.rm = TRUE)
  
  # Calculate a suitable position, for example, 90% up the plot
  ypos <- yrange[1] + 0.85 * diff(yrange)
  
  pll <- pll + 
    annotate("text", x = 1850, y = ypos, label = metrics_text, hjust = 0, size = 3)
  pll
  
  
  ttt <- paste0("p",i, "=pll")
  eval(parse(text=ttt))
}



for (i in 1:5){
  
  ttt <- paste0("newfname", "=ffnames",i)
  eval(parse(text=ttt))
  
  
  datanew <- read.csv(newfname)
  
  
  resall <- runALL(datanew)
  
  resulttemp <- evaluationmetric(resall)
  
  data_melt2 <- melt(resall,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')
  
  metrics_text <- paste("MAE_mwoutlier1:", resulttemp$MAE_mwoutlier1, "\n",
                        "MAE_mwoutlier2:", resulttemp$MAE_mwoutlier2, "\n",
                        "MAE_mw:", resulttemp$MAE_mw, "\n")
  
  pll<-ggplot(data_melt2, aes(x=year, y=waterlevel, colour=DataSouces,linetype=DataSouces,shape=DataSouces))+
    mytheme+
    geom_line(size=1)+
    geom_point(alpha=1,size=2)+
    scale_color_manual("waterlevel",
                       values =  c("Synthetic data"  = "gray80", 
                                   "Outliers I" = "#e8490f",
                                   "Outliers II" = "#e4ce00",
                                   "Truth"       = "black",
                                   "MW"       = "#f18800",
                                   "MWOutlier I"  = "#9ec417",
                                   "MWOutlier II"  = "#44c1f0"))+
    scale_shape_manual("waterlevel"     ,values=c(16,16,16,NA,NA,NA,NA))+
    scale_linetype_manual("waterlevel"  ,values=c(0,0,0,1,1,1,1)) +
    labs(y = expression(paste("AMAX [m"^3*"/s]"))) +
    labs(x = "") +
    scale_x_continuous(breaks = seq(1850, 2015, by = 30)) +
    theme(legend.position = "bottom")+
    guides(color=guide_legend(nrow=1))
  pll
  
  # Determine the range of y values
  yrange <- range(data_melt2$waterlevel,na.rm = TRUE)
  
  # Calculate a suitable position, for example, 90% up the plot
  ypos <- yrange[1] + 0.85 * diff(yrange)
  
  pll <- pll + 
    annotate("text", x = 1850, y = ypos, label = metrics_text, hjust = 0, size = 3)
  pll
  
  ttt <- paste0("p",(i+10), "=pll")
  eval(parse(text=ttt))
}



for (i in 1:3){
  
  ttt <- paste0("newfname", "=fffnames",i)
  eval(parse(text=ttt))
  
  
  datanew <- read.csv(newfname)
  
  
  resall <- runALL(datanew)
  
  resulttemp <- evaluationmetric(resall)
  
  data_melt2 <- melt(resall,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')
  
  metrics_text <- paste("MAE_mwoutlier1:", resulttemp$MAE_mwoutlier1, "\n",
                        "MAE_mwoutlier2:", resulttemp$MAE_mwoutlier2, "\n",
                        "MAE_mw:", resulttemp$MAE_mw, "\n")
  
  pall<-ggplot(data_melt2, aes(x=year, y=waterlevel, colour=DataSouces,linetype=DataSouces,shape=DataSouces))+
    mytheme+
    geom_line(size=1)+
    geom_point(alpha=1,size=2)+
    scale_color_manual("waterlevel",
                       values =  c("Synthetic data"  = "gray80", 
                                   "Outliers I" = "#e8490f",
                                   "Outliers II" = "#e4ce00",
                                   "Truth"       = "black",
                                   "MW"       = "#f18800",
                                   "MWOutlier I"  = "#9ec417",
                                   "MWOutlier II"  = "#44c1f0"))+
    scale_shape_manual("waterlevel"     ,values=c(16,16,16,NA,NA,NA,NA))+
    scale_linetype_manual("waterlevel"  ,values=c(0,0,0,1,1,1,1)) +
    labs(y = expression(paste("AMAX [m"^3*"/s]"))) +
    labs(x = "") +
    scale_x_continuous(breaks = seq(1850, 2015, by = 30)) +
    theme(legend.position = "bottom")+
    guides(color=guide_legend(nrow=1))+ theme(legend.position = "none")
  pll
  
  # Determine the range of y values
  yrange <- range(data_melt2$waterlevel,na.rm = TRUE)
  
  # Calculate a suitable position, for example, 90% up the plot
  ypos <- yrange[1] + 0.85 * diff(yrange)
  
  pll <- pll + 
    annotate("text", x = 1850, y = ypos, label = metrics_text, hjust = 0, size = 3)
  pll
  
  ttt <- paste0("p",(i+10+5), "=pll")
  eval(parse(text=ttt))
}





### Chaing scale, location, Both
# Outlier1: p1-p6, p7, p8-10
# Outlier2: p11-p18, p19-23, p24-19
# movingwindow: 





pdfName <- "166yr_BestMethod_ChangingTypes" 

pdf(file = paste0('./figures/GEV_results/',pdfName,'.pdf',sep = ""),  width = 17.9, height = 9.8)

ggdraw() +
  draw_plot(p2, x = 0, y = .6, width = .33, height = .3)+
  draw_plot(p11, x = 0.33, y = .6, width = .33, height = .3)+
  draw_plot(p6, x = 0.67, y = .6, width = .33, height = .3)+
  
  draw_plot(p8, x = 0, y = .3, width = .33, height = .3)+
  draw_plot(p14, x = 0.33, y = .3, width = .33, height = .3)+
  draw_plot(p17, x = 0.67, y = .3, width = .33, height = .3)+
  
  draw_plot(p7, x = 0, y = .0, width = .33, height = .3)+
  draw_plot(p13, x = 0.33, y = .0, width = .33, height = .3)+
  draw_plot(p18, x = 0.67, y = .0, width = .33, height = .3)+
  
  draw_plot_label(label = c("(a)","(b)","(c)",
                            "(d)","(e)","(f)",
                            "(g)","(h)","(i)"), size = 12,
                  x = c(0,0.33,0.66,
                        0,0.33,0.66,
                        0,0.33,0.66), y = c(0.9,0.9,0.9,
                                            0.6,0.6,0.6,
                                            0.3,0.3,0.3))+
  geom_text(data = data.frame(
    text = c("Best method - MWOutiler I", "Best method - MWOutiler II", "Best method - MW"),
    x = c(0.165, 0.495, 0.85),
    y = c(0.92, 0.92, 0.92)
  ),
  aes(x = x, y = y, label = text),
  size = 6) +
  theme_void()
  

dev.off()




pdfName <- "166yr_BestMethod_Functions" 

pdf(file = paste0('./figures/GEV_results/',pdfName,'.pdf',sep = ""),  width = 17.9, height = 9.8)

ggdraw() +
  draw_plot(p19, x = 0, y = .6, width = .33, height = .3)+
  draw_plot(p11, x = 0.33, y = .6, width = .33, height = .3)+
  draw_plot(p1, x = 0.67, y = .6, width = .33, height = .3)+
  
  draw_plot(p7, x = 0, y = .3, width = .33, height = .3)+
  draw_plot(p20, x = 0.33, y = .3, width = .33, height = .3)+
  draw_plot(p16, x = 0.67, y = .3, width = .33, height = .3)+
  
  draw_plot(p8, x = 0, y = .0, width = .33, height = .3)+
  draw_plot(p15, x = 0.33, y = .0, width = .33, height = .3)+
  draw_plot(p18, x = 0.67, y = .0, width = .33, height = .3)+
  
  draw_plot_label(label = c("(a)","(b)","(c)",
                            "(d)","(e)","(f)",
                            "(g)","(h)","(i)"), size = 12,
                  x = c(0,0.33,0.66,
                        0,0.33,0.66,
                        0,0.33,0.66), y = c(0.9,0.9,0.9,
                                            0.6,0.6,0.6,
                                            0.3,0.3,0.3))+
  geom_text(data = data.frame(
    text = c("Best method - MWOutiler I", "Best method - MWOutiler II", "Best method - MW"),
    x = c(0.165, 0.495, 0.85),
    y = c(0.92, 0.92, 0.92)
  ),
  aes(x = x, y = y, label = text),
  size = 6) +
  theme_void()


dev.off()


