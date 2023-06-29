# 
# GAMLSS last figure to illustrate the difference:
# case 1 using 1910-1939, to predict the last 30 yr estimated floods
# case 2 using 1910-1959, to predict the last 30 yr estimated floods
# case 3 using 1910-2009, to predict the last 30 yr estimated floods
# # gamlss-f04-Functions
# pdfName <- "gamlss-f04-ChangingType" 
# pdf(file = paste0('./figures/GAMLSS_results/',pdfName,'.pdf',sep = ""),  width = 15.9, height = 9.8)
# Two figures are saved!!
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

  
  
  ##################
  ####Using GAMLSS methods. ?? Please note using Non-stationary or Both
  data_second <- GamlssFigure1Data(raw_data,baseyear = 1950)
  
  
  
  ##################
  ####Using GAMLSS methods only 30 years.   # 1910-1939
  newraw <- raw_data
  newraw$maxVal[newraw$year < 1910 | newraw$year > 1939] <- NA
  data_secondnew1 <- GamlssFigure1Data(newraw,baseyear = 1915)
  
  
  ##################
  ####Using GAMLSS methods only 50 years.   # 1910-1959
  newraw <- raw_data
  newraw$maxVal[newraw$year < 1910 | newraw$year > 1959] <- NA
  data_secondnew2 <- GamlssFigure1Data(newraw,baseyear = 1915)
  
  
  ##################
  ####Using GAMLSS methods only 100 years.   # 1910-2009
  newraw <- raw_data
  newraw$maxVal[newraw$year < 1910 | newraw$year > 2009] <- NA
  data_secondnew3 <- GamlssFigure1Data(newraw,baseyear = 1915)
  
  
  
  gamlass<-data_second$flood_magnitude_100yr
  flow_100<-raw_data$flow100
  gamlss_30 <- data_secondnew1$flood_magnitude_100yr
  gamlss_50 <- data_secondnew2$flood_magnitude_100yr
  gamlss_100 <- data_secondnew3$flood_magnitude_100yr
  
  

  gamlassdf<-data.frame(raw_data$year, gamlass)
  gamlassdf_30<-data.frame(raw_data$year, gamlss_30)
  gamlassdf_50<-data.frame(raw_data$year, gamlss_50)
  gamlassdf_100<-data.frame(raw_data$year, gamlss_100)
  
  

  names(gamlassdf)<-c("year","gamlass_166yr")
  names(gamlassdf_30)<-c("year","gamlass_30yr")
  names(gamlassdf_50)<-c("year","gamlass_50yr")
  names(gamlassdf_100)<-c("year","gamlass_100yr")
  
  
  reswl_all<- merge(gamlassdf_30,gamlassdf, by="year", all=TRUE)
  reswl_all<- merge(gamlassdf_50,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(gamlassdf_100,reswl_all, by="year", all=TRUE)
  reswl_all<- merge(raw_data,reswl_all, by="year", all=TRUE)


  
  
  
  reswl_all2 <- reswl_all[,c("year","maxVal","flow100",
                             "gamlass_166yr","gamlass_30yr","gamlass_50yr","gamlass_100yr")]
  names(reswl_all2) <- c("year","Synthetic data","Truth",
                         "GAMLSS ALL","GAMLSS 30yr","GAMLSS 50yr","GAMLSS 100yr")
  
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
# 
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
  
  resall <- reswl_all2[(nrow(reswl_all2)-30+1):nrow(reswl_all2),]
  
  
  resulttemp <- evaluationmetricGAMLSS_last30yr(resall)
  
  data_melt2 <- melt(reswl_all2,id.vars = 'year', variable.name='DataSouces',value.name='waterlevel')
  
  metrics_text <- paste("MAE_GAMLSS ALL:", resulttemp$MAE_gamlss_ALL, "\n",
                        "MAE_GAMLSS 30yr:", resulttemp$MAE_gamlss_30yr, "\n",
                        "MAE_GAMLSS 50yr:", resulttemp$MAE_gamlss_50yr, "\n",
                        "MAE_GAMLSS 100yr:", resulttemp$MAE_gamlss_100yr, "\n")
  
  
  
  # Create the ggplot object
  # Create the ggplot object
  pll <- ggplot(data_melt2, aes(x = year, y = waterlevel, colour = DataSouces, linetype = DataSouces, shape = DataSouces)) +
    mytheme +

    scale_color_manual("waterlevel",
                       values = c("Synthetic data" = "gray80",
                                  "Truth" = "black",
                                  "GAMLSS ALL" = "blue",
                                  "GAMLSS 30yr" = "#f18800",
                                  "GAMLSS 50yr" = "#9ec417",
                                  "GAMLSS 100yr" = "#44c1f0")) +
    scale_shape_manual("waterlevel", values = c(16, NA, NA, NA, NA, NA)) +
    scale_linetype_manual("waterlevel", values = c(0, 1, 1, 1, 1, 1)) +
    labs(y = expression(paste("AMAX [m"^3 * "/s]"))) +
    labs(x = "") +
    # scale_y_log10() +  # Set y-axis to log scale
    scale_x_continuous(breaks = seq(1910, 2015, by = 15), limits = c(1910, 2015)) +
    theme(legend.position = "bottom")+
    guides(color = guide_legend(nrow = 2, ncol = 4))  # Adjust the legend to have 1 column
  
  
  
  pll <- pll +
    geom_rect(xmin = 1986, xmax = 2015, ymin = -Inf, ymax = Inf, fill = "gray", color = "NA", alpha = 0.05)

  

  
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
    annotate("text", x = 1970, y = ypos, label = metrics_text, hjust = 0, size = 3)
  # # Add line and text annotation for GAMLSS 30yr legend
  # pll <- pll +
  #   geom_segment(x = 1919, y = 1500, xend = 1939, yend = 1500, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 1) +
  #   annotate("text", x = 1929, y = 1550, label = "GAMLSS 30yr\n(1910-1939)", hjust = 0.5, size = 3)
  # 
  
  # Display the plot
  pll
  
  
  
  ttt <- paste0("a",i, "=pll")
  eval(parse(text=ttt))
  
  
  
}


# changing p1 a1

# gamlss-f04-Functions

pdfName <- "gamlss-f04-ChangingType" 

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
                                            0.3,0.3,0.3))


dev.off()





