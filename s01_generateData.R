##############
####Main code to generate synthetic data and save them to csv files.
# s1: read the ./gcmdata/ashiba_riversGCM.csv ---> determine the loc, scale, 
# parameters for each referenced rivers.
# s2: generate the synthetic data and save them in the directory of 
# syntheticdataV2 
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


# change the working directory
pwd <- getwd()
setwd(pwd)

# load all functions stored in the ashiba_functions.R
source("./ashiba_functions.R")


##########step1:
####Load the annual maximum time series for 47 rivers form GCMs, 
# and then categorize them into six types.

years <- data.frame(year = seq(1850,2015))
data <- read.csv("./gcmdata/ashiba_riversGCM.csv", header = TRUE, sep = ",")
data <- data[order(data$location), ]
data_earth3 <- data[data$GCMs == "EC-Earth3", ]

# Prepare your data
scaled_data <- scale(data_earth3$location)

# Perform hierarchical clustering
hc <- hclust(dist(scaled_data), method = "ward.D2")

# Cut the dendrogram to get the desired number of clusters
k <- 6  # desired number of clusters
cluster_labels <- cutree(hc, k = k)

# Create a list to store the median or next available value for each cluster
median_values <- vector("list", k)

# Find the median or next available value for each cluster
for (i in 1:k) {
  cluster_rows <- data_earth3[cluster_labels == i, ]
  median_location <- median(cluster_rows$location)
  median_index <- which(cluster_rows$location >= median_location)[1]
  if (is.na(median_index)) {
    median_index <- 1
  }
  median_values[[i]] <- cluster_rows[median_index, ]
}

# Create a new dataframe with the median or next available values
new_df <- do.call(rbind, median_values)

# Print the new dataframe
print(new_df)



######## step2:
######Run the code for case 1, case 2 and case 3



####### case 1, changing location
# Linear Power Partial

location_intial_range <- ceiling(new_df$location)
scale_initial_range_coefficient <- c(0.1, 0.2, 0.3) # this*location_intial_value
shape_initial_range <- new_df$shape




b0_coefficients <- c(1,3/4,2/4,1/4)

### decreasing!!! trend
b1_coefficients <- c(1/5,1/4,1/3,1/2)
b2_coefficients <- c(3/5,3/4,2/3,1)




upper_coe_loc <- 1.5
down_coe_loc <- 0.8

for (i in 1:length(location_intial_range)){
  for (j in 1:length(scale_initial_range_coefficient)){
    for (k in 1:length(b0_coefficients)){
      inital_location <- location_intial_range[i]
      inital_scale <- inital_location*scale_initial_range_coefficient[j]
      inital_shape <-  shape_initial_range[i]


      data_loc1 <- syntheticData_changingLocation(location=inital_location,
                                                 scale=inital_scale,
                                                 shape=inital_shape,
                                                 upper_coe=upper_coe_loc,
                                                 down_coe=down_coe_loc,
                                                 b0_coe=b0_coefficients[k],
                                                 Type="Linear",raw_data=years,
                                                 changingPoint=7,b1_coe=1,b2_coe=1)
      

     write.csv(data_loc1,file=paste0("./syntheticdataV2/ChangingLocation_Linear/",
                                     "Linear_",
                                    inital_location,"_",inital_scale,"_",
                                    inital_shape,"_",b0_coefficients[k],
                                    "_exp.csv"),row.names = F)

     data_loc2 <- syntheticData_changingLocation(location=inital_location,
                                                 scale=inital_scale,
                                                 shape=inital_shape,
                                                upper_coe=upper_coe_loc,
                                                down_coe=down_coe_loc,
                                                b0_coe=b0_coefficients[k],
                                                Type="Power",raw_data=years,
                                                changingPoint=7,b1_coe=1,b2_coe=1)
     write.csv(data_loc2,file=paste0("./syntheticdataV2/ChangingLocation_Power/",
                                     "Power_",
                                     inital_location,"_",inital_scale,"_",
                                     inital_shape,"_",b0_coefficients[k],
                                     "_exp.csv"),row.names = F)
     data_loc3 <- syntheticData_changingLocation(location=inital_location,
                                                 scale=inital_scale,
                                                 shape=inital_shape,
                                                 upper_coe=upper_coe_loc,
                                                 down_coe=down_coe_loc,
                                                 b0_coe=b0_coefficients[k],
                                                 Type="Partial",raw_data=years,
                                                 changingPoint=80,
                                                 b1_coe=b1_coefficients[k],
                                                 b2_coe=b2_coefficients[k])
     write.csv(data_loc3,file=paste0("./syntheticdataV2/ChangingLocation_Partial/",
                                     "Partial_",
                                     inital_location,"_",inital_scale,"_",
                                     inital_shape,"_80_",round(b1_coefficients[k],digits=2),"_",
                                     round(b2_coefficients[k],digits=2),
                                     "_exp.csv"),row.names = F)


     data_loc4 <- syntheticData_changingLocation(location=inital_location,
                                                 scale=inital_scale,
                                                 shape=inital_shape,
                                                 upper_coe=upper_coe_loc,
                                                 down_coe=down_coe_loc,
                                                 b0_coe=b0_coefficients[k],
                                                 Type="Partial_1",raw_data=years,
                                                 changingPoint=80,
                                                 changingPoint_1=50,
                                                 changingPoint_2=100,
                                                 b1_coe=b1_coefficients[k],
                                                 b2_coe=b2_coefficients[k])

     write.csv(data_loc4,file=paste0("./syntheticdataV2/ChangingLocation_Partial_1/",
                                     "Partial1_",
                                     inital_location,"_",inital_scale,"_",
                                     inital_shape,"_80_50_100",round(b1_coefficients[k],digits=2),"_",
                                     round(b2_coefficients[k],digits=2),
                                     "_exp.csv"),row.names = F)

    }
  }
}



####### case 2, changing scale
# Linear Power Partial

location_intial_range <- ceiling(new_df$location)
scale_initial_range_coefficient <- c(0.1, 0.2, 0.3) # this*location_intial_value
shape_initial_range <- new_df$shape




b0_coefficients <- c(1,3/4,2/4,1/4)

### decreasing!!! trend
b1_coefficients <- c(1/5,1/4,1/3,1/2)
b2_coefficients <- c(3/5,3/4,2/3,1)



upper_coe_loc <- 1.1
down_coe_loc <- 0.9

for (i in 1:length(location_intial_range)){
  for (j in 1:length(scale_initial_range_coefficient)){
    for (k in 1:length(b0_coefficients)){
      inital_location <- location_intial_range[i]
      inital_scale <- inital_location*scale_initial_range_coefficient[j]
      inital_shape <- shape_initial_range[i]


      data_loc1 <- syntheticData_changingScale(location=inital_location,
                                                  scale=inital_scale,
                                                  shape=inital_shape,
                                                  upper_coe=upper_coe_loc,
                                                  down_coe=down_coe_loc,
                                                  b0_coe=b0_coefficients[k],
                                                  Type="Linear",raw_data=years,
                                                  changingPoint=7,b1_coe=1,b2_coe=1)

      write.csv(data_loc1,file=paste0("./syntheticdataV2/ChangingScale_Linear/",
                                      "Linear_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_",b0_coefficients[k],
                                      "_exp.csv"),row.names = F)

      data_loc2 <- syntheticData_changingScale(location=inital_location,
                                                  scale=inital_scale,
                                                  shape=inital_shape,
                                                  upper_coe=upper_coe_loc,
                                                  down_coe=down_coe_loc,
                                                  b0_coe=b0_coefficients[k],
                                                  Type="Power",raw_data=years,
                                                  changingPoint=7,b1_coe=1,b2_coe=1)
      write.csv(data_loc2,file=paste0("./syntheticdataV2/ChangingScale_Power/",
                                      "Power_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_",b0_coefficients[k],
                                      "_exp.csv"),row.names = F)
      data_loc3 <- syntheticData_changingScale(location=inital_location,
                                                  scale=inital_scale,
                                                  shape=inital_shape,
                                                  upper_coe=upper_coe_loc,
                                                  down_coe=down_coe_loc,
                                                  b0_coe=b0_coefficients[k],
                                                  Type="Partial",raw_data=years,
                                                  changingPoint=80,
                                                  b1_coe=b1_coefficients[k],
                                                  b2_coe=b2_coefficients[k])
      write.csv(data_loc3,file=paste0("./syntheticdataV2/ChangingScale_Partial/",
                                      "Partial_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_80_",round(b1_coefficients[k],digits=2),"_",
                                      round(b2_coefficients[k],digits=2),
                                      "_exp.csv"),row.names = F)




      data_loc4 <- syntheticData_changingScale(location=inital_location,
                                               scale=inital_scale,
                                               shape=inital_shape,
                                               upper_coe=upper_coe_loc,
                                               down_coe=down_coe_loc,
                                               b0_coe=b0_coefficients[k],
                                               Type="Partial_1",raw_data=years,
                                               changingPoint=80,
                                               changingPoint_1=50,
                                               changingPoint_2=100,
                                               b1_coe=b1_coefficients[k],
                                               b2_coe=b2_coefficients[k])

      write.csv(data_loc4,file=paste0("./syntheticdataV2/ChangingScale_Partial_1/",
                                      "Partial1_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_80_50_100",round(b1_coefficients[k],digits=2),"_",
                                      round(b2_coefficients[k],digits=2),
                                      "_exp.csv"),row.names = F)


    }
  }
}




####### case 3, changing location and scale
# Linear Power Partial


location_intial_range <- ceiling(new_df$location)
scale_initial_range_coefficient <- c(0.1, 0.2, 0.3) # this*location_intial_value
shape_initial_range <- new_df$shape

b0_coefficients <- c(1,3/4,2/4,1/4)

### decreasing!!! trend
b1_coefficients <- c(1/5,1/4,1/3,1/2)
b2_coefficients <- c(3/5,3/4,2/3,1)

upper_coe_loc <- 1.5
down_coe_loc <- 0.8

upper_coe_sca <- 0.2
down_coe_sca <- 0.1


for (i in 1:length(location_intial_range)){
  for (j in 1:length(scale_initial_range_coefficient)){
    for (k in 1:length(b0_coefficients)){
      inital_location <- location_intial_range[i]
      inital_scale <- inital_location*scale_initial_range_coefficient[j]
      inital_shape <- shape_initial_range[i]
      
      
      data_loc1 <- syntheticData_changingLocationscale(location=inital_location,
                                               scale=inital_scale,
                                               shape=inital_shape,
                                               upper_coe=upper_coe_loc,
                                               down_coe=down_coe_loc,
                                               b0_coe=b0_coefficients[k],
                                               upper_coe_1=upper_coe_sca,
                                               down_coe_1=down_coe_sca,
                                               Type="Linear",raw_data=years,
                                               changingPoint=7,b1_coe=1,b2_coe=1)

      b01 <- (mean(data_loc1$location)*upper_coe_sca - mean(data_loc1$location)*down_coe_sca)/(length(years$year)-1)

      write.csv(data_loc1,file=paste0("./syntheticdataV2/ChangingLocationScale_Linear/",
                                      "Linear_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_",b0_coefficients[k],
                                      "_",round(b01,digits=2) ,
                                      "_exp.csv"),row.names = F)

      data_loc2 <- syntheticData_changingLocationscale(location=inital_location,
                                               scale=inital_scale,
                                               shape=inital_shape,
                                               upper_coe=upper_coe_loc,
                                               down_coe=down_coe_loc,
                                               b0_coe=b0_coefficients[k],
                                               upper_coe_1=upper_coe_sca,
                                               down_coe_1=down_coe_sca,
                                               Type="Power",raw_data=years,
                                               changingPoint=7,b1_coe=1,b2_coe=1)
      b01 <- (mean(data_loc1$location)*upper_coe_sca - mean(data_loc1$location)*down_coe_sca)/((length(years$year)-1)^2)

      write.csv(data_loc2,file=paste0("./syntheticdataV2/ChangingLocationScale_Power/",
                                      "Power_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_",b0_coefficients[k],
                                      "_",round(b01,digits=2) ,
                                      "_exp.csv"),row.names = F)

      data_loc3 <- syntheticData_changingLocationscale(location=inital_location,
                                               scale=inital_scale,
                                               shape=inital_shape,
                                               upper_coe=upper_coe_loc,
                                               down_coe=down_coe_loc,
                                               b0_coe=b0_coefficients[k],
                                               upper_coe_1=upper_coe_sca,
                                               down_coe_1=down_coe_sca,
                                               Type="Partial",raw_data=years,
                                               changingPoint=80,
                                               b1_coe=b1_coefficients[k],
                                               b2_coe=b2_coefficients[k])

      write.csv(data_loc3,file=paste0("./syntheticdataV2/ChangingLocationScale_Partial/",
                                      "Partial_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_80_",round(b1_coefficients[k],digits=2),"_",
                                      round(b2_coefficients[k],digits=2),
                                      "_exp.csv"),row.names = F)

      
      
      data_loc4 <- syntheticData_changingLocationscale(location=inital_location,
                                                         scale=inital_scale,
                                                         shape=inital_shape,
                                                         upper_coe=upper_coe_loc,
                                                         down_coe=down_coe_loc,
                                                         b0_coe=b0_coefficients[k],
                                                         upper_coe_1=upper_coe_sca,
                                                         down_coe_1=down_coe_sca,
                                                         Type="Partial_1",raw_data=years,
                                                         changingPoint=80,changingPoint_1=50,changingPoint_2=100,
                                                         b1_coe=b1_coefficients[k],
                                                         b2_coe=b2_coefficients[k])
      
      write.csv(data_loc4,file=paste0("./syntheticdataV2/ChangingLocationScale_Partial_1/", 
                                      "Partial1_",
                                      inital_location,"_",inital_scale,"_",
                                      inital_shape,"_80_50_100",round(b1_coefficients[k],digits=2),"_",
                                      round(b2_coefficients[k],digits=2),
                                      "_exp.csv"),row.names = F)
      
    }
  }
}
