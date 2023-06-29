

#######
# To run this code, you should run the ashiba_Example.Rmd to generate synthetic Data
# first. Then save them as PNG by yourself.
####


#######---Figure 1
inital_location <- 3000
inital_scale <- 900
inital_shape <- 0.15

upper_coe_loc <- 1.5
down_coe_loc <- 0.8


b0_coefficients <- c(1,1/2,1/3)

b1_coefficients <- c(1/3,1/4,1/5)
b2_coefficients <- c(1,1/3,1/2)

downboundary <- down_coe_loc*inital_location
upperboudary <- upper_coe_loc*inital_location
b <- (upperboudary-downboundary)/(length(qcov)-1)
bmax<-(upperboudary-downboundary)/((length(qcov)-1)^2)

locations1<-data_loc_linear_1$location
locations2<-data_loc_linear_2$location
locations3<-data_loc_linear_3$location
random_numbers1<-data_loc_linear_1$random_numbers
random_numbers2<-data_loc_linear_2$random_numbers
random_numbers3<-data_loc_linear_3$random_numbers



locations4<-data_loc_power_1$location
locations5<-data_loc_power_2$location
locations6<-data_loc_power_3$location
random_numbers4<-data_loc_power_1$random_numbers
random_numbers5<-data_loc_power_2$random_numbers
random_numbers6<-data_loc_power_3$random_numbers


locations7<-data_loc_partial_1$location
locations8<-data_loc_partial_2$location
locations9<-data_loc_partial_3$location
random_numbers7<-data_loc_partial_1$random_numbers
random_numbers8<-data_loc_partial_2$random_numbers
random_numbers9<-data_loc_partial_3$random_numbers


#### plot
par(mfrow = c(3, 6), mar = c(5, 4, 4, 2) + 0.1, oma = c(1, 2, 2, 0))
par(las = 0, font.main = 2, cex.main = 1)


col <- adjustcolor("grey", alpha = 0.2)

plot(locations1, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[1]*b,digits=2), "*t"), xlab = "t", ylab = "Location")

rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers1,  xlab = "t", ylab = "Discharge (m3/s)", xlim = c(0, 166))


plot(locations4, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[1]*bmax,digits=2), "*t*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers4, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations7, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[1]*b,digits=2), ", b2=", round(b2_coefficients[1]*b,digits=2)), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers7, xlab = "t", ylab = "Discharge (m3/s)")




plot(locations2, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[2]*b,digits=2), "*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers2, xlab = "t", ylab = "Discharge (m3/s)")


plot(locations5, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[2]*bmax,digits=2), "*t*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers5, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations8, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[2]*b,digits=2), ", b2=", round(b2_coefficients[2]*b,digits=2)), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers8, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations3, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[3]*b,digits=2), "*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers3, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations6, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[3]*bmax,digits=2), "*t*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers6, xlab = "t", ylab = "Discharge (m3/s)")


plot(locations9, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[3]*b,digits=2), ", b2=", round(b2_coefficients[3]*b,digits=2)), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers9, xlab = "t", ylab = "Discharge (m3/s)")





#######---Figure 2


inital_location <- 3000
inital_scale <- 900
inital_shape <- 0.15

upper_coe_loc <- 1.1
down_coe_loc <- 0.9


b0_coefficients <- c(1,1/2,1/3)

b1_coefficients <- c(1/3,1/4,1/5)
b2_coefficients <- c(1,1/3,1/2)

downboundary <- down_coe_loc*inital_scale
upperboudary <- upper_coe_loc*inital_scale
b <- (upperboudary-downboundary)/(length(qcov)-1)
bmax<-(upperboudary-downboundary)/((length(qcov)-1)^2)

locations1<-data_sca_linear_1$scale
locations2<-data_sca_linear_2$scale
locations3<-data_sca_linear_3$scale
random_numbers1<-data_sca_linear_1$random_numbers
random_numbers2<-data_sca_linear_2$random_numbers
random_numbers3<-data_sca_linear_3$random_numbers



locations4<-data_sca_power_1$scale
locations5<-data_sca_power_2$scale
locations6<-data_sca_power_3$scale
random_numbers4<-data_sca_power_1$random_numbers
random_numbers5<-data_sca_power_2$random_numbers
random_numbers6<-data_sca_power_3$random_numbers


locations7<-data_sca_partial_1$scale
locations8<-data_sca_partial_2$scale
locations9<-data_sca_partial_3$scale
random_numbers7<-data_sca_partial_1$random_numbers
random_numbers8<-data_sca_partial_2$random_numbers
random_numbers9<-data_sca_partial_3$random_numbers


#### plot
par(mfrow = c(3, 6), mar = c(5, 4, 4, 2) + 0.1, oma = c(1, 2, 2, 0))
par(las = 0, font.main = 2, cex.main = 1)


col <- adjustcolor("grey", alpha = 0.2)

plot(locations1, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("scale=", round(down_coe_loc*inital_scale,digits=0), "+", round(b0_coefficients[1]*b,digits=2), "*t"), xlab = "t", ylab = "Scale")

rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers1,  xlab = "t", ylab = "Discharge (m3/s)", xlim = c(0, 166))


plot(locations4, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("scale=", round(down_coe_loc*inital_scale,digits=0), "+", round(b0_coefficients[1]*bmax,digits=3), "*t*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers4, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations7, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[1]*b,digits=2), ", b2=", round(b2_coefficients[1]*b,digits=2)), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers7, xlab = "t", ylab = "Discharge (m3/s)")




plot(locations2, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("scale=", round(down_coe_loc*inital_scale,digits=0), "+", round(b0_coefficients[2]*b,digits=2), "*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers2, xlab = "t", ylab = "Discharge (m3/s)")


plot(locations5, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("scale=", round(down_coe_loc*inital_scale,digits=0), "+", round(b0_coefficients[2]*bmax,digits=3), "*t*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers5, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations8, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[2]*b,digits=2), ", b2=", round(b2_coefficients[2]*b,digits=2)), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers8, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations3, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("scale=", round(down_coe_loc*inital_scale,digits=0), "+", round(b0_coefficients[3]*b,digits=2), "*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers3, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations6, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("scale=", round(down_coe_loc*inital_scale,digits=0), "+", round(b0_coefficients[3]*bmax,digits=3), "*t*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers6, xlab = "t", ylab = "Discharge (m3/s)")


plot(locations9, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[3]*b,digits=2), ", b2=", round(b2_coefficients[3]*b,digits=2)), xlab = "t", ylab = "Scale")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
plot(random_numbers9, xlab = "t", ylab = "Discharge (m3/s)")







#######---Figure 3


inital_location <- 3000
inital_scale <- 900
inital_shape <- 0.15

upper_coe_loc <- 1.5
down_coe_loc <- 0.8

upper_coe_sca <- 0.4
down_coe_sca <- 0.2

b0_coefficients <- c(1,1/2,1/3)

b1_coefficients <- c(1/3,1/4,1/5)
b2_coefficients <- c(1,1/3,1/2)

downboundary <- down_coe_loc*inital_location
upperboudary <- upper_coe_loc*inital_location

b <- (upperboudary-downboundary)/(length(qcov)-1)
bmax<-(upperboudary-downboundary)/((length(qcov)-1)^2)



locations1<-data_locsca_linear_1$location
locations2<-data_locsca_linear_3$location
locations3<-data_locsca_linear_3$location
scales1<-data_locsca_linear_1$scale
scales2<-data_locsca_linear_3$scale
scales3<-data_locsca_linear_3$scale
random_numbers1<-data_locsca_linear_1$random_numbers
random_numbers2<-data_locsca_linear_2$random_numbers
random_numbers3<-data_locsca_linear_3$random_numbers



locations4<-data_locsca_power_1$location
locations5<-data_locsca_power_2$location
locations6<-data_locsca_power_3$location
scales4<-data_locsca_power_1$scale
scales5<-data_locsca_power_3$scale
scales6<-data_locsca_power_3$scale
random_numbers4<-data_locsca_power_1$random_numbers
random_numbers5<-data_locsca_power_2$random_numbers
random_numbers6<-data_locsca_power_3$random_numbers


locations7<-data_locsca_partial_1$location
locations8<-data_locsca_partial_2$location
locations9<-data_locsca_partial_3$location
scales7<-data_locsca_partial_1$scale
scales8<-data_locsca_partial_3$scale
scales9<-data_locsca_partial_3$scale
random_numbers7<-data_locsca_partial_1$random_numbers
random_numbers8<-data_locsca_partial_2$random_numbers
random_numbers9<-data_locsca_partial_3$random_numbers


#### plot
par(mfrow = c(3, 9), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))
par(las = 0, font.main = 2, cex.main = 1)


col <- adjustcolor("grey", alpha = 0.2)

plot(locations1, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[1]*b,digits=2), "*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations1)*upper_coe_sca - mean(locations1)*down_coe_sca)/(length(qcov)-1)
plot(scales1, ylim = c(mean(locations1)*down_coe_sca, mean(locations1)*upper_coe_sca), pch = 16, col = "grey",main = paste0("scale=", round(mean(locations1)*down_coe_sca,digits=0), "+", round(b0_coefficients[1]*b01,digits=2), "*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations1)*down_coe_sca, par("usr")[2], mean(locations1)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations1)*down_coe_sca, 166,  mean(locations1)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers1,  xlab = "t", ylab = "Discharge (m3/s)", xlim = c(0, 166))


plot(locations4, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[1]*bmax,digits=2), "*t*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations4)*upper_coe_sca - mean(locations4)*down_coe_sca)/((length(qcov)-1)^2)
plot(scales4, ylim = c(mean(locations4)*down_coe_sca, mean(locations4)*upper_coe_sca), pch = 16, col = "grey",main = paste0("scale=", round(mean(locations1)*down_coe_sca,digits=0), "+", round(b0_coefficients[1]*b01,digits=2), "*t*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations4)*down_coe_sca, par("usr")[2], mean(locations4)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations4)*down_coe_sca, 166,  mean(locations4)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers4, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations7, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[1]*b,digits=2), ", b2=", round(b2_coefficients[1]*b,digits=2)), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations7)*upper_coe_sca - mean(locations7)*down_coe_sca)/(length(qcov)-1)
plot(scales7, ylim = c(mean(locations7)*down_coe_sca, mean(locations7)*upper_coe_sca), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[1]*b,digits=2), ", b2=", round(b2_coefficients[1]*b,digits=2)), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations7)*down_coe_sca, par("usr")[2], mean(locations7)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations7)*down_coe_sca, 166, mean(locations7)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers7, xlab = "t", ylab = "Discharge (m3/s)")




plot(locations2, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[2]*b,digits=2), "*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations2)*upper_coe_sca - mean(locations2)*down_coe_sca)/(length(qcov)-1)
plot(scales2, ylim = c(mean(locations2)*down_coe_sca, mean(locations2)*upper_coe_sca), pch = 16, col = "grey",main = paste0("scale=", round(mean(locations1)*down_coe_sca,digits=0), "+", round(b0_coefficients[2]*b01,digits=2), "*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations2)*down_coe_sca, par("usr")[2], mean(locations2)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations2)*down_coe_sca, 166,  mean(locations2)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers2,  xlab = "t", ylab = "Discharge (m3/s)", xlim = c(0, 166))


plot(locations5, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[2]*bmax,digits=2), "*t*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations5)*upper_coe_sca - mean(locations5)*down_coe_sca)/((length(qcov)-1)^2)
plot(scales5, ylim = c(mean(locations5)*down_coe_sca, mean(locations5)*upper_coe_sca), pch = 16, col = "grey",main = paste0("scale=", round(mean(locations1)*down_coe_sca,digits=0), "+", round(b0_coefficients[2]*b01,digits=2), "*t*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations5)*down_coe_sca, par("usr")[2], mean(locations5)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations5)*down_coe_sca, 166,  mean(locations5)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers5, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations8, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[2]*b,digits=2), ", b2=", round(b2_coefficients[2]*b,digits=2)), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations8)*upper_coe_sca - mean(locations8)*down_coe_sca)/(length(qcov)-1)
plot(scales8, ylim = c(mean(locations8)*down_coe_sca, mean(locations8)*upper_coe_sca), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[2]*b,digits=2), ", b2=", round(b2_coefficients[2]*b,digits=2)), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations8)*down_coe_sca, par("usr")[2], mean(locations8)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations8)*down_coe_sca, 166, mean(locations8)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers8, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations3, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[3]*b,digits=2), "*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations3)*upper_coe_sca - mean(locations3)*down_coe_sca)/(length(qcov)-1)
plot(scales3, ylim = c(mean(locations3)*down_coe_sca, mean(locations3)*upper_coe_sca), pch = 16, col = "grey",main = paste0("scale=", round(mean(locations1)*down_coe_sca,digits=0), "+", round(b0_coefficients[3]*b01,digits=2), "*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations3)*down_coe_sca, par("usr")[2], mean(locations3)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations3)*down_coe_sca, 166,  mean(locations3)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers3,  xlab = "t", ylab = "Discharge (m3/s)", xlim = c(0, 166))


plot(locations6, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("location=", round(down_coe_loc*inital_location,digits=0), "+", round(b0_coefficients[3]*bmax,digits=2), "*t*t"), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations6)*upper_coe_sca - mean(locations6)*down_coe_sca)/((length(qcov)-1)^2)
plot(scales6, ylim = c(mean(locations6)*down_coe_sca, mean(locations6)*upper_coe_sca), pch = 16, col = "grey",main = paste0("scale=", round(mean(locations1)*down_coe_sca,digits=0), "+", round(b0_coefficients[3]*b01,digits=2), "*t*t"), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations6)*down_coe_sca, par("usr")[2], mean(locations5)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations6)*down_coe_sca, 166,  mean(locations6)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers6, xlab = "t", ylab = "Discharge (m3/s)")



plot(locations9, ylim = c(downboundary, upperboudary), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[3]*b,digits=2), ", b2=", round(b2_coefficients[3]*b,digits=2)), xlab = "t", ylab = "Location")
rect(par("usr")[1], downboundary, par("usr")[2], upperboudary, col = col, border = NA)
segments(0, downboundary, 166, upperboudary, col = "black",lwd=1, lty = 2)
b01 <- (mean(locations9)*upper_coe_sca - mean(locations9)*down_coe_sca)/(length(qcov)-1)
plot(scales9, ylim = c(mean(locations9)*down_coe_sca, mean(locations9)*upper_coe_sca), pch = 16, col = "grey",main = paste0("b1=", round(b1_coefficients[3]*b,digits=2), ", b2=", round(b2_coefficients[3]*b,digits=2)), xlab = "t", ylab = "Scale")
rect(par("usr")[1], mean(locations9)*down_coe_sca, par("usr")[2], mean(locations9)*upper_coe_sca, col = col, border = NA)
segments(0, mean(locations9)*down_coe_sca, 166, mean(locations9)*upper_coe_sca, col = "black",lwd=1, lty = 2)
plot(random_numbers9, xlab = "t", ylab = "Discharge (m3/s)")
