##############################
#GAMLSS fitting approaches: 
# Stationary and Non-Stationary
#Input: dataTemp (a dataframe containing two columns: maxVal and year)
#this function using the LOGNO, GU, GA, WEI to fit the data, 4
#conditions, 1 constant, 2 changing mu, 3 changing sigma,4 changing both,
#return the dataTemp with some adding columns 
#(mu sigma aic sbc under all conditions)
fitted_infor_4models_4conditions <- function(dataTemp) {
  #############
  ###four distibutions with two-parameters
  # LOGNO GU (identity, log); GA WEI (log, log)
  
  Q <- dataTemp$maxVal
  qcov <- dataTemp$year
  
  # LOGNO,  constant u sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~1,family = LOGNO());summary(estGAM) 
  
  dataTemp$mu_1_LOGNO <- estGAM[["mu.coefficients"]][1]; dataTemp$sigma_1_LOGNO <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_1_LOGNO <- estGAM$aic; dataTemp$SBC_1_LOGNO <-  estGAM$sbc
  
  
  # LOGNO,  time-varying u  constant sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~1,family = LOGNO());summary(estGAM) 
  
  dataTemp$mu_2_LOGNO <- estGAM[["mu.coefficients"]][1] + estGAM[["mu.coefficients"]][2]*qcov; 
  dataTemp$sigma_2_LOGNO <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_2_LOGNO <- estGAM$aic; dataTemp$SBC_2_LOGNO <-  estGAM$sbc
  
  
  # LOGNO,   constant u  time-varying sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~qcov,family = LOGNO());summary(estGAM)
  
  dataTemp$mu_3_LOGNO <- estGAM[["mu.coefficients"]][1]
  dataTemp$sigma_3_LOGNO <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_3_LOGNO <- estGAM$aic; dataTemp$SBC_3_LOGNO <-  estGAM$sbc
  
  # LOGNO,   time-varying  u   sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~qcov,family = LOGNO());summary(estGAM)
  
  dataTemp$mu_4_LOGNO <-  estGAM[["mu.coefficients"]][1]+ estGAM[["mu.coefficients"]][2]*qcov; 
  dataTemp$sigma_4_LOGNO <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_4_LOGNO <- estGAM$aic; dataTemp$SBC_4_LOGNO <-  estGAM$sbc
  
  
  
  # GU,  constant u sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~1,family = GU());summary(estGAM) 
  
  dataTemp$mu_1_GU <- estGAM[["mu.coefficients"]][1]; dataTemp$sigma_1_GU <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_1_GU <- estGAM$aic; dataTemp$SBC_1_GU <-  estGAM$sbc
  
  
  # GU,  time-varying u  constant sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~1,family = GU());summary(estGAM) 
  
  dataTemp$mu_2_GU <- estGAM[["mu.coefficients"]][1] + estGAM[["mu.coefficients"]][2]*qcov; 
  dataTemp$sigma_2_GU <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_2_GU <- estGAM$aic; dataTemp$SBC_2_GU <-  estGAM$sbc
  
  
  # GU,   constant u  time-varying sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~qcov,family = GU());summary(estGAM)
  
  dataTemp$mu_3_GU <- estGAM[["mu.coefficients"]][1]
  dataTemp$sigma_3_GU <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_3_GU <- estGAM$aic; dataTemp$SBC_3_GU <-  estGAM$sbc
  
  # GU,   time-varying  u   sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~qcov,family = GU());summary(estGAM)
  
  dataTemp$mu_4_GU <-  estGAM[["mu.coefficients"]][1]+ estGAM[["mu.coefficients"]][2]*qcov; 
  dataTemp$sigma_4_GU <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_4_GU <- estGAM$aic; dataTemp$SBC_4_GU <-  estGAM$sbc
  
  # GA,  constant u sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~1,family = GA());summary(estGAM) 
  
  dataTemp$mu_1_GA <- exp(estGAM[["mu.coefficients"]][1]); dataTemp$sigma_1_GA <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_1_GA <- estGAM$aic; dataTemp$SBC_1_GA <-  estGAM$sbc
  
  # GA,  time-varying u  constant sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~1,family = GA());summary(estGAM) 
  
  dataTemp$mu_2_GA <- exp(estGAM[["mu.coefficients"]][1]+ estGAM[["mu.coefficients"]][2]*qcov); 
  dataTemp$sigma_2_GA <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_2_GA <- estGAM$aic; dataTemp$SBC_2_GA <-  estGAM$sbc
  
  # GA,   constant u  time-varying sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~qcov,family = GA());summary(estGAM)
  
  dataTemp$mu_3_GA <- exp(estGAM[["mu.coefficients"]][1])
  dataTemp$sigma_3_GA <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_3_GA <- estGAM$aic; dataTemp$SBC_3_GA <-  estGAM$sbc
  
  # GA,   time-varying  u   sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~qcov,family = GA());summary(estGAM)
  
  dataTemp$mu_4_GA <-  exp(estGAM[["mu.coefficients"]][1]+ estGAM[["mu.coefficients"]][2]*qcov); 
  dataTemp$sigma_4_GA <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_4_GA <- estGAM$aic; dataTemp$SBC_4_GA <-  estGAM$sbc
  
  
  # WEI,  constant u sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~1,family = WEI());summary(estGAM) 
  
  dataTemp$mu_1_WEI <- exp(estGAM[["mu.coefficients"]][1]); dataTemp$sigma_1_WEI <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_1_WEI <- estGAM$aic; dataTemp$SBC_1_WEI <-  estGAM$sbc
  
  
  # WEI,  time-varying u  constant sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~1,family = WEI());summary(estGAM) 
  
  dataTemp$mu_2_WEI <- exp(estGAM[["mu.coefficients"]][1]+ estGAM[["mu.coefficients"]][2]*qcov); 
  dataTemp$sigma_2_WEI <- exp(estGAM[["sigma.coefficients"]][1])
  dataTemp$AIC_2_WEI <- estGAM$aic; dataTemp$SBC_2_WEI <-  estGAM$sbc
  
  
  # WEI,   constant u  time-varying sigma
  estGAM<-gamlss(Q~1,sigma.formula = ~qcov,family = WEI());summary(estGAM)
  
  dataTemp$mu_3_WEI <- exp(estGAM[["mu.coefficients"]][1])
  dataTemp$sigma_3_WEI <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_3_WEI <- estGAM$aic; dataTemp$SBC_3_WEI <-  estGAM$sbc
  
  # WEI,   time-varying  u   sigma
  estGAM<-gamlss(Q~qcov,sigma.formula = ~qcov,family = WEI());summary(estGAM)
  
  dataTemp$mu_4_WEI <-  exp(estGAM[["mu.coefficients"]][1]+ estGAM[["mu.coefficients"]][2]*qcov); 
  dataTemp$sigma_4_WEI <- exp(estGAM[["sigma.coefficients"]][1] + estGAM[["sigma.coefficients"]][2]*qcov)
  dataTemp$AIC_4_WEI <- estGAM$aic
  dataTemp$SBC_4_WEI <-  estGAM$sbc
  
  return(dataTemp) 
}


##############################
#reject_outliers_method1, dataTemp  -> chunk_size (e.g., 30) as one group, 
#then determine the discharge at the given threshold_returnPeriod at each group. 
#and set the records > discharge as NA at each group, 
#then combine them and return the dataframe results
Reject_outliers_method1 <- function(dataTemp,threshold_returnPeriod,chunk) {
  if(ceiling(nrow(dataTemp)/chunk)==1){
    dataTemp$r <- 1
  }else{
    dataTemp$r <- cut(seq_along(dataTemp$maxVal), 
                      breaks = ceiling(nrow(dataTemp)/chunk), labels = FALSE)
  }
  
  
  d <- split(dataTemp, dataTemp$r)
  
  # Calculate return levels for each sample
  return_levels <- lapply(d, function(sample) {
    fit_gev <- fevd(sample$maxVal)
    return.level(fit_gev, return.period = threshold_returnPeriod)})
  # Extract upper outliers
  b <- as.numeric(unlist(return_levels))
  
  tempData <- bind_rows(lapply(seq_along(d), function(i) {
    d_chunk <- d[[i]]
    b_chunk <- b[i]
    subset <- d_chunk[d_chunk$maxVal > b_chunk,]
    if(nrow(subset) > 0) {subset} else {NULL}}))
  
  
  results<-dataTemp
  results[dataTemp$maxVal %in% tempData$maxVal,]<-NA
  
  return(results)
}


##############################
###reject_outliers_method2, dataTemp and window_size as 20.
# we set the frequency as 2, once larger than 2, set NA
# return dataTemp (year, maxVal, and frequency)
Reject_outliers_method2 <- function(dataTemp,window_size,threshold_times) {
  
  maxVal <- dataTemp$maxVal
  
  rst <- list()
  for (i in 1:(length(maxVal)-window_size+1)) {
    cd <- maxVal[i:(i+window_size-1)]
    for (j in 1:window_size) {
      modified_list <- cd[-j]
      fit_gev <- fevd(modified_list)
      test<-fit_gev$results$par
      k<-qevd(0.99, loc =test[1] , scale = test[2], shape = test[3], type = c("GEV"))
      rst <- append(rst, k)}
  }
  
  sublist <- split(rst, rep(1:(length(maxVal)-window_size+1), each = window_size))
  converted_data <- lapply(sublist, function(sublist) as.numeric(unlist(sublist)))
  
  result <- lapply(converted_data, function(sublist) {
    # Convert sublist to a matrix or dist object
    sublist_matrix <- matrix(unlist(sublist), ncol = 1)  # Assuming each sublist is a column vector
    
    # Apply the LOF algorithm
    mlof <- lof(sublist_matrix, k = (window_size-1))
    
    # Determine the threshold
    thr <- quantile(mlof, 0.97)
    
    # Identify the outliers
    out_index <- which(mlof >= thr)
  })
  
  year_list<-dataTemp$year
  
  year_mw <- list()
  for (i in 1:(length(year_list) - window_size + 1)) {
    sublist <- year_list[i:(i + window_size - 1)]
    year_mw[[i]] <- sublist}
  
  get_values <- function(index_list_result, year_mw) {
    values <- year_mw[index_list_result]
    return(values)}
  
  # Apply the indexing operation for each pair of sublists in A and B
  result_1<- mapply(get_values,result, year_mw)
  

  counts <- table(result_1)
  
  
  # Sort the counts in descending order
  sorted_counts <- sort(counts, decreasing = TRUE)
  
  sorted_counts <- as.data.frame(sorted_counts)
  
  names(sorted_counts) <- c("year","frequency")
  
  dataTemp <- merge(dataTemp,sorted_counts,by="year",all = TRUE)
  
  dataTemp$maxVal[dataTemp$frequency > threshold_times] <- NA
  
  return(dataTemp)
}


##############################
###Discharge_at_GivenReturnPeriodandSamples: 
#Moving_window to fit the curves
#Inputs:   dataTemp: year, maxVal, "r";
#window_size = 30 or other value; return_periods = c(100, 10)
#output: discharges at_GivenReturnPeriodSamples 
Discharge_at_GivenReturnPeriodandSamples <- function(dataTemp,
                                                     window_size,
                                                     return_periods) {
  ret_list <- list()
  
  for (i in 1:(nrow(dataTemp)-window_size+1)){
    current_data <- dataTemp[i:(i+window_size-1), , drop=FALSE]
    current_data <- na.omit(current_data)
    fit_gev <- fevd(current_data$maxVal,current_data)
    temp <- return.level(fit_gev,return.period=return_periods)
    
    ret <- as.list(temp)
    ret_list <- append(ret_list, ret)    # 137*3=411
  }
  
  
  vec <- unlist(ret_list)
  df <- data.frame(Name = names(vec), Value = as.numeric(vec))
  
  
  df$rowid <- 1:nrow(df)
  df_wide <- dcast(df, rowid ~ Name, value.var = "Value")
  df_wide$rowid <- NULL
  
  new_cols <- list()
  for (col in names(df_wide)) {
    new_cols[[col]] <- na.omit(df_wide[[col]])}
  df <- do.call(data.frame, new_cols)
  
  colnames(df) <- colnames(df_wide)
  colnames(df) <- paste0("return_", colnames(df))
  
  return(df)
}


##############################
##flood extreme estimate with GAMLSS method

GamlssFigure1Data <- function(raw_data,baseyear){
  data_info <- fitted_infor_4models_4conditions(raw_data)
  
  # We donot use the stationary model for GAMLSS, If you wanna use it, please uncommentted the lines.
  
  model_summary <- data_info[, c('SBC_1_WEI', 'SBC_2_WEI','SBC_3_WEI','SBC_4_WEI','SBC_1_LOGNO',
                                 'SBC_2_LOGNO','SBC_3_LOGNO','SBC_4_LOGNO','SBC_1_GU','SBC_2_GU',
                                 'SBC_3_GU','SBC_4_GU','SBC_1_GA','SBC_2_GA','SBC_3_GA','SBC_4_GA')]
  
  # only choose the non-stationary condition
  model_summary <- data_info[, c('SBC_2_WEI','SBC_3_WEI','SBC_4_WEI',
                                 'SBC_2_LOGNO','SBC_3_LOGNO','SBC_4_LOGNO','SBC_2_GU',
                                 'SBC_3_GU','SBC_4_GU','SBC_2_GA','SBC_3_GA','SBC_4_GA')]
  # 
  # model_summary <- data_info[, c('SBC_1_WEI',
  #                                'SBC_1_LOGNO','SBC_1_GU','SBC_1_GA')]
  # 
  model_summary <-  as.data.frame(model_summary)
  best_model_info <- names(which.min(model_summary[1, ]))
  variables <- strsplit(best_model_info, "_")[[1]]
  parameter_id <- as.numeric(variables[2])
  model_id <- variables[3]
  
  ### here we select that best model, its mu and sigma
  tt <- paste("data_second <- data_info[, c('year', 'maxVal',", 
              paste0("'mu_", parameter_id, "_", model_id, "', '", "sigma_", 
                     parameter_id, "_", model_id, "'", collapse = ", "), ")]", sep = "")
  
  eval(parse(text = tt))
  
  names(data_second) <- c("year","maxVal","mu","sigma")
  
  ### we get the discharge for each year at 20- 50- 100- return periods
  
  ### define the year 
  
  if(model_id == "GA"){
    data_second$flood_magnitude_100yr <- qGA(0.99, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50yr <- qGA(0.98, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_20yr <- qGA(0.95, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_90th <- qGA(0.90, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_80th <- qGA(0.80, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50th <- qGA(0.50, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_1_1yr <- qGA(0.01, mu = data_second$mu, sigma = data_second$sigma)
    
    ### select discharge at a given return period and in a reference year
    index = which(data_second$year==baseyear) 
    
    base_discharge_50yr <- data_second$flood_magnitude_50yr[index]
    base_discharge_100yr <- data_second$flood_magnitude_100yr[index]
    base_discharge_20yr <- data_second$flood_magnitude_20yr[index]
    
    data_second$return_period_at_100yr_discharge_in_reference_year <- 1/(1-pGA(base_discharge_100yr, mu = data_second$mu, 
                                                                               sigma = data_second$sigma))
    data_second$return_period_at_50yr_discharge_in_reference_year <- 1/(1-pGA(base_discharge_50yr, mu = data_second$mu, 
                                                                              sigma = data_second$sigma))
    data_second$return_period_at_20yr_discharge_in_reference_year <- 1/(1-pGA(base_discharge_20yr, mu = data_second$mu, 
                                                                              sigma = data_second$sigma))
  }else if (model_id == "GU"){
    data_second$flood_magnitude_100yr <- qGU(0.99, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50yr <- qGU(0.98, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_20yr <- qGU(0.95, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_90th <- qGU(0.90, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_80th <- qGU(0.80, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50th <- qGU(0.50, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_1_1yr <- qGU(0.01, mu = data_second$mu, sigma = data_second$sigma)
    
    ### select discharge at a given return period and in a reference year
    index = which(data_second$year==baseyear) #### you may change the year here!!!!!
    
    base_discharge_50yr <- data_second$flood_magnitude_50yr[index]
    base_discharge_100yr <- data_second$flood_magnitude_100yr[index]
    base_discharge_20yr <- data_second$flood_magnitude_20yr[index]
    
    data_second$return_period_at_100yr_discharge_in_reference_year <- 1/(1-pGU(base_discharge_100yr, mu = data_second$mu, 
                                                                               sigma = data_second$sigma))
    data_second$return_period_at_50yr_discharge_in_reference_year <- 1/(1-pGU(base_discharge_50yr, mu = data_second$mu, 
                                                                              sigma = data_second$sigma))
    data_second$return_period_at_20yr_discharge_in_reference_year <- 1/(1-pGU(base_discharge_20yr, mu = data_second$mu, 
                                                                              sigma = data_second$sigma))
  }else if (model_id == "WEI"){
    data_second$flood_magnitude_100yr <- qWEI(0.99, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50yr <- qWEI(0.98, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_20yr <- qWEI(0.95, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_90th <- qWEI(0.90, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_80th <- qWEI(0.80, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50th <- qWEI(0.50, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_1_1yr <- qWEI(0.01, mu = data_second$mu, sigma = data_second$sigma)
    
    ### select discharge at a given return period and in a reference year
    index = which(data_second$year==baseyear) #### you may change the year here!!!!!
    
    base_discharge_50yr <- data_second$flood_magnitude_50yr[index]
    base_discharge_100yr <- data_second$flood_magnitude_100yr[index]
    base_discharge_20yr <- data_second$flood_magnitude_20yr[index]
    
    
    data_second$return_period_at_100yr_discharge_in_reference_year <- 1/(1-pWEI(base_discharge_100yr, 
                                                                                mu = data_second$mu, sigma = data_second$sigma))
    data_second$return_period_at_50yr_discharge_in_reference_year <- 1/(1-pWEI(base_discharge_50yr,
                                                                               mu = data_second$mu, sigma = data_second$sigma))
    data_second$return_period_at_20yr_discharge_in_reference_year <- 1/(1-pWEI(base_discharge_20yr, 
                                                                               mu = data_second$mu, sigma = data_second$sigma))
  }else if (model_id == "LOGNO"){
    data_second$flood_magnitude_100yr <- qLOGNO(0.99, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50yr <- qLOGNO(0.98, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_20yr <- qLOGNO(0.95, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_90th <- qLOGNO(0.90, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_80th <- qLOGNO(0.80, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_50th <- qLOGNO(0.50, mu = data_second$mu, sigma = data_second$sigma)
    data_second$flood_magnitude_1_1yr <- qLOGNO(0.01, mu = data_second$mu, sigma = data_second$sigma)
    
    ### select discharge at a given return period and in a reference year
    index = which(data_second$year==baseyear) #### you may change the year here!!!!!
    
    base_discharge_50yr <- data_second$flood_magnitude_50yr[index]
    base_discharge_100yr <- data_second$flood_magnitude_100yr[index]
    base_discharge_20yr <- data_second$flood_magnitude_20yr[index]
    
    
    data_second$return_period_at_100yr_discharge_in_reference_year <- 1/(1-pLOGNO(base_discharge_100yr, 
                                                                                  mu = data_second$mu, sigma = data_second$sigma))
    data_second$return_period_at_50yr_discharge_in_reference_year <- 1/(1-pLOGNO(base_discharge_50yr, 
                                                                                 mu = data_second$mu, sigma = data_second$sigma))
    data_second$return_period_at_20yr_discharge_in_reference_year <- 1/(1-pLOGNO(base_discharge_20yr,
                                                                                 mu = data_second$mu, sigma = data_second$sigma))
    
  }
  
  return(data_second)
}


#################
##### Using GAMlss Moving Window to estimate the 100yr-flood
GamlssMovingWindow <- function(raw_data,
                               window_size){
  
  for (i in 1:(nrow(raw_data)-window_size+1)){
    
    tryCatch({
 
      current_data <- raw_data[i:(i+window_size-1), , drop=FALSE]
      # current_data2 <- na.omit(current_data)
      
      data_second <- GamlssFigure1Data(current_data,baseyear = min(current_data$year))
      
      ## check if NA or not.
      resulttemp <- data.frame(year = data_second$year[window_size],
                               flood_100yr = data_second$flood_magnitude_100yr[window_size])
      
      if(i==1){
        resultsALL <- resulttemp
      }else{
        resultsALL <- bind_rows(resultsALL,resulttemp)
      }
      
    }, error = function(e) {
      
      resulttemp <- data.frame(year = current_data$year[window_size],
                               flood_100yr = NA)
      resultsALL <- bind_rows(resultsALL,resulttemp)
      
      # Handle the error (optional)
      print(paste("Error occurred for element", i, ":"))
      print(e)
      
      # Continue to the next iteration
      return()
    })
    
  }
  return(resultsALL)
}


##############################
###Generating synthetic data with Non-stationary GEV model
# exp1 changing location
syntheticData_changingLocation <- function(location,scale,shape,
                                           upper_coe,down_coe,b0_coe,
                                           Type,raw_data,changingPoint,changingPoint_1,changingPoint_2,b1_coe,b2_coe) {
  
  qcov<- raw_data$year
  
  # check the initial_location, scale, and shape value.
  if(location<0 ||scale<0){
    print("GEV parameters should be postive!")
    return(0)
  }
  if(scale>0.5*location){
    print("Scale is too large compared to location, Please reset!")
    return(0)
  }
  
  upperboudary <- upper_coe*location
  downboundary <- down_coe*location
  
  # Choose the type for generating fake data
  if(Type=="Linear"){
    # y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b <- b0_coe*bmax
    
    locations <- (qcov-min(qcov))*b+downboundary
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scale, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scale, shape = shape, lower.tail = TRUE)
    
  }else if(Type=="Power"){
    # y=bx^2+b0
    bmax <- (upperboudary-downboundary)/((length(qcov)-1)^2)
    b <- b0_coe*bmax
    locations <- ((qcov-min(qcov))^2)*b+downboundary
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scale, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scale, shape = shape,
                       lower.tail = TRUE)
  }else if(Type=="Partial"){
    # y=bx+b0 - y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b1 <- b1_coe*bmax
    b2 <- b2_coe*bmax
    locations_1 <- (qcov-min(qcov))*b1+downboundary
    locations_2 <- (qcov-min(qcov))*b2+downboundary
    locations <- c(locations_1[1:changingPoint],locations_2[(changingPoint+1):length(qcov)])
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scale, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scale, shape = shape, lower.tail = TRUE)
    
  }else if(Type=="Partial_1"){
    # y=bx+b0 - y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b1 <- b1_coe*bmax
    b2 <- b2_coe*bmax
    b3 <- b2_coe*(-bmax)
    
    locations_1 <- (qcov-min(qcov))*b1+downboundary
    locations_2 <- (qcov-min(qcov))*b2+downboundary
    locations_3 <-(qcov-min(qcov))*b3+downboundary
    
    locations <- c(locations_1[1:changingPoint_1],locations_3[(changingPoint_1+1):changingPoint_2],locations_2[(changingPoint_2+1):length(qcov)])
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scale, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scale, shape = shape, lower.tail = TRUE)
    
  }
  # result<-random_numbers
  final_result <- data.frame(random_numbers = random_numbers,
                             return_level = return_level,
                             location = locations,
                             scale = scale,
                             shape = shape)
  return(final_result)
}



########################
# exp2 changing scale
syntheticData_changingScale <- function(location,scale,shape,
                                        upper_coe,down_coe,b0_coe,
                                        Type,raw_data,changingPoint,changingPoint_1,changingPoint_2,b1_coe,b2_coe) {
  
  qcov<- raw_data$year
  
  # check the initial_location, scale, and shape value.
  if(location<0 ||scale<0){
    print("GEV parameters should be postive!")
    return(0)
  }
  if(scale>0.5*location||scale<0.02*location){
    print("Scale is too large or too small compared to location, Please reset!")
    return(0)
  }
  
  upperboudary <- upper_coe*scale
  downboundary <- down_coe*scale
  
  # Choose the type for generating fake data
  if(Type=="Linear"){
    # y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b <- b0_coe*bmax
    
    scales <- (qcov-min(qcov))*b+downboundary
    random_numbers <- rgev(n = length(qcov), loc = location, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = location, scale = scales, shape = shape, lower.tail = TRUE)
    # infoLine <- trendline_sum(seq(1,length(random_numbers_1)),random_numbers_1)
    # infoLine$parameter$a
  }else if(Type=="Power"){
    # y=bx^2+b0
    bmax <- (upperboudary-downboundary)/((length(qcov)-1)^2)
    b <- b0_coe*bmax
    scales <- ((qcov-min(qcov))^2)*b+downboundary
    random_numbers <- rgev(n = length(qcov), loc = location, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = location, scale = scales, shape = shape,lower.tail = TRUE)
  }else if(Type=="Partial"){
    # y=bx+b0 - y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b1 <- b1_coe*bmax
    b2 <- b2_coe*bmax
    scales_1 <- (qcov-min(qcov))*b1+downboundary
    scales_2 <- (qcov-min(qcov))*b2+downboundary
    scales <- c(scales_1[1:changingPoint],scales_2[(changingPoint+1):length(qcov)])
    random_numbers <- rgev(n = length(qcov), loc = location, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = location, scale = scales, shape = shape,lower.tail = TRUE)
  }else if(Type=="Partial_1"){
    # y=bx+b0 - y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b1 <- b1_coe*bmax
    b2 <- b2_coe*bmax
    b3 <- b2_coe*(-bmax)
    
    scales_1 <- (qcov-min(qcov))*b1+downboundary
    scales_2 <- (qcov-min(qcov))*b2+downboundary
    scales_3 <-(qcov-min(qcov))*b3+downboundary
    
    scales <- c(scales_1[1:changingPoint_1],scales_3[(changingPoint_1+1):changingPoint_2],scales_2[(changingPoint_2+1):length(qcov)])
    random_numbers <- rgev(n = length(qcov), loc = location, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = location, scale = scales, shape = shape, lower.tail = TRUE)
    
  }
  final_result <- data.frame(random_numbers = random_numbers,
                             return_level = return_level,
                             location = location,
                             scale = scales,
                             shape = shape)
  return(final_result)
}

########################
# exp3 changing location and scale
syntheticData_changingLocationscale <- function(location,scale,shape,
                                                upper_coe,down_coe,b0_coe, upper_coe_1,down_coe_1,
                                                Type,raw_data,changingPoint,changingPoint_1,changingPoint_2,b1_coe,b2_coe) {
  
  qcov<- raw_data$year
  
  # check the initial_location, scale, and shape value.
  if(location<0){
    print("GEV parameters should be postive!")
    return(0)
  }
  if(scale>0.5*location){
    print("Scale is too large compared to location, Please reset!")
    return(0)
  }
  
  upperboudary <- upper_coe*location
  downboundary <- down_coe*location
  
  # Choose the type for generating fake data
  if(Type=="Linear"){
    # y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b <- b0_coe*bmax
    locations <- (qcov-min(qcov))*b+downboundary
    
    new_location<-mean(locations)
    upperboudary_1<- upper_coe_1*new_location 
    downboundary_1<- down_coe_1*new_location
    bmax_1<-(upperboudary_1-downboundary_1)/(length(qcov)-1)
    b_1 <- b0_coe*bmax_1
    scales<- (qcov-min(qcov))*b_1+downboundary_1
    
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scales, shape = shape,
                       lower.tail = TRUE)
  }else if(Type=="Power"){
    # y=bx^2+b0
    bmax <- (upperboudary-downboundary)/((length(qcov)-1)^2)
    b <- b0_coe*bmax
    locations <- ((qcov-min(qcov))^2)*b+downboundary
    
    new_location<-mean(locations)
    upperboudary_1<- upper_coe_1*new_location 
    downboundary_1<- down_coe_1*new_location
    bmax_1<-(upperboudary_1-downboundary_1)/((length(qcov)-1)^2)
    b_1 <- b0_coe*bmax_1
    scales<- ((qcov-min(qcov))^2)*b_1+downboundary_1
    
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scales, shape = shape,
                       lower.tail = TRUE)
  }else if(Type=="Partial"){
    # y=bx+b0 - y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b1 <- b1_coe*bmax
    locations_1 <- (qcov-min(qcov))*b1+downboundary
    new_location_1<-mean(locations_1)
    
    upperboudary_1<- upper_coe_1*new_location_1 
    downboundary_1<- down_coe_1*new_location_1
    
    bmax_1<-(upperboudary_1-downboundary_1)/(length(qcov)-1)
    b_1 <- b0_coe*bmax_1
    scale_1<- (qcov-min(qcov))*b_1+downboundary_1
    
    
    b2 <- b2_coe*bmax
    locations_2 <- (qcov-min(qcov))*b2+downboundary
    new_location_2<-mean(locations_2)
    upperboudary_2<- upper_coe_1*new_location_2 
    downboundary_2<- down_coe_1*new_location_2
    bmax_2<-(upperboudary_2-downboundary_2)/(length(qcov)-1)
    b_2 <- b0_coe*bmax_2
    scale_2<- (qcov-min(qcov))*b_2+downboundary_2
    
    locations <- c(locations_1[1:changingPoint],locations_2[(changingPoint+1):length(qcov)])
    scales <- c(scale_1[1:changingPoint],scale_2[(changingPoint+1):length(qcov)])
    
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scales, shape = shape,
                       lower.tail = TRUE)
    
  }else if(Type=="Partial_1"){
    # y=bx+b0 - y=bx+b0
    bmax <- (upperboudary-downboundary)/(length(qcov)-1)
    b1 <- b1_coe*bmax
    locations_1 <- (qcov-min(qcov))*b1+downboundary
    new_location_1<-mean(locations_1)
    
    upperboudary_1<- upper_coe_1*new_location_1 
    downboundary_1<- down_coe_1*new_location_1
    
    bmax_1<-(upperboudary_1-downboundary_1)/(length(qcov)-1)
    b_1 <- b0_coe*bmax_1
    scale_1<- (qcov-min(qcov))*b_1+downboundary_1
    
    
    b2 <- b2_coe*bmax
    locations_2 <- (qcov-min(qcov))*b2+downboundary
    new_location_2<-mean(locations_2)
    upperboudary_2<- upper_coe_1*new_location_2 
    downboundary_2<- down_coe_1*new_location_2
    bmax_2<-(upperboudary_2-downboundary_2)/(length(qcov)-1)
    b_2 <- b0_coe*bmax_2
    scale_2<- (qcov-min(qcov))*b_2+downboundary_2
    
    b3<-(b1_coe+b2_coe)/2*(-bmax)
    # b3<-b1_coe*(-bmax)
    locations_3 <-(qcov-min(qcov))*b3+downboundary
    new_location_3<-mean(locations_3)
    upperboudary_3<- upper_coe_1*new_location_3 
    downboundary_3<- down_coe_1*new_location_3
    bmax_3<-(upperboudary_3-downboundary_3)/(length(qcov)-1)
    b_3 <- b0_coe*(-bmax_1)
    scale_3<- (qcov-min(qcov))*b_3+downboundary_3
    
    
    locations <- c(locations_1[1:changingPoint_1],locations_3[(changingPoint_1+1):changingPoint_2],locations_2[(changingPoint_2+1):length(qcov)])
    scales <- c(scale_1[1:changingPoint_1],scale_3[(changingPoint_1+1):changingPoint_2],scale_2[(changingPoint_2+1):length(qcov)])
    
    random_numbers <- rgev(n = length(qcov), loc = locations, scale = scales, shape = shape)
    return_level<-qgev(0.99, loc = locations, scale = scales, shape = shape, lower.tail = TRUE)
    
  }
  final_result <- data.frame(random_numbers = random_numbers,
                             return_level = return_level,
                             location = locations,
                             scale = scales,
                             shape = shape)
  return(final_result)
}


##################
## evaluation GEV results.
evaluationmetric <- function(reswl_all){
  

  allmetrics1 <- gof(sim = reswl_all$`MWOutlier.I`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics2 <- gof(sim = reswl_all$`MWOutlier.II`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics3 <- gof(sim = reswl_all$MW,obs = reswl_all$Truth, na.rm=TRUE)
  
  
  
  evaluation_results <- data.frame(MAE_mwoutlier1 = allmetrics1[2],
                                   MAE_mwoutlier2 = allmetrics2[2],
                                   MAE_mw = allmetrics3[2],
                                   
                                   
                                   RMSE_mwoutlier1 = allmetrics1[4],
                                   RMSE_mwoutlier2 = allmetrics2[4],
                                   RMSE_mw = allmetrics3[4],
                                   
                                   PBIASpercent_mwoutlier1 = allmetrics1[6],
                                   PBIASpercent_mwoutlier2 = allmetrics2[6],
                                   PBIASpercent_mw = allmetrics3[6],
                                   
                                   NSE_mwoutlier1 = allmetrics1[9],
                                   NSE_mwoutlier2 = allmetrics2[9],
                                   NSE_mw = allmetrics3[9],
                                   
                                   
                                   r_mwoutlier1 = allmetrics1[16],
                                   r_mwoutlier2 = allmetrics2[16],
                                   r_mw = allmetrics3[16],
                                   
                                   
                                   r2_mwoutlier1 = allmetrics1[17],
                                   r2_mwoutlier2 = allmetrics2[17],
                                   r2_mw = allmetrics3[17])
  
  return(evaluation_results)
}


##################
## evaluation GAMLSS results.
evaluationmetricGAMLSS <- function(reswl_all){

  
  
  allmetrics1 <- gof(sim = reswl_all$`GAMLSS ALL`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics2 <- gof(sim = reswl_all$`GAMLSS 30yr`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics3 <- gof(sim = reswl_all$`GAMLSS 30yr Outlier I`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics4 <- gof(sim = reswl_all$`GAMLSS 30yr Outlier II`,obs = reswl_all$Truth, na.rm=TRUE)
 
  
  
  evaluation_results <- data.frame(MAE_gamlss = allmetrics1[2],
                                   MAE_gamlssMW = allmetrics2[2],
                                   MAE_gamlssMWOutlier1 = allmetrics3[2],
                                   MAE_gamlssMWOutlier2 = allmetrics4[2],
                                   
                                   
                                   RMSE_gamlss = allmetrics1[4],
                                   RMSE_gamlssMW = allmetrics2[4],
                                   RMSE_gamlssMWOutlier1 = allmetrics3[4],
                                   RMSE_gamlssMWOutlier2 = allmetrics4[4],
                                   
                                   
                                   PBIASpercent_gamlss = allmetrics1[6],
                                   PBIASpercent_gamlssMW = allmetrics2[6],
                                   PBIASpercent_gamlssMWOutlier1 = allmetrics3[6],
                                   PBIASpercent_gamlssMWOutlier2 = allmetrics4[6])
  
  return(evaluation_results)
}



##################
## evaluation GAMLSS results.
evaluationmetricGAMLSS_last30yr <- function(reswl_all){
  
  
  
  allmetrics1 <- gof(sim = reswl_all$`GAMLSS ALL`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics2 <- gof(sim = reswl_all$`GAMLSS 30yr`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics3 <- gof(sim = reswl_all$`GAMLSS 50yr`,obs = reswl_all$Truth, na.rm=TRUE)
  allmetrics4 <- gof(sim = reswl_all$`GAMLSS 100yr`,obs = reswl_all$Truth, na.rm=TRUE)
  
  
  
  evaluation_results <- data.frame(MAE_gamlss_ALL = allmetrics1[2],
                                   MAE_gamlss_30yr = allmetrics2[2],
                                   MAE_gamlss_50yr = allmetrics3[2],
                                   MAE_gamlss_100yr = allmetrics4[2],
                                   
                                   
                                   RMSE_gamlss_ALL = allmetrics1[4],
                                   RMSE_gamlss_30yr = allmetrics2[4],
                                   RMSE_gamlss_50yr = allmetrics3[4],
                                   RMSE_gamlss_100yr = allmetrics4[4],
                                   
                                   
                                   PBIASpercent_gamlss_ALL = allmetrics1[6],
                                   PBIASpercent_gamlss_30yr = allmetrics2[6],
                                   PBIASpercent_gamlss_50yr = allmetrics3[6],
                                   PBIASpercent_gamlss_100yr = allmetrics4[6])
  
  return(evaluation_results)
}
