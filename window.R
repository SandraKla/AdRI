###################################################################################################
####################################### Script for the Window-Methods #############################
###################################################################################################

####################################### Bootstrapping #############################################

#' Bootstrapping Confidence Intervals with package boot and 1000 Bootstrap replicates
#'
#' @param x Data for the Confidence Intervals
#' @param value Select for median or quantile
#' @param plot.it Plot the Bootstrapping
Boot_CI <- function(x, plot.it = FALSE){
  
  boot.quantile <- function(x, indices, ...){
    quantile(x[indices], ...)}
    
  boot_para <- boot(x, boot.quantile, R = 1000, probs = c(0.025, 0.975))
  
  if(plot.it){
    plot(boot_para)}
  
  # Calculate the quantile of the different bootstrapping values with Confidence Intervals
  ci <- apply(boot_para$t, MARGIN = 2, FUN = quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE)
  
  return(ci)
}


####################################### Regular Window-method #####################################

#' Regular Window-method with the modified.tukey() and three different calculation methods:
#' Nonparametric with Bootstrapping, Parametric or with the Hoffmann-Method 
#' 
#' @param data_ Given dataset:
#' 
#'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param window range of the window
#' @param method Nonparametric with Bootstrapping, Parametric or with the Hoffmann-Method
window_method <- function(data_, window, method){

  ##################################### Mean/Median ###############################################
  mean_with_outliers <-  data.frame()
  mean_with_tukey <-  data.frame() 
  ##################################### Quantiles #################################################
  quantiles_with_outliers <- data.frame()
  quantiles_with_tukey <- data.frame()
  ##################################### Standard derivation #######################################
  sd_with_outliers <- data.frame()
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_outliers_2_5 <- data.frame()
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_outliers_97_5 <- data.frame()
  confidence_with_tukey_97_5 <- data.frame()
  
  n_data <- data.frame()
  n_data_tukey <- data.frame()
  
  for(i in seq(min(data_[,4])+window, max(data_[,4])+window, by = window)){
    
    age_code <- i
    
    # The data subset 
    age_data <- subset(data_, data_[,4] <= i)                            # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > i-window)     # Below the lowest condition
    
    if(i-window == 0){
      age_data_ready <- subset(age_data, age_data$age_days >= i-window)}
    
    n <- nrow(age_data_ready)
    
    normal_log <- FALSE
    try(normal_log <- def.lognorm(age_data_ready$value, plot.it = FALSE)$lognorm)
    
    if(normal_log == TRUE){outliers_tukey <- modified.tukey(age_data_ready$value, plot.it = FALSE, log.mode = TRUE)}
    else{outliers_tukey <- modified.tukey(age_data_ready$value, plot.it = FALSE)}
    
    n_data <- rbind(n_data,nrow(age_data_ready))
    n_data_tukey <- rbind(n_data_tukey,length(outliers_tukey))
  
    ##################################### NONPARAMETRIC ###########################################
    if(method == "nonpara"){
    
      # Median (nonparametric)
      mean_tukey <- median(outliers_tukey)
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      mean_outlier <- median(age_data_ready$value)
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      
      # Quantile (nonparametric)
      quantiles_tukey <- quantile(outliers_tukey, probs = c(0.0275, 0.975))
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      quantiles_outlier <- quantile(age_data_ready$value, probs = c(0.0275, 0.975))
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)
      
      # 95% Confidence Interval with Bootstrapping
      try(confidence_tukey <- Boot_CI(outliers_tukey))
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,t(data.frame(confidence_tukey[,1][2:3])))
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,t(data.frame(confidence_tukey[,2][2:3])))
    
      try(confidence_outlier <- Boot_CI(age_data_ready$value))
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,t(data.frame(confidence_outlier[,1][2:3])))
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,t(data.frame(confidence_outlier[,2][2:3])))
    }
  
    ##################################### QQ-Plot #################################################
    if(method == "qqplot"){
   
      newx <- c(-1.96,0,1.96)
    
      # Make the QQ-Plot
      qqdata <- qqnorm(age_data_ready$value)
      lm_qqline <- lm(y~ x, data = qqdata)
    
      qqdata_tukey <- qqnorm(outliers_tukey)
      lm_qqline_tukey <- lm(y~x, data = qqdata_tukey)
      
      # Predict the RI and the Median
      result_qqline <- predict(lm_qqline, newdata=data.frame(x=newx), interval="confidence")
      result_qqline_tukey <- predict(lm_qqline_tukey, newdata=data.frame(x=newx), interval="confidence")
      
      # fit      lwr      upr
      # 1 20.69910 16.24553 25.15268 # 2.5% Reference interval
      # 2 38.68263 34.24530 43.11996 # 50% Median
      # 3 56.66616 52.21259 61.11974 # 97.5% Refernce interval
    
      # Median
      mean_tukey <- result_qqline_tukey[2,1]
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      mean_outlier <- result_qqline[2,1]
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
    
      # Quantile
      quantiles_tukey <- c(result_qqline_tukey[1,1], result_qqline_tukey[3,1])
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      quantiles_outlier <- c(result_qqline[1,1], result_qqline[3,1])
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)
    
      # 95% Confidence Interval
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,c(result_qqline_tukey[1,2], result_qqline_tukey[1,3]))
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,c(result_qqline_tukey[3,2], result_qqline_tukey[3,3]))
    
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,c(result_qqline[1,2], result_qqline[1,3]))
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,c(result_qqline[3,2], result_qqline[3,3]))
    }
  
    ##################################### PARAMETRIC ##############################################
    if(method == "para"){
    
      # Mean (parametric)
      mean_tukey <- mean(outliers_tukey)
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      mean_outlier <- mean(age_data_ready$value)
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      
      # Standard derivation (parametric)
      sd_outlier <- sd(age_data_ready$value)
      sd_with_outliers <- rbind(sd_with_outliers, sd_outlier)
      sd_tukey <- sd(outliers_tukey)
      sd_with_tukey <- sd(sd_with_tukey, sd_tukey)
      
      # Quantile (parametric) [mu - 1.96*sigma][mu + 1.96*sigma]
      quantiles_tukey <- mean_tukey + c(-1, 1) * qnorm(p = 0.975) * sd_tukey
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      quantiles_outlier <- mean_outlier + c(-1, 1) * qnorm(p = 0.975) * sd_outlier
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)

      # Error (parametric)
      error_outlier <- qnorm(0.975)*sd_outlier/sqrt(n)
      error_tukey <- qnorm(0.975)*sd_tukey/sqrt(n)
    
      # 95% Confidence Interval (parametric) [RI - error][RI + error]
      confidence_outliers_2_5 <- c(quantiles_outlier[1] - error_outlier, quantiles_outlier[1] + error_outlier)
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5, confidence_outliers_2_5)
      confidence_tukey_2_5 <- c(quantiles_tukey[1] - error_tukey, quantiles_tukey[1] + error_tukey)
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
      
      confidence_outliers_97_5 <- c(quantiles_outlier[2] - error_outlier, quantiles_outlier[2] + error_outlier)
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5, confidence_outliers_97_5)
      confidence_tukey_97_5 <- c(quantiles_tukey[2] - error_tukey, quantiles_tukey[2] + error_tukey)
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
    }
  }
 
  ##################################### Save the data #############################################
  
  # Window-Data without outlierdetection (add tail to plot the data)
  window_data <<- data.frame(age_days = seq(min(data_[,4]), age_code, by = window), 
                             mean = c(mean_with_outliers[,1], tail(mean_with_outliers[,1], n=1)), 
                             quantile1 = c(quantiles_with_outliers[,1],tail(quantiles_with_outliers[,1], n=1)),
                             quantile2 = c(quantiles_with_outliers[,2],tail(quantiles_with_outliers[,2], n=1)))
  
  # Window-Data with the modified.tukey()
  window_data_tukey <<- data.frame(age_days = seq(min(data_[,4]), age_code, by = window), 
                                   mean = c(mean_with_tukey[,1], tail(mean_with_tukey[,1], n=1)), 
                                   quantile1 = c(quantiles_with_tukey[,1],tail(quantiles_with_tukey[,1], n=1)),
                                   quantile2 = c(quantiles_with_tukey[,2],tail(quantiles_with_tukey[,2], n=1)))
  
  ##################################### Tables to download ########################################
  
  window_data_all_outlier <<- data.frame("Age-range from"         = seq(min(data_[,4]), age_code-1, by=window),
                                         "to [Days]"              = c(head(seq(min(data_[,4]) + window, age_code, by = window),-1),max(data_$age_days)),
                                         "Age-range from"         = round_df(seq(min(data_[,4]), age_code-1, by=window)/365,3),
                                         "to [Years]"             = c(round_df(head(seq(min(data_[,4]) + window, age_code, by = window)/365,-1),3), max(data_$age)),
                                         "Median"                 = c(mean_with_outliers[,1]), 
                                         "2.5 % RI"               = c(quantiles_with_outliers[,1]),
                                         "97.5% RI"               = c(quantiles_with_outliers[,2]),
                                         "95% CI (2.5% RI) from"  = c(confidence_with_outliers_2_5[,1]),
                                         "to"                     = c(confidence_with_outliers_2_5[,2]),
                                         "95% CI (97.5% RI) from" = c(confidence_with_outliers_97_5[,1]),
                                         "to"                     = c(confidence_with_outliers_97_5[,2]),
                                         "Number of data points"  = c(n_data[,1]), check.names = FALSE)
  
  window_data_all_tukey <<- data.frame("Age-range from"           = seq(min(data_[,4]), age_code-1, by=window),
                                       "to [Days]"                = c(head(seq(min(data_[,4]) + window, age_code, by = window),-1),max(data_$age_days)),
                                       "Age-range from"           = round_df(seq(min(data_[,4]), age_code-1, by=window)/365, 3),
                                       "to [Years]"               = c(round_df(head(seq(min(data_[,4]) + window, age_code, by = window)/365,-1),3), max(data_$age)),
                                       "Median"                   = c(mean_with_tukey[,1]), 
                                       "2.5 % RI"                 = c(quantiles_with_tukey[,1]),
                                       "97.5% RI"                 = c(quantiles_with_tukey[,2]),
                                       "95% CI (2.5% RI) from"    = c(confidence_with_tukey_2_5[,1]),
                                       "to"                       = c(confidence_with_tukey_2_5[,2]),
                                       "95% CI (97.5% RI) from"   = c(confidence_with_tukey_97_5[,1]),
                                       "to"                       = c(confidence_with_tukey_97_5[,2]),
                                       "Number of data points"    = c(n_data_tukey[,1]), check.names = FALSE)
  if(method == "para"){
    window_data_all_outlier <<- data.frame("Age-range from"         = seq(min(data_[,4]), age_code-1, by=window),
                                           "to [Days]"              = c(head(seq(min(data_[,4]) + window, age_code, by = window),-1),max(data_$age_days)),
                                           "Age-range from"         = round_df(seq(min(data_[,4]), age_code-1, by=window)/365, 3),
                                           "to [Years]"             = c(round_df(head(seq(min(data_[,4]) + window, age_code, by = window)/365,-1),3), max(data_$age)),
                                           "Mean"                   = c(mean_with_outliers[,1]), 
                                           "2.5 % RI"               = c(quantiles_with_outliers[,1]),
                                           "97.5% RI"               = c(quantiles_with_outliers[,2]),
                                           "95% CI (2.5% RI) from"  = c(confidence_with_outliers_2_5[,1]),
                                           "to"                     = c(confidence_with_outliers_2_5[,2]),
                                           "95% CI (97.5% RI) from" = c(confidence_with_outliers_97_5[,1]),
                                           "to"                     = c(confidence_with_outliers_97_5[,2]),
                                           "Number of data points"  = c(n_data[,1]), check.names = FALSE)
    
    window_data_all_tukey <<- data.frame("Age-range from"           = seq(min(data_[,4]), age_code-1, by=window),
                                         "to [Days]"                = c(head(seq(min(data_[,4]) + window, age_code, by = window),-1),max(data_$age_days)),
                                         "Age-range from"           = round_df(seq(min(data_[,4]), age_code-1, by=window)/365, 3),
                                         "to [Years]"               = c(round_df(head(seq(min(data_[,4]) + window, age_code, by = window)/365,-1),3), max(data_$age)),
                                         "Mean"                     = c(mean_with_tukey[,1]), 
                                         "2.5 % RI"                 = c(quantiles_with_tukey[,1]),
                                         "97.5% RI"                 = c(quantiles_with_tukey[,2]),
                                         "95% CI (2.5% RI) from"    = c(confidence_with_tukey_2_5[,1]),
                                         "to"                       = c(confidence_with_tukey_2_5[,2]),
                                         "95% CI (97.5% RI) from"   = c(confidence_with_tukey_97_5[,1]),
                                         "to"                       = c(confidence_with_tukey_97_5[,2]),
                                         "Number of data points"    = c(n_data_tukey[,1]), check.names = FALSE)}
}


####################################### Window-Method coupled to the Decision Tree ################

#' Window_method were the age groups are calculated with the help machine learning, more accurate 
#' Decision Tree from rpart(). This age groups are used to calculate the RI with these methods:
#' Nonparametric with Bootstrapping, Parametric or with the Hoffmann-Method 
#'
#' @param data_window_split Selected data
#' 
#' #'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param split Age group
#' @param method Nonparametric with Bootstrapping, Parametric or with the Hoffmann-Method
#' @param plot_log Plot the function def.lognorm() of each age groups
window_method_split <- function(data_window_split, split, method, plot_log = FALSE){
  
  ##################################### Mean/Median ###############################################
  mean_with_outliers <-  data.frame()
  mean_with_tukey <-  data.frame() 
  ##################################### Quantile ##################################################
  quantiles_with_outliers <- data.frame()
  quantiles_with_tukey <- data.frame() 
  ##################################### Standard derivation #######################################
  sd_with_outliers <- data.frame()
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_outliers_2_5 <- data.frame()
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_outliers_97_5 <- data.frame()
  confidence_with_tukey_97_5 <- data.frame()
  ##################################### Value of Box-Cox-Powertransformation ######################
  # boxcox_groups <- data.frame()
  n_data <- data.frame()
  n_data_tukey <- data.frame()
  
  for (i in seq(2,length(split))){
    
    age_code <- split[i]

    # The data subset 
    age_data <- subset(data_window_split, data_window_split$age_days <= split[i])  # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i-1])             # Below the lowest condition
    if(split[i-1] == 0){
      age_data_ready <- subset(age_data, age_data$age_days >= split[i-1])}  # Below the lowest condition
    if(!nrow(age_data_ready) == 0){
      if(plot_log){
        # Plot the distribution of each group to check for normally distribution
        try(def.lognorm(age_data_ready$value, plot.it = TRUE))}
        
        # Box-Cox Powertransformation
        # age_data_ready_box <<- age_data_ready
        # boxcox_ <- boxcox(lm(value ~ age_days, data = age_data_ready_box), plotit = FALSE)
        # boxcox_groups <- rbind(boxcox_groups, boxcox_$x[which.max(boxcox_$y)])
    }
    
    # Outlier-Detection with modified.tukey()
    
    normal_log <- FALSE
    try(normal_log <- def.lognorm(age_data_ready$value, plot.it = FALSE)$lognorm)
    
    if(normal_log == TRUE){outliers_tukey <- modified.tukey(age_data_ready$value, plot.it = FALSE, log.mode = TRUE)}
    else{outliers_tukey <- modified.tukey(age_data_ready$value, plot.it = FALSE)}
    
    n_data <- rbind(n_data,nrow(age_data_ready))
    n_data_tukey <- rbind(n_data_tukey,length(outliers_tukey))
    
    ################################### NONPARAMETRIC #############################################
    if(method == "nonpara"){
      
      # Tukey
      mean_tukey <- median(outliers_tukey) 
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      quantiles_tukey <- quantile(outliers_tukey, probs = c(0.0275, 0.975))
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
    
      # Without Outlierdetection
      mean_outlier <- median(age_data_ready$value)
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      quantiles_outlier <- quantile(age_data_ready$value, probs = c(0.0275, 0.975))
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)

      # 95% Confidence Interval with Bootstrapping
      confidence_tukey <- Boot_CI(outliers_tukey)
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,t(data.frame(confidence_tukey[,1][2:3])))
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,t(data.frame(confidence_tukey[,2][2:3])))
    
      confidence_outlier <- Boot_CI(age_data_ready$value)
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,t(data.frame(confidence_outlier[,1][2:3])))
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,t(data.frame(confidence_outlier[,2][2:3])))}
    
    ################################### PARAMETRIC ################################################
    if(method == "para"){
      
      n <- nrow(age_data_ready)
    
      # Tukey (Quantile (parametric) [mu - 1.96*sigma][mu + 1.96*sigma])
      mean_tukey <- median(outliers_tukey) 
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      sd_tukey <- sd(outliers_tukey)
      sd_with_tukey <- sd(sd_with_tukey, sd_tukey)
      quantiles_tukey <- mean_tukey + c(-1, 1) * qnorm(p = 0.975) * sd_tukey
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # Without Outlierdetection (Quantile (parametric) [mu - 1.96*sigma][mu + 1.96*sigma])
      mean_outlier <- median(age_data_ready$value)
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      sd_outlier <- sd(age_data_ready$value)
      sd_with_outliers <- rbind(sd_with_outliers, sd_outlier)
      quantiles_outlier <- mean_outlier + c(-1, 1) * qnorm(p = 0.975) * sd_outlier
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)
      
      # Error (parametric)
      error_outlier <- qnorm(0.975)*sd_outlier/sqrt(n)
      error_tukey <- qnorm(0.975)*sd_tukey/sqrt(n)
      
      # 95% Confidence Interval (parametric) [RI - error][RI + error] 
      confidence_outliers_2_5 <- c(quantiles_outlier[1] - error_outlier, quantiles_outlier[1] + error_outlier)
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5, confidence_outliers_2_5)
      confidence_tukey_2_5 <- c(quantiles_tukey[1] - error_tukey, quantiles_tukey[1] + error_tukey)
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
      
      confidence_outliers_97_5 <- c(quantiles_outlier[2] - error_outlier, quantiles_outlier[2] + error_outlier)
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5, confidence_outliers_97_5)
      confidence_tukey_97_5 <- c(quantiles_tukey[2] - error_tukey, quantiles_tukey[2] + error_tukey)
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
    }
    
    ################################### QQ-PLOT ###################################################
    if(method == "qqplot"){
      
      newx <- c(-1.96,0,1.96)
      
      qqdata <- qqnorm(age_data_ready$value, plot.it = FALSE)
      lm_qqline <- lm(y~ x, data = qqdata)
      result_qqline <- predict(lm_qqline, newdata=data.frame(x=newx), interval="confidence")
      
      qqdata_tukey <- qqnorm(outliers_tukey, plot.it = FALSE)
      lm_qqline_tukey <- lm(y~x, data = qqdata_tukey)
      result_qqline_tukey <- predict(lm_qqline_tukey, newdata=data.frame(x=newx), interval="confidence")
      
      # Tukey
      mean_tukey <- result_qqline_tukey[2,1]
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      quantiles_tukey <- c(result_qqline_tukey[1,1], result_qqline_tukey[3,1])
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # Without Outlierdetection
      mean_outlier <- result_qqline[2,1]
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      quantiles_outlier <- c(result_qqline[1,1], result_qqline[3,1])
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier) 
      
      # 95% Confidence Interval
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,c(result_qqline_tukey[1,2], result_qqline_tukey[1,3]))
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,c(result_qqline_tukey[3,2], result_qqline_tukey[3,3]))
      
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,c(result_qqline[1,2], result_qqline[1,3]))
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,c(result_qqline[3,2], result_qqline[3,3]))}
  }
  
  ##################################### Save the data #############################################

  # Window-Data without outlierdetection
  window_data_rpart <<- data.frame(age_days = split, mean = c(mean_with_outliers[,1], tail(mean_with_outliers[,1], n=1)),
                                   quantile1 = c(quantiles_with_outliers[,1], tail(quantiles_with_outliers[,1], n=1)),
                                   quantile2 = c(quantiles_with_outliers[,2], tail(quantiles_with_outliers[,2], n=1)))
  
  # Window-Data with modified.tukey()
  window_data_tukey_rpart <<- data.frame(age_days = split, mean = c(mean_with_tukey[,1],tail(mean_with_outliers[,1], n=1) ),
                                         quantile1 = c(quantiles_with_tukey[,1],  tail(quantiles_with_outliers[,1], n=1)),
                                         quantile2 = c(quantiles_with_tukey[,2], tail(quantiles_with_outliers[,2], n=1)))

  ##################################### Tables to download ########################################

  window_data_split_outlier <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                           "to [Days]"              = split[2:length(split)],
                                           "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                           "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                           "Median"                 = c(mean_with_outliers[,1]),
                                           "2.5 % RI"               = c(quantiles_with_outliers[,1]),
                                           "97.5% RI"               = c(quantiles_with_outliers[,2]),
                                           "95% CI (2.5% RI) from"  = c(confidence_with_outliers_2_5[,1]),
                                           "to"                     = c(confidence_with_outliers_2_5[,2]),
                                           "95% CI (97.5% RI) from" = c(confidence_with_outliers_97_5[,1]),
                                           "to"                     = c(confidence_with_outliers_97_5[,2]), 
                                           "Number of data points"  = c(n_data[,1]), check.names = FALSE)

  window_data_split_tukey <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                         "to [Days]"              = split[2:length(split)],
                                         "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                         "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                         "Median"                 = c(mean_with_tukey[,1]),
                                         "2.5 % RI"               = c(quantiles_with_tukey[,1]),
                                         "97.5% RI"               = c(quantiles_with_tukey[,2]),
                                         "95% CI (2.5% RI) from"  = c(confidence_with_tukey_2_5[,1]),
                                         "to"                     = c(confidence_with_tukey_2_5[,2]),
                                         "95% CI (97.5% RI) from" = c(confidence_with_tukey_97_5[,1]),
                                         "to"                     = c(confidence_with_tukey_97_5[,2]),
                                         "Number of data points"  = c(n_data_tukey[,1]), check.names = FALSE)
  
  if(method == "para"){
    window_data_split_outlier <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                             "to [Days]"              = split[2:length(split)],
                                             "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                             "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                             "Mean"                   = c(mean_with_outliers[,1]),
                                             "2.5 % RI"               = c(quantiles_with_outliers[,1]),
                                             "97.5% RI"               = c(quantiles_with_outliers[,2]),
                                             "95% CI (2.5% RI) from"  = c(confidence_with_outliers_2_5[,1]),
                                             "to"                     = c(confidence_with_outliers_2_5[,2]),
                                             "95% CI (97.5% RI) from" = c(confidence_with_outliers_97_5[,1]),
                                             "to"                     = c(confidence_with_outliers_97_5[,2]), 
                                             "Number of data points"  = c(n_data[,1]), check.names = FALSE)
    
    window_data_split_tukey <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                           "to [Days]"              = split[2:length(split)],
                                           "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                           "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                           "Mean"                   = c(mean_with_tukey[,1]),
                                           "2.5 % RI"               = c(quantiles_with_tukey[,1]),
                                           "97.5% RI"               = c(quantiles_with_tukey[,2]),
                                           "95% CI (2.5% RI) from"  = c(confidence_with_tukey_2_5[,1]),
                                           "to"                     = c(confidence_with_tukey_2_5[,2]),
                                           "95% CI (97.5% RI) from" = c(confidence_with_tukey_97_5[,1]),
                                           "to"                     = c(confidence_with_tukey_97_5[,2]), 
                                           "Number of data points"  = c(n_data_tukey[,1]), check.names = FALSE)}
}


#' Same as window_method_split()
#' 
#' @param split Age group from the LIS
#' @param method Nonparametric with Bootstrapping, Parametric or with the Hoffmann-Method
#' @param plot_log Plot the function def.lognorm() of each age groups
window_method_lis <- function(data_window_split, split, method, plot_log = FALSE){
  
  ##################################### Mean/Median ###############################################
  mean_with_outliers <-  data.frame()
  mean_with_tukey <-  data.frame() 
  ##################################### Quantile ##################################################
  quantiles_with_outliers <- data.frame()
  quantiles_with_tukey <- data.frame() 
  ##################################### Standard derivation #######################################
  sd_with_outliers <- data.frame()
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_outliers_2_5 <- data.frame()
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_outliers_97_5 <- data.frame()
  confidence_with_tukey_97_5 <- data.frame()
  ##################################### Value of Box-Cox-Powertransformation ######################
  # boxcox_groups <- data.frame()
  n_data <- data.frame()
  n_data_tukey <- data.frame()
  
  for (i in seq(2,length(split))){
    
    age_code <- split[i]
    
    # The data subset 
    age_data <- subset(data_window_split, data_window_split$age_days <= split[i])  # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i-1])                       # Below the lowest condition
    if(split[i-1] == 0){
      age_data_ready <- subset(age_data, age_data$age_days >= split[i-1])}  # Below the lowest condition
    
    if(!nrow(age_data_ready) == 0){
      if(plot_log){
        # Plot the distribution of each group to check for normally distribution
        try(def.lognorm(age_data_ready$value, plot.it = TRUE))}
      
      # Box-Cox Powertransformation
      # age_data_ready_box <<- age_data_ready
      # boxcox_ <- boxcox(lm(value ~ age_days, data = age_data_ready_box), plotit = FALSE)
      # boxcox_groups <- rbind(boxcox_groups, boxcox_$x[which.max(boxcox_$y)])
    }
    
   
    normal_log <- FALSE
    try(normal_log <- def.lognorm(age_data_ready$value, plot.it = FALSE)$lognorm)
    
    if(normal_log == TRUE){outliers_tukey <- modified.tukey(age_data_ready$value, plot.it = FALSE, log.mode = TRUE)}
    else{outliers_tukey <- modified.tukey(age_data_ready$value, plot.it = FALSE)}
    
    n_data <- rbind(n_data,nrow(age_data_ready))
    n_data_tukey <- rbind(n_data_tukey,length(outliers_tukey))
    
    ################################### NONPARAMETRIC #############################################
    if(method == "nonpara"){
      
      # Tukey
      mean_tukey <- median(outliers_tukey) 
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      quantiles_tukey <- quantile(outliers_tukey, probs = c(0.0275, 0.975))
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # Without Outlierdetection
      mean_outlier <- median(age_data_ready$value)
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      quantiles_outlier <- quantile(age_data_ready$value, probs = c(0.0275, 0.975))
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)
      
      # 95% Confidence Interval with Bootstrapping
      try(confidence_tukey <- Boot_CI(outliers_tukey))
      try(confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,t(data.frame(confidence_tukey[,1][2:3]))))
      try(confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,t(data.frame(confidence_tukey[,2][2:3]))))
      
      try(confidence_outlier <- Boot_CI(age_data_ready$value))
      try(confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,t(data.frame(confidence_outlier[,1][2:3]))))
      try(confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,t(data.frame(confidence_outlier[,2][2:3]))))}
    
    ################################### PARAMETRIC ################################################
    if(method == "para"){
      
      n <- nrow(age_data_ready)
      
      # Tukey (Quantile (parametric) [mu - 1.96*sigma][mu + 1.96*sigma])
      mean_tukey <- median(outliers_tukey) 
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      sd_tukey <- sd(outliers_tukey)
      sd_with_tukey <- sd(sd_with_tukey, sd_tukey)
      quantiles_tukey <- mean_tukey + c(-1, 1) * qnorm(p = 0.975) * sd_tukey
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # Without Outlierdetection (Quantile (parametric) [mu - 1.96*sigma][mu + 1.96*sigma])
      mean_outlier <- median(age_data_ready$value)
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      sd_outlier <- sd(age_data_ready$value)
      sd_with_outliers <- rbind(sd_with_outliers, sd_outlier)
      quantiles_outlier <- mean_outlier + c(-1, 1) * qnorm(p = 0.975) * sd_outlier
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier)
      
      # Error (parametric)
      error_outlier <- qnorm(0.975)*sd_outlier/sqrt(n)
      error_tukey <- qnorm(0.975)*sd_tukey/sqrt(n)
      
      # 95% Confidence Interval (parametric) [RI - error][RI + error] 
      confidence_outliers_2_5 <- c(quantiles_outlier[1] - error_outlier, quantiles_outlier[1] + error_outlier)
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5, confidence_outliers_2_5)
      confidence_tukey_2_5 <- c(quantiles_tukey[1] - error_tukey, quantiles_tukey[1] + error_tukey)
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
      
      confidence_outliers_97_5 <- c(quantiles_outlier[2] - error_outlier, quantiles_outlier[2] + error_outlier)
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5, confidence_outliers_97_5)
      confidence_tukey_97_5 <- c(quantiles_tukey[2] - error_tukey, quantiles_tukey[2] + error_tukey)
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
    }
    
    ################################### QQ-PLOT ###################################################
    if(method == "qqplot"){
      
      newx <- c(-1.96,0,1.96)
      
      qqdata <- qqnorm(age_data_ready$value, plot.it = FALSE)
      lm_qqline <- lm(y~ x, data = qqdata)
      result_qqline <- predict(lm_qqline, newdata=data.frame(x=newx), interval="confidence")
      
      qqdata_tukey <- qqnorm(outliers_tukey, plot.it = FALSE)
      lm_qqline_tukey <- lm(y~x, data = qqdata_tukey)
      result_qqline_tukey <- predict(lm_qqline_tukey, newdata=data.frame(x=newx), interval="confidence")
      
      # Tukey
      mean_tukey <- result_qqline_tukey[2,1]
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      quantiles_tukey <- c(result_qqline_tukey[1,1], result_qqline_tukey[3,1])
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # Without Outlierdetection
      mean_outlier <- result_qqline[2,1]
      mean_with_outliers <- rbind(mean_with_outliers,mean_outlier)
      quantiles_outlier <- c(result_qqline[1,1], result_qqline[3,1])
      quantiles_with_outliers <- rbind(quantiles_with_outliers,quantiles_outlier) 
      
      # 95% Confidence Interval
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,c(result_qqline_tukey[1,2], result_qqline_tukey[1,3]))
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,c(result_qqline_tukey[3,2], result_qqline_tukey[3,3]))
      
      confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,c(result_qqline[1,2], result_qqline[1,3]))
      confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,c(result_qqline[3,2], result_qqline[3,3]))}
  }
  
  ##################################### Save the data #############################################
  
  # Window-Data without outlierdetection
  window_data_lis <<- data.frame(age_days = split, mean = c(mean_with_outliers[,1], tail(mean_with_outliers[,1], n=1)),
                                   quantile1 = c(quantiles_with_outliers[,1], tail(quantiles_with_outliers[,1], n=1)),
                                   quantile2 = c(quantiles_with_outliers[,2], tail(quantiles_with_outliers[,2], n=1)))
  
  # Window-Data with modified.tukey()
  window_data_tukey_lis <<- data.frame(age_days = split, mean = c(mean_with_tukey[,1],tail(mean_with_outliers[,1], n=1) ),
                                         quantile1 = c(quantiles_with_tukey[,1],  tail(quantiles_with_outliers[,1], n=1)),
                                         quantile2 = c(quantiles_with_tukey[,2], tail(quantiles_with_outliers[,2], n=1)))
  
  ##################################### Tables to download ########################################
  
  window_data_lis_outlier <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                           "to [Days]"              = split[2:length(split)],
                                           "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                           "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                           "Median"                 = c(mean_with_outliers[,1]),
                                           "2.5 % RI"               = c(quantiles_with_outliers[,1]),
                                           "97.5% RI"               = c(quantiles_with_outliers[,2]),
                                           "95% CI (2.5% RI) from"  = c(confidence_with_outliers_2_5[,1]),
                                           "to"                     = c(confidence_with_outliers_2_5[,2]),
                                           "95% CI (97.5% RI) from" = c(confidence_with_outliers_97_5[,1]),
                                           "to"                     = c(confidence_with_outliers_97_5[,2]), 
                                           "Number of data points"  = c(n_data[,1]), check.names = FALSE)
  
  window_data_lis_tukey <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                         "to [Days]"              = split[2:length(split)],
                                         "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                         "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                         "Median"                 = c(mean_with_tukey[,1]),
                                         "2.5 % RI"               = c(quantiles_with_tukey[,1]),
                                         "97.5% RI"               = c(quantiles_with_tukey[,2]),
                                         "95% CI (2.5% RI) from"  = c(confidence_with_tukey_2_5[,1]),
                                         "to"                     = c(confidence_with_tukey_2_5[,2]),
                                         "95% CI (97.5% RI) from" = c(confidence_with_tukey_97_5[,1]),
                                         "to"                     = c(confidence_with_tukey_97_5[,2]),
                                         "Number of data points"  = c(n_data_tukey[,1]), check.names = FALSE)
  
  if(method == "para"){
    window_data_lis_outlier <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                             "to [Days]"              = split[2:length(split)],
                                             "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                             "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                             "Mean"                   = c(mean_with_outliers[,1]),
                                             "2.5 % RI"               = c(quantiles_with_outliers[,1]),
                                             "97.5% RI"               = c(quantiles_with_outliers[,2]),
                                             "95% CI (2.5% RI) from"  = c(confidence_with_outliers_2_5[,1]),
                                             "to"                     = c(confidence_with_outliers_2_5[,2]),
                                             "95% CI (97.5% RI) from" = c(confidence_with_outliers_97_5[,1]),
                                             "to"                     = c(confidence_with_outliers_97_5[,2]), 
                                             "Number of data points"  = c(n_data[,1]), check.names = FALSE)
    
    window_data_lis_tukey <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                           "to [Days]"              = split[2:length(split)],
                                           "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                           "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                           "Mean"                   = c(mean_with_tukey[,1]),
                                           "2.5 % RI"               = c(quantiles_with_tukey[,1]),
                                           "97.5% RI"               = c(quantiles_with_tukey[,2]),
                                           "95% CI (2.5% RI) from"  = c(confidence_with_tukey_2_5[,1]),
                                           "to"                     = c(confidence_with_tukey_2_5[,2]),
                                           "95% CI (97.5% RI) from" = c(confidence_with_tukey_97_5[,1]),
                                           "to"                     = c(confidence_with_tukey_97_5[,2]), 
                                           "Number of data points"  = c(n_data_tukey[,1]), check.names = FALSE)}
}


####################################### Sliding Window ############################################

#' Sliding Window-Method: Go through the data with a specific window width and with specific steps
#'
#' @param sliding_window_data Selected data
#' 
#' #'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param width_ Range of the window
#' @param by_ Step for the window
#' @param outliers Outlierdetection (None and Tukey)
sliding_window <- function(sliding_window_data, width_ = 120, by_ = 20, outliers){
  
  # Order the data with the Index
  sliding_window_data <- sliding_window_data[order(sliding_window_data$age_days),]
  
  ##################################### Age for the Slidng Window #################################
  # Begin of the Slining Window
  sliding_age <- rollapply(sliding_window_data$age_days, width = width_, by = by_, 
                           FUN = min, na.rm = TRUE, partial = TRUE,align = "left")
  # End of the Sliding Window
  sliding_age_to <- rollapply(sliding_window_data$age_days, width = width_, by = by_, 
                             FUN = max, na.rm = TRUE, partial =TRUE,align = "left")
  
  ##################################### Median of the subset generated by rollapply() #############
  sliding_median <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                              FUN= median, na.rm = TRUE, partial =TRUE,align = "left")
  
  ##################################### Data in the window ########################################
  sliding_tukey_data <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                                  FUN = invisible, partial = TRUE, align = "left")

  ##################################### 2.5% Percentil ############################################
  sliding_2_5 <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                           FUN = quantile, probs = c(0.025), partial =TRUE, align = "left")
  
  ##################################### 97.5% Percentil ###########################################
  sliding_97_5 <- rollapply(sliding_window_data$value, width = width_, by = by_, FUN = quantile, 
                            probs = c(0.975), partial =TRUE, align = "left")
 
  ##################################### Dataframes for the Tukey-method and CI ####################
  sliding_median_tukey <- data.frame()
  sliding_2_5_tukey <- data.frame()
  sliding_97_5_tukey <- data.frame()
  
  confidence_with_tukey_2_5 <- data.frame()
  confidence_with_tukey_97_5 <- data.frame()
  confidence_with_outliers_2_5 <- data.frame()
  confidence_with_outliers_97_5 <- data.frame()
    
  for (i in seq(1,nrow(sliding_tukey_data))){ 
    
    normal_log <- FALSE
    try(normal_log <- def.lognorm(sliding_tukey_data[i,], plot.it = FALSE)$lognorm)
    
    if(normal_log == TRUE){sliding_tukey <- modified.tukey(sliding_tukey_data[i,], plot.it = FALSE, log.mode = TRUE)}
    else{sliding_tukey <- modified.tukey(sliding_tukey_data[i,], plot.it = FALSE)}
    
    # 95% Confidence Interval
    try(confidence_tukey <- Boot_CI(sliding_tukey))
    confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5,t(data.frame(confidence_tukey[,1][2:3])))
    confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5,t(data.frame(confidence_tukey[,2][2:3])))
    
    try(confidence_outlier <- Boot_CI(sliding_tukey_data[i,]))
    confidence_with_outliers_2_5 <- rbind(confidence_with_outliers_2_5,t(data.frame(confidence_outlier[,1][2:3])))
    confidence_with_outliers_97_5 <- rbind(confidence_with_outliers_97_5,t(data.frame(confidence_outlier[,2][2:3])))
      
    sliding_median_tukey <- rbind(sliding_median_tukey, median(sliding_tukey))
    sliding_2_5_tukey <- rbind(sliding_2_5_tukey, quantile(sliding_tukey, probs = c(0.025)))
    sliding_97_5_tukey <- rbind(sliding_97_5_tukey, quantile(sliding_tukey, probs = c(0.975)))
  }
    
  sliding_median_tukey <- sliding_median_tukey[,1]
  sliding_2_5_tukey <- sliding_2_5_tukey[,1]
  sliding_97_5_tukey <- sliding_97_5_tukey[,1]
  
  ##################################### Preprocessing #############################################
  
  sliding_age <- c(0, sliding_age, max(sliding_window_data$age_days))
  sliding_age_to <- c(0, sliding_age_to, max(sliding_window_data$age_days))
  
  sliding_median <- c(sliding_median[1], sliding_median, sliding_median[length(sliding_median)])
  sliding_median_tukey <- c(sliding_median_tukey[1], sliding_median_tukey, sliding_median_tukey[length(sliding_median_tukey)])
  
  sliding_2_5 <- c(sliding_2_5[1], sliding_2_5, sliding_2_5[length(sliding_2_5)])
  sliding_2_5_tukey <- c(sliding_2_5_tukey[1], sliding_2_5_tukey, sliding_2_5_tukey[length(sliding_2_5_tukey)])
  sliding_97_5 <- c(sliding_97_5[1], sliding_97_5, sliding_97_5[length(sliding_97_5)])
  sliding_97_5_tukey <- c(sliding_97_5_tukey[1], sliding_97_5_tukey, sliding_97_5_tukey[length(sliding_97_5_tukey)])
  
  sliding_with_tukey_2_5_from <- c(confidence_with_tukey_2_5[1,1], confidence_with_tukey_2_5[,1], confidence_with_tukey_2_5[1, length(confidence_with_tukey_2_5)])
  sliding_with_tukey_2_5_to <- c(confidence_with_tukey_2_5[1,2], confidence_with_tukey_2_5[,2], confidence_with_tukey_2_5[2, length(confidence_with_tukey_2_5)])
  sliding_with_tukey_97_5_from <- c(confidence_with_tukey_97_5[1,1], confidence_with_tukey_97_5[,1], confidence_with_tukey_97_5[1, length(confidence_with_tukey_97_5)])
  sliding_with_tukey_97_5_to <- c(confidence_with_tukey_97_5[1,2], confidence_with_tukey_97_5[,2], confidence_with_tukey_97_5[2, length(confidence_with_tukey_97_5)])
  
  sliding_with_outliers_2_5_from <- c(confidence_with_outliers_2_5[1,1], confidence_with_outliers_2_5[,1], confidence_with_outliers_2_5[1, length(confidence_with_outliers_2_5)])
  sliding_with_outliers_2_5_to <- c(confidence_with_outliers_2_5[1,2], confidence_with_outliers_2_5[,2], confidence_with_outliers_2_5[2, length(confidence_with_outliers_2_5)])
  sliding_with_outliers_97_5_from <- c(confidence_with_outliers_97_5[1,1], confidence_with_outliers_97_5[,1], confidence_with_outliers_97_5[1, length(confidence_with_outliers_97_5)])
  sliding_with_outliers_97_5_to <- c(confidence_with_outliers_97_5[1,2], confidence_with_outliers_97_5[,2], confidence_with_outliers_97_5[2, length(confidence_with_outliers_97_5)])
  
  slide <<- data.frame("Age-Range from" = sliding_age,
                       "to" = sliding_age_to,
                       "Median" = sliding_median,
                       "2.5% Percentil" = sliding_2_5,
                       "97.5% Percentil" = sliding_97_5,
                       "95% CI (2.5% RI) from"  = sliding_with_outliers_2_5_from,
                       "to"                     = sliding_with_outliers_2_5_to,
                       "95% CI (97.5% RI) from" = sliding_with_outliers_97_5_from,
                       "to"                     = sliding_with_outliers_97_5_to, check.names = FALSE)
  
  slide_tukey <<- data.frame("Age-Range from" = sliding_age,
                             "to" = sliding_age_to,
                             "Median" = sliding_median_tukey,
                             "2.5% Percentil" = sliding_2_5_tukey,
                             "97.5% Percentil" = sliding_97_5_tukey, 
                             "95% CI (2.5% RI) from"  = sliding_with_tukey_2_5_from,
                             "to"                     = sliding_with_tukey_2_5_to,
                             "95% CI (97.5% RI) from" = sliding_with_tukey_97_5_from,
                             "to"                     = sliding_with_tukey_97_5_to, check.names = FALSE)
  return(slide)
}


####################################### Metrics ###################################################

#' Mean Absolute Error for the window methods
mae_window <- function(){
  
  ##################################### Regular Window - No Outlierdetection ######################
  window_data_age <- window_data$age_days
  mae_data_window_data <- data.frame()
  
  for(i in seq(1:(length(window_data_age)-1))){
    
    # Make a dataframe with all age_days and their possible value
    mae_data_save <- data.frame(age_days = seq(window_data$age_days[i],window_data$age_days[i+1]), value = window_data$mean[i])
    mae_data_window_data <- rbind(mae_data_window_data, mae_data_save)
  }
  
  # Search for the age_days from the data
  mae_data_window_data <- mae_data_window_data[which(mae_data_window_data$age_days %in% data_analyte_short$age_days),]
  
  value_mae <- data.frame()
  for(i in data_analyte_short$age_days){ 
    # Filter the right data from the orginal data age_days and get a dataframe with the predicted median from the window
    value_mae <- rbind(value_mae, mae_data_window_data[mae_data_window_data$age_days == i,][[2]])
  }

  mae_window_data <- mae(data_analyte_short$value, value_mae[[1]]) # Calculate the Mean Absolute Error

  ##################################### Regular Window - Tukey ####################################
  window_data_tukey_age <- window_data_tukey$age_days
  mae_data_window_data_tukey <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_age)-1))){
    mae_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i],window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
    mae_data_window_data_tukey <- rbind(mae_data_window_data_tukey, mae_data_save)
  }
  
  mae_data_window_data_tukey <- mae_data_window_data_tukey[which(mae_data_window_data_tukey$age_days %in% data_analyte_short$age_days),]
  
  value_mae <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mae <- rbind(value_mae, mae_data_window_data_tukey[mae_data_window_data_tukey$age_days == i,][[2]])
  }

  mae_window_data_tukey <- mae(data_analyte_short$value, value_mae[[1]])
  
  ##################################### Window with Decision Tree - No Outlierdetection ###########
  window_data_rpart_age <- round(window_data_rpart$age_days)
  mae_data_window_data_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_rpart_age)-1))){
    mae_data_save <- data.frame(age_days = seq(round(window_data_rpart$age_days[i]),
                                               round(window_data_rpart$age_days[i+1])), 
                                value = window_data_rpart$mean[i])
    mae_data_window_data_rpart <- rbind(mae_data_window_data_rpart, mae_data_save)
  }
  
  mae_data_window_data_rpart <- mae_data_window_data_rpart[which(mae_data_window_data_rpart$age_days %in% data_analyte_short$age_days),]
  
  value_mae <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mae <- rbind(value_mae, mae_data_window_data_rpart[mae_data_window_data_rpart$age_days == i,][[2]])
  }
  
  mae_window_data_rpart <- mae(data_analyte_short$value, value_mae[[1]])
  
  ##################################### Window with Decision Tree - Tukey #########################
  window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
  mae_data_window_data_tukey_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
    mae_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]),round(window_data_tukey_rpart$age_days[i+1])), 
                                value = window_data_tukey_rpart$mean[i])
    mae_data_window_data_tukey_rpart <- rbind(mae_data_window_data_tukey_rpart, mae_data_save)
  }
  
  mae_data_window_data_tukey_rpart <- mae_data_window_data_tukey_rpart[which(mae_data_window_data_tukey_rpart$age_days %in% 
                                                                               data_analyte_short$age_days),]
  
  value_mae <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mae <- rbind(value_mae, mae_data_window_data_tukey_rpart[mae_data_window_data_tukey_rpart$age_days == i,][[2]])
  }

  mae_window_data_tukey_rpart <- mae(data_analyte_short$value, value_mae[[1]])
  
  return(c(mae_window_data, mae_window_data_tukey, mae_window_data_rpart, mae_window_data_tukey_rpart))
}


#' Mean squared Error for the window methods
mse_window <- function(){
  
  ##################################### Regular Window - No Outlierdetection ######################
  window_data_age <- window_data$age_days
  mse_data_window_data <- data.frame()
  
  for(i in seq(1:(length(window_data_age)-1))){
    mse_data_save <- data.frame(age_days = seq(window_data$age_days[i],window_data$age_days[i+1]), value = window_data$mean[i])
    mse_data_window_data <- rbind(mse_data_window_data, mse_data_save)
  }
  
  mse_data_window_data <- mse_data_window_data[which(mse_data_window_data$age_days %in% data_analyte_short$age_days),]
  
  value_mse <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mse <- rbind(value_mse, mse_data_window_data[mse_data_window_data$age_days == i,][[2]])
  }
  
  # Calculate the Mean Squared Error
  mse_window_data <- mse(data_analyte_short$value, value_mse[[1]])
  
  ##################################### Regular Window - Tukey ####################################
  window_data_tukey_age <- window_data_tukey$age_days
  mse_data_window_data_tukey <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_age)-1))){
    mse_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i],window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
    mse_data_window_data_tukey <- rbind(mse_data_window_data_tukey, mse_data_save)
  }
  
  mse_data_window_data_tukey <- mse_data_window_data_tukey[which(mse_data_window_data_tukey$age_days  %in% data_analyte_short$age_days),]
  
  value_mse <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mse <- rbind(value_mse, mse_data_window_data_tukey[mse_data_window_data_tukey$age_days == i,][[2]])
  }
 
  mse_window_data_tukey <- mse(data_analyte_short$value, value_mse[[1]])
  
  ##################################### Window with Decision Tree - No Outlierdetection ###########
  window_data_rpart_age <- round(window_data_rpart$age_days)
  mse_data_window_data_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_rpart_age)-1))){
    mse_data_save <- data.frame(age_days = seq(round(window_data_rpart$age_days[i]),
                                               round(window_data_rpart$age_days[i+1])), value = window_data_rpart$mean[i])
    mse_data_window_data_rpart <- rbind(mse_data_window_data_rpart, mse_data_save)
  }
  
  mse_data_window_data_rpart <- mse_data_window_data_rpart[which(mse_data_window_data_rpart$age_days  %in% data_analyte_short$age_days),]
  
  value_mse <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mse <- rbind(value_mse, mse_data_window_data_rpart[mse_data_window_data_rpart$age_days == i,][[2]])
  }
  
  mse_window_data_rpart <- mse(data_analyte_short$value, value_mse[[1]])
  
  ##################################### Window with Decision Tree - Tukey #########################
  window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
  mse_data_window_data_tukey_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
    mse_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]),round(window_data_tukey_rpart$age_days[i+1])), value = window_data_tukey_rpart$mean[i])
    mse_data_window_data_tukey_rpart <- rbind(mse_data_window_data_tukey_rpart, mse_data_save)
  }
  
  mse_data_window_data_tukey_rpart <- mse_data_window_data_tukey_rpart[which(mse_data_window_data_tukey_rpart$age_days  %in% data_analyte_short$age_days),]
  
  value_mse <- data.frame()
  for(i in data_analyte_short$age_days){
    value_mse <- rbind(value_mse, mse_data_window_data_tukey_rpart[mse_data_window_data_tukey_rpart$age_days == i,][[2]])
  }

  mse_window_data_tukey_rpart <- mse(data_analyte_short$value, value_mse[[1]])
  
  return(c(mse_window_data, mse_window_data_tukey, mse_window_data_rpart,mse_window_data_tukey_rpart))
}


#' Root mean squared error for the Window-methods
rmse_window <- function(){
  
  ##################################### Regular Window - No Outlierdetection ######################
  window_data_age <- window_data$age_days
  rmse_data_window_data <- data.frame()
  
  for(i in seq(1:(length(window_data_age)-1))){
    rmse_data_save <- data.frame(age_days = seq(window_data$age_days[i],window_data$age_days[i+1]), value = window_data$mean[i])
    rmse_data_window_data <- rbind(rmse_data_window_data, rmse_data_save)
  }
 
  # Search for the age_days from the data
  rmse_data_window_data <- rmse_data_window_data[which(rmse_data_window_data$age_days %in% data_analyte_short$age_days),]
  
  value_rmse <- data.frame()
  for(i in data_analyte_short$age_days){
    # Filter the right data from the orginal data age_days and get a datafram with the predicted median from the window
    value_rmse <- rbind(value_rmse, rmse_data_window_data[rmse_data_window_data$age_days == i,][[2]])
  }
  
  # Calculate the Root mean squared error
  rmse_window_data <- rmse(data_analyte_short$value, value_rmse[[1]])
  
  ##################################### Regular Window - Tukey ####################################
  window_data_tukey_age <- window_data_tukey$age_days
  rmse_data_window_data_tukey <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_age)-1))){
    rmse_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i],window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
    rmse_data_window_data_tukey <- rbind(rmse_data_window_data_tukey, rmse_data_save)
  }
  
  rmse_data_window_data_tukey <- rmse_data_window_data_tukey[which(rmse_data_window_data_tukey$age_days  %in% data_analyte_short$age_days),]
  
  value_rmse <- data.frame()
  for(i in data_analyte_short$age_days){ 
    value_rmse <- rbind(value_rmse, rmse_data_window_data_tukey[rmse_data_window_data_tukey$age_days == i,][[2]])
  }
  
  rmse_window_data_tukey <- rmse(data_analyte_short$value, value_rmse[[1]])
  
  ##################################### Window with Decision Tree - No Outlierdetection ###########
  window_data_rpart_age <- round(window_data_rpart$age_days)
  rmse_data_window_data_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_rpart_age)-1))){
    rmse_data_save <- data.frame(age_days = seq(round(window_data_rpart$age_days[i]), 
                                                round(window_data_rpart$age_days[i+1])), value = window_data_rpart$mean[i])
    
    rmse_data_window_data_rpart <- rbind(rmse_data_window_data_rpart, rmse_data_save)
  }
  
  rmse_data_window_data_rpart <- rmse_data_window_data_rpart[which(rmse_data_window_data_rpart$age_days %in% data_analyte_short$age_days),]
  
  value_rmse <- data.frame()
  for(i in data_analyte_short$age_days){
    value_rmse <- rbind(value_rmse, rmse_data_window_data_rpart[rmse_data_window_data_rpart$age_days == i,][[2]])
  }
  
  rmse_window_data_rpart <- rmse(data_analyte_short$value, value_rmse[[1]])
  
  ##################################### Window with Decision Tree - Tukey #########################
  window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
  rmse_data_window_data_tukey_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
    rmse_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]), round(window_data_tukey_rpart$age_days[i+1])), 
                                 value = window_data_tukey_rpart$mean[i])
    rmse_data_window_data_tukey_rpart <- rbind(rmse_data_window_data_tukey_rpart, rmse_data_save)
  }
  
  rmse_data_window_data_tukey_rpart <- rmse_data_window_data_tukey_rpart[which(
    rmse_data_window_data_tukey_rpart$age_days %in% data_analyte_short$age_days),]
  
  value_rmse <- data.frame()
  for(i in data_analyte_short$age_days){
    value_rmse <- rbind(value_rmse, rmse_data_window_data_tukey_rpart[rmse_data_window_data_tukey_rpart$age_days == i,][[2]])
  }
  
  rmse_window_data_tukey_rpart <- rmse(data_analyte_short$value, value_rmse[[1]])
  
  return(c(rmse_window_data, rmse_window_data_tukey, rmse_window_data_rpart, rmse_window_data_tukey_rpart))
}


#' R-Squared
r_window <- function(){
  
  ##################################### Regular Window - No Outlierdetection ######################
  window_data_age <- window_data$age_days
  rsquare_data_window_data <- data.frame()
  
  for(i in seq(1:(length(window_data_age)-1))){
    rsquare_data_save <- data.frame(age_days = seq(window_data$age_days[i],window_data$age_days[i+1]), value = window_data$mean[i])
    rsquare_data_window_data <- rbind(rsquare_data_window_data, rsquare_data_save)
  }
  
  rsquare_data_window_data <- rsquare_data_window_data[which(rsquare_data_window_data$age_days  %in% data_analyte_short$age_days),]
  
  value_rsquare <- data.frame()
  for(i in data_analyte_short$age_days){
    value_rsquare <- rbind(value_rsquare, rsquare_data_window_data[rsquare_data_window_data$age_days == i,][[2]])
  }
  
  # Calculate the R-Square
  rsquare_window_data <- rsq(data_analyte_short$value, value_rsquare[[1]])
  
  ##################################### Regular Window - Tukey ####################################
  window_data_tukey_age <- window_data_tukey$age_days
  rsquare_data_window_data_tukey <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_age)-1))){
    rsquare_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i], window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
    rsquare_data_window_data_tukey <- rbind(rsquare_data_window_data_tukey, rsquare_data_save)
  }
  
  rsquare_data_window_data_tukey <- rsquare_data_window_data_tukey[which(rsquare_data_window_data_tukey$age_days %in% data_analyte_short$age_days),]
  
  value_rsquare <- data.frame()
  for(i in data_analyte_short$age_days){
    value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_tukey[rsquare_data_window_data_tukey$age_days == i,][[2]])
  }
  
  rsquare_window_data_tukey <- rsq(data_analyte_short$value, value_rsquare[[1]])
  
  ##################################### Window with Decision Tree - No Outlierdetection ###########
  window_data_rpart_age <- round(window_data_rpart$age_days)
  rsquare_data_window_data_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_rpart_age)-1))){
    rsquare_data_save <- data.frame(age_days = seq(round(window_data_rpart$age_days[i]),
                                               round(window_data_rpart$age_days[i+1])), value = window_data_rpart$mean[i])
    rsquare_data_window_data_rpart <- rbind(rsquare_data_window_data_rpart, rsquare_data_save)
  }
  
  rsquare_data_window_data_rpart <- rsquare_data_window_data_rpart[which(rsquare_data_window_data_rpart$age_days %in% data_analyte_short$age_days),]
  
  value_rsquare <- data.frame()
  for(i in data_analyte_short$age_days){
    value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_rpart[rsquare_data_window_data_rpart$age_days == i,][[2]])
  }

  rsquare_window_data_rpart <- rsq(data_analyte_short$value, value_rsquare[[1]])
  
  ##################################### Window with Decision Tree - Tukey #########################
  window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
  rsquare_data_window_data_tukey_rpart <- data.frame()
  
  for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
    rsquare_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]),round(window_data_tukey_rpart$age_days[i+1])), value = window_data_tukey_rpart$mean[i])
    rsquare_data_window_data_tukey_rpart <- rbind(rsquare_data_window_data_tukey_rpart, rsquare_data_save)
  }
  
  rsquare_data_window_data_tukey_rpart <- rsquare_data_window_data_tukey_rpart[which(
    rsquare_data_window_data_tukey_rpart$age_days %in% data_analyte_short$age_days),]
  
  value_rsquare <- data.frame()
  for(i in data_analyte_short$age_days){
    value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_tukey_rpart[rsquare_data_window_data_tukey_rpart$age_days == i,][[2]])
  }
  
  rsquare_window_data_tukey_rpart <- rsq(data_analyte_short$value, value_rsquare[[1]])

  return(c(rsquare_window_data, rsquare_window_data_tukey, rsquare_window_data_rpart,rsquare_window_data_tukey_rpart))
}