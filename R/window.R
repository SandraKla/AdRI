###################################################################################################
####################################### Script for the Window-Methods #############################
###################################################################################################

####################################### Bootstrapping #############################################

#' Bootstrapping Confidence Intervals with package boot and 1000 Bootstrap replicates
#' 
#' @param x Data for the Confidence Intervals
#' @param plot.it Plot the Bootstrapping
# Boot_CI <- function(x, plot.it = FALSE){
#   
#   boot.quantile <- function(x, indices, ...){
#     quantile(x[indices], ...)}
#     
#   boot_para <- boot(x, boot.quantile, R = 1000, probs = c(0.025, 0.975))
#   
#   if(plot.it){
#     plot(boot_para)}
#   
#   # Calculate the quantile of the different bootstrapping values with Confidence Intervals
#   ci <- apply(boot_para$t, MARGIN = 2, FUN = quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE)
#   
#   return(ci)
# }

####################################### Decision Tree #############################################

#' Build a Decision Tree from the package rpart
#'
#' @param data_analyte the data
#' @param minsplit_tree Minimum number of observations that must exist in a node in order for a split to be attempted
make_rpart <- function(data_analyte, minsplit_tree = 360){
  rpart_ <<- rpart(value~age_days, data = data_analyte, method = "anova", 
                   control = rpart.control(minsplit = minsplit_tree, cp = 0.01))
}

####################################### Regular Window-method #####################################

#' Regular Window-method with the RefLim
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
#' 
#' @param window range of the window
#' @param method RefLim
window_method <- function(data_, window, method){

  ##################################### Mean ######################################################
  mean_with_tukey <-  data.frame() 
  ##################################### Quantiles #################################################
  quantiles_with_tukey <- data.frame()
  ##################################### Standard derivation #######################################
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_tukey_97_5 <- data.frame()

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
    try(normal_log <- def.distribution(age_data_ready$value, plot.it = FALSE)$lognorm)
    n_data_tukey <- rbind(n_data_tukey, n)
  
    ##################################### RefLim #################################################
    if(method == "reflim"){
   
      ci_qq <- NA
      
      modi_qq <- reflim(age_data_ready$value, n.min = 100, log.mode = normal_log)
      if(!is.na(modi_qq[3]) || !is.na(modi_qq[4])){
        if(!(modi_qq[3] >= modi_qq[4]))
          try(ci_qq <- ci.quant95(n = length(age_data_ready$value), lower.limit = modi_qq[3], upper.limit = modi_qq[4], lognorm = normal_log))
      }
      
      # Mean
      mean_tukey <- modi_qq[1]
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)

      # Quantile
      quantiles_tukey <- c(modi_qq[[3]], modi_qq[[4]])
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
    
      # 95% Confidence Interval
      if(!is.na(ci_qq)[[1]]){
        confidence_tukey_2_5 <- c(ci_qq[[1]], ci_qq[[2]])
        confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
        confidence_tukey_97_5 <- c(ci_qq[[3]], ci_qq[[4]])
        confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
      } else{
        confidence_tukey_2_5 <- c(NA, NA)
        confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
        confidence_tukey_97_5 <- c(NA, NA)
        confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
      }
    }
  }
    
  ##################################### Save the data #############################################
  
  # Window-Data with the RefLim()
  window_data_tukey <<- data.frame(age_days = seq(min(data_[,4]), age_code, by = window), 
                                   mean = c(mean_with_tukey[,1], tail(mean_with_tukey[,1], n=1)), 
                                   quantile1 = c(quantiles_with_tukey[,1],tail(quantiles_with_tukey[,1], n=1)),
                                   quantile2 = c(quantiles_with_tukey[,2],tail(quantiles_with_tukey[,2], n=1)))
  
  ##################################### Tables to download ########################################
  
  window_data_all_tukey <<- data.frame("Age-range from"           = seq(min(data_[,4]), age_code-1, by=window),
                                       "to [Days]"                = c(head(seq(min(data_[,4]) + window, age_code, by = window),-1),max(data_$age_days)),
                                       "Age-range from"           = round_df(seq(min(data_[,4]), age_code-1, by=window)/365, 3),
                                       "to [Years]"               = c(round_df(head(seq(min(data_[,4]) + window, age_code, by = window)/365,-1),3), max(data_$age)),
                                       "2.5 % RI"                 = c(quantiles_with_tukey[,1]),
                                       "97.5% RI"                 = c(quantiles_with_tukey[,2]),
                                       "95% CI (2.5% RI) from"    = c(confidence_with_tukey_2_5[[1]]),
                                       "to"                       = c(confidence_with_tukey_2_5[[2]]),
                                       "95% CI (97.5% RI) from"   = c(confidence_with_tukey_97_5[[1]]),
                                       "to"                       = c(confidence_with_tukey_97_5[[2]]),
                                       "Number of data points"    = c(n_data_tukey[,1]), check.names = FALSE)
}

####################################### Window-Method coupled to the Decision Tree ################

#' Window_method were the age groups are calculated with the help machine learning, more accurate 
#' Decision Tree from rpart().
#'
#' @param data_window_split Given dataset:
#' 
#'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param split Age group
#' @param method RefLim
#' @param plot_log Plot the function def.distribution() of each age groups
window_method_split <- function(data_window_split, split, method, plot_log = FALSE){
  
  ##################################### Mean ######################################################
  mean_with_tukey <-  data.frame() 
  ##################################### Quantile ##################################################
  quantiles_with_tukey <- data.frame() 
  ##################################### Standard derivation #######################################
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_tukey_97_5 <- data.frame()
  
  n_data_tukey <- data.frame()
  
  for (i in seq(2,length(split))){
    
    age_code <- split[i]

    # The data subset 
    age_data <- subset(data_window_split, data_window_split$age_days <= split[i])  # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i-1])             # Below the lowest condition
    if(split[i-1] == 0){
      age_data_ready <- subset(age_data, age_data$age_days >= split[i-1])}  # Below the lowest condition

    n <- nrow(age_data_ready)
    normal_log <- FALSE
    
    if(!plot_log){
      try(normal_log <- def.distribution(age_data_ready$value, plot.it = FALSE)$lognorm)
    } else{
      # Plot the distribution of each group to check for normally distribution
      try(normal_log <- def.distribution(age_data_ready$value, plot.it = TRUE, main = paste("between", split[i-1], "and", split[i], "days"))$lognorm)
    }
    n_data_tukey <- rbind(n_data_tukey,n)
    
    ################################### RefLim ###################################################
    if(method == "reflim"){
      
      ci_qq <- NA
      
      modi_qq <- reflim(age_data_ready$value, n.min = 100, log.mode = normal_log, plot.it = FALSE)
      if(!is.na(modi_qq[3]) || !is.na(modi_qq[4])){
        if(!(modi_qq[3] >= modi_qq[4]))
          try(ci_qq <- ci.quant95(n = length(age_data_ready$value), lower.limit = modi_qq[3], upper.limit = modi_qq[4], lognorm = normal_log))
      }
      
      # Mean
      mean_tukey <- modi_qq[1]
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      
      # Quantile
      quantiles_tukey <- c(modi_qq[[3]], modi_qq[[4]])
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # 95% Confidence Interval
      if(!is.na(ci_qq)[[1]]){
        confidence_tukey_2_5 <- c(ci_qq[[1]], ci_qq[[2]])
        confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
        confidence_tukey_97_5 <- c(ci_qq[[3]], ci_qq[[4]])
        confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
      } else{
        confidence_tukey_2_5 <- c(NA, NA)
        confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
        confidence_tukey_97_5 <- c(NA, NA)
        confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
      }
    }
  }
  
  ##################################### Save the data #############################################
  
  # Window-Data with RefLim
  window_data_tukey_rpart <<- data.frame(age_days = split,                                    
                                         mean = c(mean_with_tukey[,1], tail(mean_with_tukey[,1], n=1)), 
                                         quantile1 = c(quantiles_with_tukey[,1],tail(quantiles_with_tukey[,1], n=1)),
                                         quantile2 = c(quantiles_with_tukey[,2],tail(quantiles_with_tukey[,2], n=1)))

  ##################################### Tables to download ########################################

  window_data_split_tukey <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                         "to [Days]"              = split[2:length(split)],
                                         "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                         "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                         "2.5 % RI"                 = c(quantiles_with_tukey[,1]),
                                         "97.5% RI"                 = c(quantiles_with_tukey[,2]),
                                         "95% CI (2.5% RI) from"    = c(confidence_with_tukey_2_5[[1]]),
                                         "to"                       = c(confidence_with_tukey_2_5[[2]]),
                                         "95% CI (97.5% RI) from"   = c(confidence_with_tukey_97_5[[1]]),
                                         "to"                       = c(confidence_with_tukey_97_5[[2]]),
                                         "Number of data points"    = c(n_data_tukey[,1]), check.names = FALSE)
}


#' Same as window_method_split() just with given age groups
#' 
#' @param data_window_split Given dataset:
#' 
#'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param split Age group from the LIS
#' @param method RefLim
#' @param plot_log Plot the function def.distribution() of each age groups
window_method_lis <- function(data_window_split, split, method, plot_log = FALSE){
  
  ##################################### Mean  #####################################################
  mean_with_tukey <-  data.frame() 
  ##################################### Quantile ##################################################
  quantiles_with_tukey <- data.frame() 
  ##################################### Standard derivation #######################################
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_tukey_97_5 <- data.frame()

  n_data_tukey <- data.frame()
  
  for (i in seq(2,length(split))){
    
    age_code <- split[i]
    
    # The data subset 
    age_data <- subset(data_window_split, data_window_split$age_days <= split[i])  # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i-1])             # Below the lowest condition
    if(split[i-1] == 0){
      age_data_ready <- subset(age_data, age_data$age_days >= split[i-1])}  # Below the lowest condition
    
    n <- nrow(age_data_ready)
    normal_log <- FALSE
    if(!plot_log){
      try(normal_log <- def.distribution(age_data_ready$value, plot.it = FALSE)$lognorm)
    } else{
      # Plot the distribution of each group to check for normally distribution
      try(normal_log <- def.distribution(age_data_ready$value, plot.it = TRUE, main = paste("between", split[i-1], "and", split[i], "days"))$lognorm)
    }
    n_data_tukey <- rbind(n_data_tukey,n)
  
    ################################### RefLim ###################################################
    if(method == "reflim"){

      ci_qq <- NA
      
      modi_qq <- reflim(age_data_ready$value, n.min = 100, log.mode = normal_log, plot.it = FALSE)
      if(!is.na(modi_qq[3]) || !is.na(modi_qq[4])){
        if(!(modi_qq[3] >= modi_qq[4]))
          try(ci_qq <- ci.quant95(n = length(age_data_ready$value), lower.limit = modi_qq[3], upper.limit = modi_qq[4], lognorm = normal_log))
      }
      
      # Mean
      mean_tukey <- modi_qq[1]
      mean_with_tukey <- rbind(mean_with_tukey,mean_tukey)
      
      # Quantile
      quantiles_tukey <- c(modi_qq[[3]], modi_qq[[4]])
      quantiles_with_tukey <- rbind(quantiles_with_tukey,quantiles_tukey)
      
      # 95% Confidence Interval
      if(!is.na(ci_qq)[[1]]){
        confidence_tukey_2_5 <- c(ci_qq[[1]], ci_qq[[2]])
        confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
        confidence_tukey_97_5 <- c(ci_qq[[3]], ci_qq[[4]])
        confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
      } else{
        confidence_tukey_2_5 <- c(NA, NA)
        confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
        confidence_tukey_97_5 <- c(NA, NA)
        confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
      }
    }
  }
  
  ##################################### Save the data #############################################
  
  # Window-Data with RefLim
  window_data_tukey_lis <<- data.frame(age_days = split,
                                       mean = c(mean_with_tukey[,1], tail(mean_with_tukey[,1], n=1)), 
                                       quantile1 = c(quantiles_with_tukey[,1],tail(quantiles_with_tukey[,1], n=1)),
                                       quantile2 = c(quantiles_with_tukey[,2],tail(quantiles_with_tukey[,2], n=1)))
  
  ##################################### Tables to download ########################################
  
  window_data_lis_tukey <<- data.frame("Age-range from"         = split[1:length(split)-1],
                                         "to [Days]"              = split[2:length(split)],
                                         "Age-range from"         = round_df(split[1:length(split)-1]/365, 3),
                                         "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                         "2.5 % RI"                 = c(quantiles_with_tukey[,1]),
                                         "97.5% RI"                 = c(quantiles_with_tukey[,2]),
                                         "95% CI (2.5% RI) from"    = c(confidence_with_tukey_2_5[[1]]),
                                         "to"                       = c(confidence_with_tukey_2_5[[2]]),
                                         "95% CI (97.5% RI) from"   = c(confidence_with_tukey_97_5[[1]]),
                                         "to"                       = c(confidence_with_tukey_97_5[[2]]),
                                         "Number of data points"    = c(n_data_tukey[,1]), check.names = FALSE)
}


####################################### Sliding Window ############################################

#' Sliding Window-Method goes through the data with a specific window width and with specific steps
#'
#' @param sliding_window_data Given dataset:
#' 
#'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param width_ Range of the window
#' @param by_ Step for the window
#' @param outliers RefLim
sliding_window <- function(sliding_window_data, width_ = 120, by_ = 20, outliers){

  # Order the data with the Index
  sliding_window_data <- sliding_window_data[order(sliding_window_data$age_days),]

  ##################################### Age for the Sliding Window ################################
  # Begin of the Sliding Window
  sliding_age <- rollapply(sliding_window_data$age_days, width = width_, by = by_, 
                           FUN = min, na.rm = TRUE, partial = TRUE,align = "left")
  # End of the Sliding Window
  sliding_age_to <- rollapply(sliding_window_data$age_days, width = width_, by = by_, 
                             FUN = max, na.rm = TRUE, partial =TRUE,align = "left")

  ##################################### Mean of the subset generated by rollapply() ###############
  sliding_mean <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                              FUN= mean, na.rm = TRUE, partial =TRUE,align = "left")

  ##################################### Data in the window ########################################
  suppressWarnings(sliding_tukey_data <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                                  FUN = invisible, partial = TRUE, align = "left"))

  ##################################### 2.5% Percentile ###########################################
  sliding_2_5 <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                           FUN = quantile, probs = c(0.025), partial =TRUE, align = "left")
  
  ##################################### 97.5% Percentile ##########################################
  sliding_97_5 <- rollapply(sliding_window_data$value, width = width_, by = by_, FUN = quantile, 
                            probs = c(0.975), partial =TRUE, align = "left")
  
  ##################################### Dataframes for the RefLim and CI ##########################
  ##################################### Mean  #####################################################
  sliding_mean_tukey <-  data.frame() 
  ##################################### Quantile ##################################################
  sliding_2_5_tukey <- data.frame() 
  sliding_97_5_tukey <- data.frame()
  ##################################### Standard derivation #######################################
  sd_with_tukey <- data.frame()
  ##################################### Confidence Interval (95%) for 2.5% RI #####################
  confidence_with_tukey_2_5 <- data.frame()
  ##################################### Confidence Interval (95%) for 97.5% RI ####################
  confidence_with_tukey_97_5 <- data.frame()
  
  n_data_tukey <- data.frame()
    
  for (i in seq(1,nrow(sliding_tukey_data))){ 
    
    n <- ncol(sliding_tukey_data)
    normal_log <- FALSE
    try(normal_log <- def.distribution(sliding_tukey_data[i,], plot.it = FALSE)$lognorm)
    n_data_tukey <- rbind(n_data_tukey,n)
    
    ci_qq <- NA
    
    modi_qq <- reflim(sliding_tukey_data[i,], n.min = 100, log.mode = normal_log)
    if(!is.na(modi_qq[3]) || !is.na(modi_qq[4])){
      if(!(modi_qq[3] >= modi_qq[4]))
        try(ci_qq <- ci.quant95(n = length(sliding_tukey_data[i,]), lower.limit = modi_qq[3], upper.limit = modi_qq[4], lognorm = normal_log))
    }
    
    # Mean
    mean_tukey <- modi_qq[1]
    sliding_mean_tukey <- rbind(sliding_mean_tukey,mean_tukey)
    
    # Quantile
    quantiles_tukey <- modi_qq[[3]]
    sliding_2_5_tukey <- rbind(sliding_2_5_tukey,quantiles_tukey)
    quantiles_tukey <- modi_qq[[4]]
    sliding_97_5_tukey <- rbind(sliding_97_5_tukey,quantiles_tukey)
    
    # 95% Confidence Interval
    if(!is.na(ci_qq)[[1]]){
      confidence_tukey_2_5 <- c(ci_qq[[1]], ci_qq[[2]])
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
      confidence_tukey_97_5 <- c(ci_qq[[3]], ci_qq[[4]])
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
    } else{
      confidence_tukey_2_5 <- c(NA, NA)
      confidence_with_tukey_2_5 <- rbind(confidence_with_tukey_2_5, confidence_tukey_2_5)
      confidence_tukey_97_5 <- c(NA, NA)
      confidence_with_tukey_97_5 <- rbind(confidence_with_tukey_97_5, confidence_tukey_97_5)
    }
  }

  ##################################### Preprocessing #############################################
  
  sliding_age <- c(0, sliding_age, max(sliding_window_data$age_days))
  sliding_n_data_tukey <- c(n_data_tukey[1,1], n_data_tukey[,1], n_data_tukey[nrow(n_data_tukey),1])
  sliding_age_to <- c(0, sliding_age_to, max(sliding_window_data$age_days))
  
  sliding_mean_tukey <- c(sliding_mean_tukey[1,1], sliding_mean_tukey[,1], sliding_mean_tukey[length(sliding_mean_tukey[,1]), 1])
  sliding_2_5_tukey <- c(sliding_2_5_tukey[1, 1], sliding_2_5_tukey[,1], sliding_2_5_tukey[length(sliding_2_5_tukey[,1]), 1])
  sliding_97_5_tukey <- c(sliding_97_5_tukey[1, 1], sliding_97_5_tukey[,1], sliding_97_5_tukey[length(sliding_97_5_tukey[,1]), 1])
  
  sliding_with_tukey_2_5_from <- c(confidence_with_tukey_2_5[1,1], confidence_with_tukey_2_5[,1], confidence_with_tukey_2_5[1, length(confidence_with_tukey_2_5)])
  sliding_with_tukey_2_5_to <- c(confidence_with_tukey_2_5[1,2], confidence_with_tukey_2_5[,2], confidence_with_tukey_2_5[2, length(confidence_with_tukey_2_5)])
  sliding_with_tukey_97_5_from <- c(confidence_with_tukey_97_5[1,1], confidence_with_tukey_97_5[,1], confidence_with_tukey_97_5[1, length(confidence_with_tukey_97_5)])
  sliding_with_tukey_97_5_to <- c(confidence_with_tukey_97_5[1,2], confidence_with_tukey_97_5[,2], confidence_with_tukey_97_5[2, length(confidence_with_tukey_97_5)])
  
  slide_tukey <<- data.frame("Age-Range from" = sliding_age,
                             "to [Days]" = sliding_age_to,
                             "Age-Range from" = round_df(sliding_age/365, 3),
                             "to [Years]" = round_df(sliding_age_to/365, 3),
                             "2.5 % RI" = sliding_2_5_tukey,
                             "97.5% RI" = sliding_97_5_tukey, 
                             "95% CI (2.5% RI) from"  = sliding_with_tukey_2_5_from,
                             "to"                     = sliding_with_tukey_2_5_to,
                             "95% CI (97.5% RI) from" = sliding_with_tukey_97_5_from,
                             "to"                     = sliding_with_tukey_97_5_to, 
                             "Number of data points"  = sliding_n_data_tukey, check.names = FALSE)
  return(slide_tukey)
}

####################################### Metrics ###################################################
#' 
#' #' Mean Absolute Error for the window methods
#' mae_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_tukey_age <- window_data_tukey$age_days
#'   mae_data_window_data_tukey <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_age)-1))){
#'     mae_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i],window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
#'     mae_data_window_data_tukey <- rbind(mae_data_window_data_tukey, mae_data_save)
#'   }
#'   
#'   mae_data_window_data_tukey <- mae_data_window_data_tukey[which(mae_data_window_data_tukey$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_mae <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mae <- rbind(value_mae, mae_data_window_data_tukey[mae_data_window_data_tukey$age_days == i,][[2]])
#'   }
#' 
#'   mae_window_data_tukey <- mae(data_analyte_short$value, value_mae[[1]])
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
#'   mae_data_window_data_tukey_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
#'     mae_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]),round(window_data_tukey_rpart$age_days[i+1])), 
#'                                 value = window_data_tukey_rpart$mean[i])
#'     mae_data_window_data_tukey_rpart <- rbind(mae_data_window_data_tukey_rpart, mae_data_save)
#'   }
#'   
#'   mae_data_window_data_tukey_rpart <- mae_data_window_data_tukey_rpart[which(mae_data_window_data_tukey_rpart$age_days %in% 
#'                                                                                data_analyte_short$age_days),]
#'   
#'   value_mae <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mae <- rbind(value_mae, mae_data_window_data_tukey_rpart[mae_data_window_data_tukey_rpart$age_days == i,][[2]])
#'   }
#' 
#'   mae_window_data_tukey_rpart <- mae(data_analyte_short$value, value_mae[[1]])
#'   
#'   return(c(mae_window_data, mae_window_data_tukey, mae_window_data_rpart, mae_window_data_tukey_rpart))
#' }
#' 
#' 
#' #' Mean squared Error for the window methods
#' mse_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_tukey_age <- window_data_tukey$age_days
#'   mse_data_window_data_tukey <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_age)-1))){
#'     mse_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i],window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
#'     mse_data_window_data_tukey <- rbind(mse_data_window_data_tukey, mse_data_save)
#'   }
#'   
#'   mse_data_window_data_tukey <- mse_data_window_data_tukey[which(mse_data_window_data_tukey$age_days  %in% data_analyte_short$age_days),]
#'   
#'   value_mse <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mse <- rbind(value_mse, mse_data_window_data_tukey[mse_data_window_data_tukey$age_days == i,][[2]])
#'   }
#'  
#'   mse_window_data_tukey <- mse(data_analyte_short$value, value_mse[[1]])
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
#'   mse_data_window_data_tukey_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
#'     mse_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]),round(window_data_tukey_rpart$age_days[i+1])), value = window_data_tukey_rpart$mean[i])
#'     mse_data_window_data_tukey_rpart <- rbind(mse_data_window_data_tukey_rpart, mse_data_save)
#'   }
#'   
#'   mse_data_window_data_tukey_rpart <- mse_data_window_data_tukey_rpart[which(mse_data_window_data_tukey_rpart$age_days  %in% data_analyte_short$age_days),]
#'   
#'   value_mse <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mse <- rbind(value_mse, mse_data_window_data_tukey_rpart[mse_data_window_data_tukey_rpart$age_days == i,][[2]])
#'   }
#' 
#'   mse_window_data_tukey_rpart <- mse(data_analyte_short$value, value_mse[[1]])
#'   
#'   return(c(mse_window_data, mse_window_data_tukey, mse_window_data_rpart,mse_window_data_tukey_rpart))
#' }
#' 
#' 
#' #' Root mean squared error for the Window-methods
#' rmse_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_tukey_age <- window_data_tukey$age_days
#'   rmse_data_window_data_tukey <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_age)-1))){
#'     rmse_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i],window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
#'     rmse_data_window_data_tukey <- rbind(rmse_data_window_data_tukey, rmse_data_save)
#'   }
#'   
#'   rmse_data_window_data_tukey <- rmse_data_window_data_tukey[which(rmse_data_window_data_tukey$age_days  %in% data_analyte_short$age_days),]
#'   
#'   value_rmse <- data.frame()
#'   for(i in data_analyte_short$age_days){ 
#'     value_rmse <- rbind(value_rmse, rmse_data_window_data_tukey[rmse_data_window_data_tukey$age_days == i,][[2]])
#'   }
#'   
#'   rmse_window_data_tukey <- rmse(data_analyte_short$value, value_rmse[[1]])
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
#'   rmse_data_window_data_tukey_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
#'     rmse_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]), round(window_data_tukey_rpart$age_days[i+1])), 
#'                                  value = window_data_tukey_rpart$mean[i])
#'     rmse_data_window_data_tukey_rpart <- rbind(rmse_data_window_data_tukey_rpart, rmse_data_save)
#'   }
#'   
#'   rmse_data_window_data_tukey_rpart <- rmse_data_window_data_tukey_rpart[which(
#'     rmse_data_window_data_tukey_rpart$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_rmse <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_rmse <- rbind(value_rmse, rmse_data_window_data_tukey_rpart[rmse_data_window_data_tukey_rpart$age_days == i,][[2]])
#'   }
#'   
#'   rmse_window_data_tukey_rpart <- rmse(data_analyte_short$value, value_rmse[[1]])
#'   
#'   return(c(rmse_window_data, rmse_window_data_tukey, rmse_window_data_rpart, rmse_window_data_tukey_rpart))
#' }
#' 
#' 
#' #' R-Squared
#' r_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_tukey_age <- window_data_tukey$age_days
#'   rsquare_data_window_data_tukey <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_age)-1))){
#'     rsquare_data_save <- data.frame(age_days = seq(window_data_tukey$age_days[i], window_data_tukey$age_days[i+1]), value = window_data_tukey$mean[i])
#'     rsquare_data_window_data_tukey <- rbind(rsquare_data_window_data_tukey, rsquare_data_save)
#'   }
#'   
#'   rsquare_data_window_data_tukey <- rsquare_data_window_data_tukey[which(rsquare_data_window_data_tukey$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_rsquare <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_tukey[rsquare_data_window_data_tukey$age_days == i,][[2]])
#'   }
#'   
#'   rsquare_window_data_tukey <- rsq(data_analyte_short$value, value_rsquare[[1]])
#'   
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_tukey_rpart_age <- round(window_data_tukey_rpart$age_days)
#'   rsquare_data_window_data_tukey_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_tukey_rpart_age)-1))){
#'     rsquare_data_save <- data.frame(age_days = seq(round(window_data_tukey_rpart$age_days[i]),round(window_data_tukey_rpart$age_days[i+1])), value = window_data_tukey_rpart$mean[i])
#'     rsquare_data_window_data_tukey_rpart <- rbind(rsquare_data_window_data_tukey_rpart, rsquare_data_save)
#'   }
#'   
#'   rsquare_data_window_data_tukey_rpart <- rsquare_data_window_data_tukey_rpart[which(
#'     rsquare_data_window_data_tukey_rpart$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_rsquare <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_tukey_rpart[rsquare_data_window_data_tukey_rpart$age_days == i,][[2]])
#'   }
#'   
#'   rsquare_window_data_tukey_rpart <- rsq(data_analyte_short$value, value_rsquare[[1]])
#' 
#'   return(c(rsquare_window_data, rsquare_window_data_tukey, rsquare_window_data_rpart,rsquare_window_data_tukey_rpart))
#' }