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

#' Regular Window-method with the reflim
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
#' @param method reflim
window_method <- function(data_, window, method){

  ##################################### Mean ######################################################
  mean_with_reflimR <-  data.frame() 
  ##################################### Quantiles #################################################
  quantiles_with_reflimR <- data.frame()
  ##################################### Confidence Interval (95%) for the RI ######################
  confidence_with_reflimR <- data.frame()
  n_data_reflimR <- data.frame()
  
  for (i in seq(min(data_[,4]) + window, max(data_[,4]) + window, by = window)) {
    
    age_code <- i
    
    # The data subset
    age_data <- subset(data_, data_[,4] <= i)                            # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > i - window)     # Below the lowest condition
    
    if (i - window == 0) {
      age_data_ready <- subset(age_data, age_data$age_days >= i - window)}
    
    normal_log <- FALSE
    try(normal_log <- lognorm(age_data_ready$value, plot.it = FALSE)$lognorm)
    n_data_reflimR <- rbind(n_data_reflimR, nrow(age_data_ready))
  
    ##################################### reflimR ################################################
    if (method == "reflim") {
      try(modi_qq <- reflim(age_data_ready$value, lognormal = normal_log))
      
      mean_with_reflimR <- rbind(mean_with_reflimR, modi_qq$stats)
      quantiles_with_reflimR <- rbind(quantiles_with_reflimR, modi_qq[[3]])
      confidence_with_reflimR <- rbind(confidence_with_reflimR, modi_qq$confidence.int)
    }
  }
    
  ##################################### Save the data #############################################
  
  # Window-Data with the reflim()
  
  window_data_reflimR <<- data.frame(age_days = seq(min(data_[,4]), age_code, by = window), 
                                   mean = c(mean_with_reflimR[,1], tail(mean_with_reflimR[,1], n = 1)), 
                                   quantile1 = c(quantiles_with_reflimR[,1],tail(quantiles_with_reflimR[,1], n = 1)),
                                   quantile2 = c(quantiles_with_reflimR[,2],tail(quantiles_with_reflimR[,2], n = 1)))
  
  ##################################### Tables to download ########################################
  
  window_data_all_reflimR <<- data.frame("Age-range from"           = seq(min(data_[,4]), age_code - 1, by = window),
                                       "to [Days]"                = c(head(seq(min(data_[,4]) + window, age_code, by = window),-1),max(data_$age_days)),
                                       "Age-range from"           = round_df(seq(min(data_[,4]), age_code - 1, by = window)/365, 3),
                                       "to [Years]"               = c(round_df(head(seq(min(data_[,4]) + window, age_code, by = window)/365,-1),3), max(data_$age)),
                                       "2.5 % RI"                 = c(quantiles_with_reflimR[,1]),
                                       "97.5% RI"                 = c(quantiles_with_reflimR[,2]),
                                       "95% CI (2.5% RI) from"    = c(confidence_with_reflimR[,1]),
                                       "to"                       = c(confidence_with_reflimR[,2]),
                                       "95% CI (97.5% RI) from"   = c(confidence_with_reflimR[,3]),
                                       "to"                       = c(confidence_with_reflimR[,4]),
                                       "Number of data points"    = c(n_data_reflimR[,1]), check.names = FALSE)
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
#' @param method reflim
#' @param plot_log Plot the function lognorm() of each age groups
window_method_split <- function(data_window_split, split, method, plot_log = FALSE){
  
  ##################################### Mean ######################################################
  mean_with_reflimR <-  data.frame() 
  ##################################### Quantile ##################################################
  quantiles_with_reflimR <- data.frame() 
  ##################################### Confidence Interval (95%) for the RI ######################
  confidence_with_reflimR <- data.frame()
  n_data_reflimR <- data.frame()
  
  for (i in seq(2,length(split))) {
    
    age_code <- split[i]

    # The data subset 
    age_data <- subset(data_window_split, data_window_split$age_days <= split[i])  # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i - 1])             # Below the lowest condition
    if (split[i - 1] == 0) {
      age_data_ready <- subset(age_data, age_data$age_days >= split[i - 1])}  # Below the lowest condition

    normal_log <- FALSE
    
    if (!plot_log) {
      try(normal_log <- lognorm(age_data_ready$value, plot.it = FALSE)$lognorm)
    } else{
      # Plot the distribution of each group to check for normally distribution
      try(normal_log <- lognorm(age_data_ready$value, plot.it = TRUE, main = paste("between", split[i-1], "and", split[i], "days"))$lognorm)
    }
    n_data_reflimR <- rbind(n_data_reflimR, nrow(age_data_ready))
    
    ################################### reflimR ##################################################
    if (method == "reflim") {
      
      try(modi_qq <- reflim(age_data_ready$value, lognormal = normal_log, plot.it = FALSE))

      mean_with_reflimR <- rbind(mean_with_reflimR, modi_qq$stats)
      quantiles_with_reflimR <- rbind(quantiles_with_reflimR, modi_qq[[3]])
      confidence_with_reflimR <- rbind(confidence_with_reflimR, modi_qq$confidence.int)
    }
  }
  
  ##################################### Save the data #############################################
  
  # Window-Data with reflim()
  window_data_reflimR_rpart <<- data.frame(age_days = split,                                    
                                         mean = c(mean_with_reflimR[,1], tail(mean_with_reflimR[,1], n = 1)), 
                                         quantile1 = c(quantiles_with_reflimR[,1], tail(quantiles_with_reflimR[,1], n = 1)),
                                         quantile2 = c(quantiles_with_reflimR[,2], tail(quantiles_with_reflimR[,2], n = 1)))

  ##################################### Tables to download ########################################

  window_data_split_reflimR <<- data.frame("Age-range from"         = split[1:length(split) - 1],
                                         "to [Days]"              = split[2:length(split)],
                                         "Age-range from"         = round_df(split[1:length(split) - 1]/365, 3),
                                         "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                         "2.5 % RI"                 = c(quantiles_with_reflimR[,1]),
                                         "97.5% RI"                 = c(quantiles_with_reflimR[,2]),
                                         "95% CI (2.5% RI) from"    = c(confidence_with_reflimR[,1]),
                                         "to"                       = c(confidence_with_reflimR[,2]),
                                         "95% CI (97.5% RI) from"   = c(confidence_with_reflimR[,3]),
                                         "to"                       = c(confidence_with_reflimR[,4]),
                                         "Number of data points"    = c(n_data_reflimR[,1]), check.names = FALSE)
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
#' @param method reflim
#' @param plot_log Plot the function lognorm() of each age groups
window_method_lis <- function(data_window_split, split, method, plot_log = FALSE){
  
  ##################################### Mean ######################################################
  mean_with_reflimR <-  data.frame() 
  ##################################### Quantiles #################################################
  quantiles_with_reflimR <- data.frame()
  ##################################### Confidence Interval (95%) for the RI ######################
  confidence_with_reflimR <- data.frame()
  n_data_reflimR <- data.frame()
  
  for (i in seq(2,length(split))) {
    
    age_code <- split[i]
    
    # The data subset 
    age_data <- subset(data_window_split, data_window_split$age_days <= split[i])  # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i - 1])             # Below the lowest condition
    if (split[i - 1] == 0) {
      age_data_ready <- subset(age_data, age_data$age_days >= split[i - 1])}  # Below the lowest condition
    
    normal_log <- FALSE
    if (!plot_log) {
      try(normal_log <- lognorm(age_data_ready$value, plot.it = FALSE)$lognorm)
    } else{
      # Plot the distribution of each group to check for normally distribution
      try(normal_log <- lognorm(age_data_ready$value, plot.it = TRUE, main = paste("between", split[i-1], "and", split[i], "days"))$lognorm)
    }
    n_data_reflimR <- rbind(n_data_reflimR, nrow(age_data_ready))
  
    ################################### reflimR ##################################################
    if (method == "reflim") {

      try(modi_qq <- reflim(age_data_ready$value, lognormal = normal_log, plot.it = FALSE))
      
      mean_with_reflimR <- rbind(mean_with_reflimR, modi_qq$stats)
      quantiles_with_reflimR <- rbind(quantiles_with_reflimR, modi_qq[[3]])
      confidence_with_reflimR <- rbind(confidence_with_reflimR, modi_qq$confidence.int)
    }
  }
  
  ##################################### Save the data #############################################
  
  # Window-Data with reflim()
  window_data_reflimR_lis <<- data.frame(age_days = split,
                                       mean = c(mean_with_reflimR[,1], tail(mean_with_reflimR[,1], n = 1)), 
                                       quantile1 = c(quantiles_with_reflimR[,1],tail(quantiles_with_reflimR[,1], n = 1)),
                                       quantile2 = c(quantiles_with_reflimR[,2],tail(quantiles_with_reflimR[,2], n = 1)))
  
  ##################################### Tables to download ########################################
  
  window_data_lis_reflimR <<- data.frame("Age-range from"         = split[1:length(split) - 1],
                                         "to [Days]"              = split[2:length(split)],
                                         "Age-range from"         = round_df(split[1:length(split) - 1]/365, 3),
                                         "to [Years]"             = round_df(split[2:length(split)]/365, 3),
                                         "2.5 % RI"                 = c(quantiles_with_reflimR[,1]),
                                         "97.5% RI"                 = c(quantiles_with_reflimR[,2]),
                                         "95% CI (2.5% RI) from"    = c(confidence_with_reflimR[,1]),
                                         "to"                       = c(confidence_with_reflimR[,2]),
                                         "95% CI (97.5% RI) from"   = c(confidence_with_reflimR[,3]),
                                         "to"                       = c(confidence_with_reflimR[,4]),
                                         "Number of data points"    = c(n_data_reflimR[,1]), check.names = FALSE)
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
#' @param outliers reflim
sliding_window <- function(sliding_window_data, width_ = 120, by_ = 20, outliers){

  # Order the data with the Index
  sliding_window_data <- sliding_window_data[order(sliding_window_data$age_days),]

  ##################################### Age for the Sliding Window ################################
  # Begin of the Sliding Window
  sliding_age <- rollapply(sliding_window_data$age_days, width = width_, by = by_, 
                           FUN = min, na.rm = TRUE, partial = TRUE,align = "left")
  # End of the Sliding Window
  sliding_age_to <- rollapply(sliding_window_data$age_days, width = width_, by = by_, 
                             FUN = max, na.rm = TRUE, partial = TRUE,align = "left")

  ##################################### Mean of the subset generated by rollapply() ###############
  sliding_mean <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                              FUN = mean, na.rm = TRUE, partial = TRUE,align = "left")

  ##################################### Data in the window ########################################
  suppressWarnings(sliding_reflimR_data <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                                  FUN = invisible, partial = TRUE, align = "left"))

  ##################################### 2.5% Percentile ###########################################
  sliding_2_5 <- rollapply(sliding_window_data$value, width = width_, by = by_, 
                           FUN = quantile, probs = c(0.025), partial = TRUE, align = "left")
  
  ##################################### 97.5% Percentile ##########################################
  sliding_97_5 <- rollapply(sliding_window_data$value, width = width_, by = by_, FUN = quantile, 
                            probs = c(0.975), partial = TRUE, align = "left")
  
  ##################################### Dataframes for the reflim and CI ##########################
  ##################################### Mean  #####################################################
  mean_with_reflimR <-  data.frame() 
  ##################################### Quantile ##################################################
  quantiles_with_reflimR <- data.frame()
  ##################################### Confidence Interval (95%) for the RI ######################
  confidence_with_reflimR <- data.frame()
  n_data_reflimR <- data.frame()
    
  for (i in seq(1,nrow(sliding_reflimR_data))) { 
    
    normal_log <- FALSE
    try(normal_log <- lognorm(sliding_reflimR_data[i,], plot.it = FALSE)$lognorm)
    n_data_reflimR <- rbind(n_data_reflimR, ncol(sliding_reflimR_data))
    
    try(modi_qq <- reflim(sliding_reflimR_data[i,], lognormal = normal_log))
    mean_with_reflimR <- rbind(mean_with_reflimR, modi_qq$stats)
    quantiles_with_reflimR <- rbind(quantiles_with_reflimR, modi_qq[[3]])
    confidence_with_reflimR <- rbind(confidence_with_reflimR, modi_qq$confidence.int)
  }
  
  sliding_age <- c(0, sliding_age, max(sliding_window_data$age_days))
  sliding_n_data_reflimR <- c(n_data_reflimR[1,1], n_data_reflimR[,1], n_data_reflimR[nrow(n_data_reflimR),1])
  sliding_age_to <- c(0, sliding_age_to, max(sliding_window_data$age_days))
  
  n_data_reflimR <- c(n_data_reflimR[1,1], n_data_reflimR[,1], n_data_reflimR[length(n_data_reflimR[,1]), 1])
  mean_with_reflimR <- c(mean_with_reflimR[1,1], mean_with_reflimR[,1], mean_with_reflimR[length(mean_with_reflimR[,1]), 1])
  quantiles_with_reflimR_2.5 <- c(quantiles_with_reflimR[1,1], quantiles_with_reflimR[,1], quantiles_with_reflimR[length(quantiles_with_reflimR[,1]), 1])
  quantiles_with_reflimR_97.5 <- c(quantiles_with_reflimR[1,2], quantiles_with_reflimR[,2], quantiles_with_reflimR[length(quantiles_with_reflimR[,2]), 1])
  confidence_with_reflimR_2.5_l <- c(confidence_with_reflimR[1,1], confidence_with_reflimR[,1], confidence_with_reflimR[length(confidence_with_reflimR[,1]), 1])
  confidence_with_reflimR_97.5_l <- c(confidence_with_reflimR[1,2], confidence_with_reflimR[,2], confidence_with_reflimR[length(confidence_with_reflimR[,2]), 1])
  confidence_with_reflimR_2.5_u <- c(confidence_with_reflimR[1,3], confidence_with_reflimR[,3], confidence_with_reflimR[length(confidence_with_reflimR[,3]), 1])
  confidence_with_reflimR_97.5_u <- c(confidence_with_reflimR[1,4], confidence_with_reflimR[,4], confidence_with_reflimR[length(confidence_with_reflimR[,4]), 1])
  
  slide_reflimR <<- data.frame("Age-range from"       = sliding_age,
                             "to [Days]"              = sliding_age_to,
                             "Age-Range from"         = round_df(sliding_age/365, 3),
                             "to [Years]"             = round_df(sliding_age_to/365, 3),
                             "2.5 % RI"               = c(quantiles_with_reflimR_2.5),
                             "97.5% RI"               = c(quantiles_with_reflimR_97.5),
                             "95% CI (2.5% RI) from"  = c(confidence_with_reflimR_2.5_l),
                             "to"                     = c(confidence_with_reflimR_97.5_l),
                             "95% CI (97.5% RI) from" = c(confidence_with_reflimR_2.5_u),
                             "to"                     = c(confidence_with_reflimR_97.5_u),
                             "Number of data points"  = c(n_data_reflimR), check.names = FALSE)
  
  return(slide_reflimR)
}

####################################### Metrics ###################################################
#' 
#' #' Mean Absolute Error for the window methods
#' mae_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_reflimR_age <- window_data_reflimR$age_days
#'   mae_data_window_data_reflimR <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_age)-1))){
#'     mae_data_save <- data.frame(age_days = seq(window_data_reflimR$age_days[i],window_data_reflimR$age_days[i+1]), value = window_data_reflimR$mean[i])
#'     mae_data_window_data_reflimR <- rbind(mae_data_window_data_reflimR, mae_data_save)
#'   }
#'   
#'   mae_data_window_data_reflimR <- mae_data_window_data_reflimR[which(mae_data_window_data_reflimR$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_mae <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mae <- rbind(value_mae, mae_data_window_data_reflimR[mae_data_window_data_reflimR$age_days == i,][[2]])
#'   }
#' 
#'   mae_window_data_reflimR <- mae(data_analyte_short$value, value_mae[[1]])
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_reflimR_rpart_age <- round(window_data_reflimR_rpart$age_days)
#'   mae_data_window_data_reflimR_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_rpart_age)-1))){
#'     mae_data_save <- data.frame(age_days = seq(round(window_data_reflimR_rpart$age_days[i]),round(window_data_reflimR_rpart$age_days[i+1])), 
#'                                 value = window_data_reflimR_rpart$mean[i])
#'     mae_data_window_data_reflimR_rpart <- rbind(mae_data_window_data_reflimR_rpart, mae_data_save)
#'   }
#'   
#'   mae_data_window_data_reflimR_rpart <- mae_data_window_data_reflimR_rpart[which(mae_data_window_data_reflimR_rpart$age_days %in% 
#'                                                                                data_analyte_short$age_days),]
#'   
#'   value_mae <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mae <- rbind(value_mae, mae_data_window_data_reflimR_rpart[mae_data_window_data_reflimR_rpart$age_days == i,][[2]])
#'   }
#' 
#'   mae_window_data_reflimR_rpart <- mae(data_analyte_short$value, value_mae[[1]])
#'   
#'   return(c(mae_window_data, mae_window_data_reflimR, mae_window_data_rpart, mae_window_data_reflimR_rpart))
#' }
#' 
#' 
#' #' Mean squared Error for the window methods
#' mse_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_reflimR_age <- window_data_reflimR$age_days
#'   mse_data_window_data_reflimR <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_age)-1))){
#'     mse_data_save <- data.frame(age_days = seq(window_data_reflimR$age_days[i],window_data_reflimR$age_days[i+1]), value = window_data_reflimR$mean[i])
#'     mse_data_window_data_reflimR <- rbind(mse_data_window_data_reflimR, mse_data_save)
#'   }
#'   
#'   mse_data_window_data_reflimR <- mse_data_window_data_reflimR[which(mse_data_window_data_reflimR$age_days  %in% data_analyte_short$age_days),]
#'   
#'   value_mse <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mse <- rbind(value_mse, mse_data_window_data_reflimR[mse_data_window_data_reflimR$age_days == i,][[2]])
#'   }
#'  
#'   mse_window_data_reflimR <- mse(data_analyte_short$value, value_mse[[1]])
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_reflimR_rpart_age <- round(window_data_reflimR_rpart$age_days)
#'   mse_data_window_data_reflimR_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_rpart_age)-1))){
#'     mse_data_save <- data.frame(age_days = seq(round(window_data_reflimR_rpart$age_days[i]),round(window_data_reflimR_rpart$age_days[i+1])), value = window_data_reflimR_rpart$mean[i])
#'     mse_data_window_data_reflimR_rpart <- rbind(mse_data_window_data_reflimR_rpart, mse_data_save)
#'   }
#'   
#'   mse_data_window_data_reflimR_rpart <- mse_data_window_data_reflimR_rpart[which(mse_data_window_data_reflimR_rpart$age_days  %in% data_analyte_short$age_days),]
#'   
#'   value_mse <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_mse <- rbind(value_mse, mse_data_window_data_reflimR_rpart[mse_data_window_data_reflimR_rpart$age_days == i,][[2]])
#'   }
#' 
#'   mse_window_data_reflimR_rpart <- mse(data_analyte_short$value, value_mse[[1]])
#'   
#'   return(c(mse_window_data, mse_window_data_reflimR, mse_window_data_rpart,mse_window_data_reflimR_rpart))
#' }
#' 
#' 
#' #' Root mean squared error for the Window-methods
#' rmse_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_reflimR_age <- window_data_reflimR$age_days
#'   rmse_data_window_data_reflimR <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_age)-1))){
#'     rmse_data_save <- data.frame(age_days = seq(window_data_reflimR$age_days[i],window_data_reflimR$age_days[i+1]), value = window_data_reflimR$mean[i])
#'     rmse_data_window_data_reflimR <- rbind(rmse_data_window_data_reflimR, rmse_data_save)
#'   }
#'   
#'   rmse_data_window_data_reflimR <- rmse_data_window_data_reflimR[which(rmse_data_window_data_reflimR$age_days  %in% data_analyte_short$age_days),]
#'   
#'   value_rmse <- data.frame()
#'   for(i in data_analyte_short$age_days){ 
#'     value_rmse <- rbind(value_rmse, rmse_data_window_data_reflimR[rmse_data_window_data_reflimR$age_days == i,][[2]])
#'   }
#'   
#'   rmse_window_data_reflimR <- rmse(data_analyte_short$value, value_rmse[[1]])
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_reflimR_rpart_age <- round(window_data_reflimR_rpart$age_days)
#'   rmse_data_window_data_reflimR_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_rpart_age)-1))){
#'     rmse_data_save <- data.frame(age_days = seq(round(window_data_reflimR_rpart$age_days[i]), round(window_data_reflimR_rpart$age_days[i+1])), 
#'                                  value = window_data_reflimR_rpart$mean[i])
#'     rmse_data_window_data_reflimR_rpart <- rbind(rmse_data_window_data_reflimR_rpart, rmse_data_save)
#'   }
#'   
#'   rmse_data_window_data_reflimR_rpart <- rmse_data_window_data_reflimR_rpart[which(
#'     rmse_data_window_data_reflimR_rpart$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_rmse <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_rmse <- rbind(value_rmse, rmse_data_window_data_reflimR_rpart[rmse_data_window_data_reflimR_rpart$age_days == i,][[2]])
#'   }
#'   
#'   rmse_window_data_reflimR_rpart <- rmse(data_analyte_short$value, value_rmse[[1]])
#'   
#'   return(c(rmse_window_data, rmse_window_data_reflimR, rmse_window_data_rpart, rmse_window_data_reflimR_rpart))
#' }
#' 
#' 
#' #' R-Squared
#' r_window <- function(){
#'   
#'   ##################################### Regular Window - Tukey ####################################
#'   window_data_reflimR_age <- window_data_reflimR$age_days
#'   rsquare_data_window_data_reflimR <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_age)-1))){
#'     rsquare_data_save <- data.frame(age_days = seq(window_data_reflimR$age_days[i], window_data_reflimR$age_days[i+1]), value = window_data_reflimR$mean[i])
#'     rsquare_data_window_data_reflimR <- rbind(rsquare_data_window_data_reflimR, rsquare_data_save)
#'   }
#'   
#'   rsquare_data_window_data_reflimR <- rsquare_data_window_data_reflimR[which(rsquare_data_window_data_reflimR$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_rsquare <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_reflimR[rsquare_data_window_data_reflimR$age_days == i,][[2]])
#'   }
#'   
#'   rsquare_window_data_reflimR <- rsq(data_analyte_short$value, value_rsquare[[1]])
#'   
#'   
#'   ##################################### Window with Decision Tree - Tukey #########################
#'   window_data_reflimR_rpart_age <- round(window_data_reflimR_rpart$age_days)
#'   rsquare_data_window_data_reflimR_rpart <- data.frame()
#'   
#'   for(i in seq(1:(length(window_data_reflimR_rpart_age)-1))){
#'     rsquare_data_save <- data.frame(age_days = seq(round(window_data_reflimR_rpart$age_days[i]),round(window_data_reflimR_rpart$age_days[i+1])), value = window_data_reflimR_rpart$mean[i])
#'     rsquare_data_window_data_reflimR_rpart <- rbind(rsquare_data_window_data_reflimR_rpart, rsquare_data_save)
#'   }
#'   
#'   rsquare_data_window_data_reflimR_rpart <- rsquare_data_window_data_reflimR_rpart[which(
#'     rsquare_data_window_data_reflimR_rpart$age_days %in% data_analyte_short$age_days),]
#'   
#'   value_rsquare <- data.frame()
#'   for(i in data_analyte_short$age_days){
#'     value_rsquare <- rbind(value_rsquare, rsquare_data_window_data_reflimR_rpart[rsquare_data_window_data_reflimR_rpart$age_days == i,][[2]])
#'   }
#'   
#'   rsquare_window_data_reflimR_rpart <- rsq(data_analyte_short$value, value_rsquare[[1]])
#' 
#'   return(c(rsquare_window_data, rsquare_window_data_reflimR, rsquare_window_data_rpart,rsquare_window_data_reflimR_rpart))
#' }