###################################################################################################
####################################### Script for the Window-Methods #############################
###################################################################################################

####################################### Decision Tree #############################################

#' Build a Decision Tree for the data with the package rpart
#'
#' @param df Dataset with the columns value, age_days and sex.
#' @param tree_cp Complexity hyperparameter for rpart.
#' @param tree_minsplit minsplit hyperparameter for rpart.
make_rpart <- function(df, tree_cp, tree_minsplit) {
  
  if (tree_minsplit == 0 || tree_minsplit == "" || is.na(tree_minsplit)) {
    tree_minsplit <- 20
  } else{
    tree_minsplit <- as.numeric(tree_minsplit)
  }
  
  if (tree_cp == 0 || tree_cp == "" || is.na(tree_cp)) {
    tree_cp <- 0.01
  } else{
    tree_cp <- as.numeric(tree_cp)
  }
  
  rpart_ <<- rpart(value ~ age_days + sex, data = df, 
                   control = rpart.control(cp = tree_cp, minsplit = tree_minsplit))
}

#' Function to draw reflim lines to iterate for each RI
#' 
#' @param df Dataset.
#' @param g ggplot.
#' @param subset Subset of the dataset.
#' @param ri_color Color for ggplot.
#' @param xlog Logarithmic x-axis.
#' @param reflim_n.min n.min parameter for reflimR.
iterate_ri_rpart <- function(df, g, subset, ri_color, reflim_n.min = 40) {
  
  if (reflim_n.min <= 39 || is.na(reflim_n.min) || reflim_n.min == "") {
    reflim_n.min = 40
  }
  
  if (nrow(subset) != 0) {
    for (i in 1:nrow(subset)) {
      x <- subset[i,]
      
      tryCatch({
        reflim_results <-
          df %>% filter(leaf %in% x$leaf) %>% pull(value) %>%
          reflim(n.min = reflim_n.min, plot.it = FALSE)
        
        reflim_results_sex <- df %>% filter(leaf %in% x$leaf)
        reflim_results_sex <- reflim_results_sex$sex %>% unique()
        
        if ("F" %in% reflim_results_sex &&
            "M" %in% reflim_results_sex) {
          reflim_results_sex <- "MF"
        }
        
          g <-
            g + geom_rect(
              alpha = 0,
              colour = ri_color,
              xmin = x$min_age,
              xmax = x$max_age,
              ymin = reflim_results$limits['lower.lim'],
              ymax = reflim_results$limits['upper.lim']
            )
        
        ri_results <-
          rbind(
            ri_results,
            c(
              x$min_age,
              x$max_age,
              reflim_results_sex,
              reflim_results$limits['lower.lim'],
              reflim_results$limits['upper.lim']
            )
          )
      },
      # use quantile if reflim is not working
      
      error = function(e) {
        quantile_results <- df %>% filter(leaf %in% x$leaf) %>% pull(value)
        
        quantile_results_sex <- df %>% filter(leaf %in% x$leaf)
        quantile_results_sex <- quantile_results_sex$sex %>% unique()
        
        if ("F" %in% quantile_results_sex &&
            "M" %in% quantile_results_sex) {
          quantile_results_sex <- "MF"
        }
        
        i
          g <-
            g + geom_rect(
              alpha = 0,
              colour = ri_color,
              xmin = x$min_age,
              xmax = x$max_age,
              ymin = as.numeric(quantile(quantile_results, 0.025)),
              ymax = as.numeric(quantile(quantile_results, 0.975))
            )
        
        ri_results <-
          rbind(
            ri_results,
            c(
              x$min_age,
              x$max_age,
              reflim_results_sex,
              as.numeric(quantile(quantile_results, 0.025)),
              as.numeric(quantile(quantile_results, 0.975))
            )
          )
      })
    }
  }
  ri_results <<- ri_results
  return(g)
}

#' draw_ri_rpart two draw the data
#'
#' @param df Dataset
#' @param g ggplot
#' @param sex If dataset is for female or male
#' @param is_unisex If dataset is for female and male
#' @param xlog Logarithmic x-axis
#' @param reflim_n.min n.min parameter for reflimR
draw_ri_rpart <- function(df, g, sex, is_unisex = FALSE, reflim_n.min) {
  
  #define color of reference interval box
  ri_color <-
    case_when(
      sex == 'M' & is_unisex == FALSE ~ 'cornflowerblue',
      sex == 'F' & is_unisex == FALSE ~ 'indianred',
      sex %in% c('M', 'F') &
        is_unisex == TRUE ~ 'black'
    )
  
  leafs_unisex_group <-
    df %>%  group_by(leaf) %>%  summarise(n_sex = n_distinct(sex)) %>% filter(n_sex == 2) %>% pull(leaf)
  
  if (is_unisex == TRUE) {
    #get the groups which are unisex
    
    #create subset of unisex_groups
    subset <-
      df %>% filter(leaf %in% leafs_unisex_group) %>% group_by(leaf) %>% summarise(
        n = n(),
        min_age = min(age_days),
        max_age = max(age_days)
      ) %>%
      arrange(min_age, max_age)
    #draw ri
    g <- iterate_ri_rpart(df, g, subset, ri_color, reflim_n.min)
  }
  else if (is_unisex == FALSE) {
    subset <-
      df %>% group_by(leaf, sex) %>% summarise(
        n = n(),
        min_age = min(age_days),
        max_age = max(age_days),
        .groups = 'drop'
      ) %>%
      arrange(min_age, max_age) %>% filter(!leaf %in% leafs_unisex_group & sex == !!sex) %>% ungroup()
    #draw ri
    if (nrow(subset) > 0) {
      g <- iterate_ri_rpart(df, g, subset, ri_color, reflim_n.min)
    }
  }
  return(g)
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
#' @param n.min n.min for reflim()
#' @param plot_log Plot the function lognorm() of each age groups
window_method <- function(data, window, method = "reflim", n.min = 100, plot_log = FALSE) {
  # Prepare result containers
  mean_list <- list()
  quantiles_list <- list()
  ci_list <- list()
  count_list <- integer()
  
  # Define the sequence of upper bounds for each age window
  age_sequence <- seq(min(data$age_days) + window, max(data$age_days) + window, by = window)
  
  for (age_code in age_sequence) {
    
    # The data subset
    age_data <- subset(data, data$age_days <= age_code)                           # Under the condition
    age_data <- subset(age_data, age_data$age_days > age_code - window)           # Below the lowest condition
    
    # Special case for the first window (starting at 0)
    if (age_code - window == 0) {
      age_data <- subset(age_data, age_data$age_days >= age_code - window)
    }
    
    # Save number of data points for this window
    count_list <- c(count_list, nrow(age_data))
    
    # Only proceed if there is enough data
    if (method == "reflim") {
      result <- tryCatch({
        res <- reflim(age_data$value, n.min = n.min)
        list(mean = res$stats, quantiles = res[[3]], ci = res$confidence.int)
      }, error = function(e) {
        NULL
      })
      
      # Save results if successful
      if (!is.null(result)) {
        mean_list[[length(mean_list) + 1]] <- result$mean
        quantiles_list[[length(quantiles_list) + 1]] <- result$quantiles
        ci_list[[length(ci_list) + 1]] <- result$ci
      }
    }
  }
  
  # Convert results from lists to data frames
  mean_df <- do.call(rbind, mean_list)
  quantiles_df <- do.call(rbind, quantiles_list)
  ci_df <- do.call(rbind, ci_list)
  
  # Create age midpoints
  age_midpoints <- seq(min(data$age_days), max(age_sequence), by = window)
  
  # Main result for plotting or processing
  window_data <- data.frame(
    age_days = age_midpoints,
    mean = c(mean_df[, 1], tail(mean_df[, 1], 1)),
    quantile1 = c(quantiles_df[, 1], tail(quantiles_df[, 1], 1)),
    quantile2 = c(quantiles_df[, 2], tail(quantiles_df[, 2], 1))
  )
  
  # Full summary table (for download or inspection)
  window_data_all <- data.frame(
    "Age-range from [days]" = seq(min(data$age_days), max(age_sequence) - 1, by = window),
    "to [days]" = c(head(seq(min(data$age_days) + window, max(age_sequence), by = window), -1), max(data$age_days)),
    "Age-range from [years]" = round(seq(min(data$age_days), max(age_sequence) - 1, by = window) / 365, 3),
    "to [years]" = c(round(head(seq(min(data$age_days) + window, max(age_sequence), by = window) / 365, -1), 3), max(data$age)),
    "2.5% RI" = quantiles_df[, 1],
    "97.5% RI" = quantiles_df[, 2],
    "95% CI (2.5%) from" = ci_df[, 1],
    "to" = ci_df[, 2],
    "95% CI (97.5%) from" = ci_df[, 3],
    "to" = ci_df[, 4],
    "Number of data points" = count_list[1:nrow(quantiles_df)], check.names = FALSE
  )
  
  # Return both outputs as a list
  return(list(
    window_data = window_data,
    window_data_all = window_data_all
  ))
}

####################################### Window-Method coupled to the LIS ##########################

#' Window-method with LIS the reflim
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
#' @param split range of the window
#' @param method reflim
#' @param n.min n.min for reflim()
#' @param If from regular window/LIS
#' @param plot_log Plot the function lognorm() of each age groups
window_method_lis <- function(data_, split, method = "reflim", n.min = 100, plot_log = FALSE){
  
  mean_list <-  data.frame() 
  quantiles_list <- data.frame()
  confidence_list <- data.frame()
  count_list <- data.frame()
  
  for (i in seq(2,length(split))) {
    
    age_code <- split[i]
    
    # The data subset 
    age_data <- subset(data_, data_$age_days <= split[i])                     # Under the condition
    age_data_ready <- subset(age_data, age_data$age_days > split[i - 1])      # Below the lowest condition
    if (split[i - 1] == 0) {
      age_data_ready <- subset(age_data, age_data$age_days >= split[i - 1])}  # Below the lowest condition
    
    if (plot_log) {
      # Plot the distribution of each group to check for normally distribution
      try(lognorm(age_data_ready$value, plot.it = TRUE, main = paste("between", split[i - 1], "and", split[i], "days"))$lognorm)
    }
    count_list <- rbind(count_list, nrow(age_data_ready))
    
    if (method == "reflim") {
      
      modi_qq <- tryCatch({
        res <- reflim(age_data_ready$value, n.min = n.min, plot.it = FALSE)
      }, error = function(e) {
        NULL
      })
      
      mean_list <- rbind(mean_list, modi_qq$stats)
      quantiles_list <- rbind(quantiles_list, modi_qq[[3]])
      confidence_list <- rbind(confidence_list, modi_qq$confidence.int)
    }
  }
  
  # Window-Data with reflim()
  window_data <- data.frame(age_days = split,
                            mean = c(mean_list[,1], tail(mean_list[,1], n = 1)), 
                            quantile1 = c(quantiles_list[,1],tail(quantiles_list[,1], n = 1)),
                            quantile2 = c(quantiles_list[,2],tail(quantiles_list[,2], n = 1)))
  
  window_data_all <- data.frame("Age-range from [days]"    = split[1:length(split) - 1],
                                "to [days]"                = split[2:length(split)],
                                "Age-range from [years]"   = round_df(split[1:length(split) - 1]/365, 3),
                                "to [years]"               = round_df(split[2:length(split)]/365, 3),
                                "2.5% RI"                  = c(quantiles_list[,1]),
                                "97.5% RI"                 = c(quantiles_list[,2]),
                                "95% CI (2.5% RI) from"    = c(confidence_list[,1]),
                                "to"                       = c(confidence_list[,2]),
                                "95% CI (97.5% RI) from"   = c(confidence_list[,3]),
                                "to"                       = c(confidence_list[,4]),
                                "Number of data points"    = c(count_list[,1]), check.names = FALSE)
  
  # Return both outputs as a list
  return(list(
    window_data = window_data,
    window_data_all = window_data_all
  ))
}