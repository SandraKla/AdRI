###################################################################################################
####################################### Script for the Data analysis ##############################
###################################################################################################

####################################### Preprocessing #############################################

#' Preprocessing of the data
#'
#' @param data_ Selected data
#' @param age_begin Range of the data (beginning)
#' @param age_end Range of the data (end)
#' @param sex Selected sex ("m", "f", or "t" for all)
#' @param unit Unit of age ("years" or "days")
#' @param unique Only unique values
select_data <- function(data_, age_begin = 0, age_end = 123, sex = "t", unit = "years", unique = TRUE) {
  
  # Fill ID (patient) and STATION (station) when not given in the dataset
  if (is.null(data_$ID)) data_$ID <- seq_len(nrow(data_))
  
  # Create clean data frame
  suppressWarnings(data_analyte <- data.frame(
    patient   = data_$ID, 
    sex       = data_$SEX, 
    age       = as.integer(data_$AGE_YEARS), 
    age_days  = as.integer(data_$AGE_DAYS), 
    value     = as.numeric(data_$VALUE),  # Non-Numeric Values are NA and deleted later
    name      = data_$ANALYTE
  ))
  
  rows_before_filter <- nrow(data_analyte)
  
  # Subset the data with the age_end
  if (unit == "years") {
    data_analyte <- subset(data_analyte, age <= age_end & age >= age_begin, select = c(patient, sex, age, age_days, value, name))
  } else if (unit == "days") {
    data_analyte <- subset(data_analyte, age_days <= age_end & age_days >= age_begin, select = c(patient, sex, age, age_days, value, name))
  }
  
  # Deleted rows because of the subset 
  rows_after_age_filter <- nrow(data_analyte)
  if (rows_before_filter != rows_after_age_filter) {
    cat(paste("*** Information!", rows_before_filter - rows_after_age_filter, "values were deleted because of your subset for the age. ***\n"))
  }
  
  # Remove NAs
  rows_before_na <- nrow(data_analyte)
  data_analyte <- data_analyte[complete.cases(data_analyte), ]
  rows_after_na <- nrow(data_analyte)
  if (rows_before_na != rows_after_na) {
    cat(paste("*** Information!", rows_before_na - rows_after_na, "values were NA and are deleted. ***\n"))
  }
  
  # Filter by sex
  if (sex == "m") {
    data_analyte <- subset(data_analyte, sex == "M", select = c(patient, sex, age, age_days, value, name))
  } else if (sex == "f") {
    data_analyte <- subset(data_analyte, sex == "F", select = c(patient, sex, age, age_days, value, name))
  }
  
  data_analyte <- data_analyte %>% filter(sex %in% c("M", "F"))
  
  # Take only the first and unique samples from the data if ID is given
  if (unique == TRUE) {
    rows_before_unique <- nrow(data_analyte)
    data_analyte <- data_analyte %>% group_by(patient) %>% filter(row_number() == 1)
    # Convert into tibble so as.data.frame()
    data_analyte <- as.data.frame(data_analyte)
    
    if (!(rows_before_unique == nrow(data_analyte))) {
      cat(paste("*** Information!", rows_before_unique - nrow(data_analyte), "values of the patients were present several times and were deleted. ***\n"))}
  }
  
  return(data_analyte)
}

#' Outlierdetection
#'
#' @param data_analyte Data frame to filter
#' @param minsplit Minimum split size for decision tree (numeric)
outlier.detection <- function(data_analyte, minsplit = 20, cp = 0.01) {

  rows_table_ <- nrow(data_analyte)
  data_analyte_withoutOutliers <- data.frame()
  
  # Make Decision Tree to group the data in groups
  rpart_ready <- make_rpart(data_analyte, as.numeric(minsplit), as.numeric(cp))
  # Read the splits from the Decision Tree
  splits <- data.frame(rpart_$splits)
  split <- round(c(0, sort(splits$index), max(data_analyte$age_days)))
  
  # Select each range of the splits and delete outliers with the iboxplot()
  for (i in 2:length(split)) {
    data_analyte_split <- subset(data_analyte, age_days <= split[i], select = c(patient, sex, age, age_days, value, name))
    data_analyte_subset <- subset(data_analyte_split, age_days > split[i - 1])
    
    if (split[i - 1] == 0) {
      data_analyte_subset <- subset(data_analyte_split, age_days >= split[i - 1])
    }
    
    # Apply Tukey outlier detection (iboxplot)
    modi <- iboxplot(data_analyte_subset$value, plot.it = FALSE)$trunc
    
    # Keep only non-outliers
    data_analyte_save <- data_analyte_subset[data_analyte_subset$value %in% modi, ]
    data_analyte_withoutOutliers <- rbind(data_analyte_withoutOutliers, data_analyte_save)
  }
  
  # Info output
  if (rows_table_ != nrow(data_analyte_withoutOutliers)) {
    cat(paste("*** Information!", rows_table_ - nrow(data_analyte_withoutOutliers), "values were deleted because of the outlier detection. ***\n"))
  }
  
  return(data_analyte_withoutOutliers)
}

####################################### Statistics ################################################

#' Calculate R^2 
#' 
#' @param y Datapoints 
#' @param y_predict Predicted values
rsq <- function(y,y_predict){
  
  ss_res <- sum((y - y_predict)^2)
  ss_tot <- sum((y - mean(y))^2)
  r_sq <- 1 - (ss_res/ss_tot)
  return(r_sq)
}

#' Calculate Mean absolute error 
#' 
#' @param y Datapoints 
#' @param y_predict Predicted values
mae <- function(y, y_predict){
  1/length(y) * sum(abs(y - y_predict))}


#' Calculate Mean squared error 
#' 
#' @param y Datapoints 
#' @param y_predict Predicted values
mse <- function(y, y_predict){
  1/length(y) * sum((y - y_predict)^2)}


#' Calculate Root mean squared error 

#' 
#' @param y Datapoints 
#' @param y_predict Predicted values
rmse <- function(y, y_predict){
  sqrt(1/length(y) * sum((y - y_predict)^2))}


#' Function for data preparation
#' 
#' @param x Expects a vector
#' @param no.zero
numeric.data <- function(x, no.zero = FALSE){
  xx <- as.numeric(as.character(x))
  xx <- xx[!is.na(xx)]
  ifelse(no.zero, xx <- xx[xx > 0], xx <- xx[xx >= 0])
  return(xx)
}

#' Round numeric values from a dataframe
#' 
#' @param x Expects a dataframe
#' @param digits Digits to round
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

#' zlog-Standardization of a single quantitative laboratory result x
#' 
#' @param x Expects a dataframe
#' @param lower.limt Lower Reference Limit
#' @param upper.limit Upper Reference Limit
zlog <- function(x, lower.limit, upper.limit){
  if (x <= 0 | lower.limit <= 0 | upper.limit <= 0) {
    stop("(zlog) All parameters must be greater than 0")
  }
  if (upper.limit <= lower.limit) {
    stop("(zlog) upper.limit must be greater than lower.limit")
  }
  
  logl <- log(lower.limit)
  logu <- log(upper.limit)
  mu.log <- (logl + logu) / 2
  sigma.log <- (logu - logl) / 3.919928
  
  return((log(x) - mu.log) / sigma.log)
}
