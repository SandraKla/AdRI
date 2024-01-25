###################################################################################################
####################################### Script for the Data analysis ##############################
###################################################################################################

####################################### Preprocessing #############################################

#' Preprocessing of the data: Given age range in years
#'
#' @param data Selected data
#' @param age_begin Range of the data in years (beginning)
#' @param age_end Range of the data in years (end)
#' @param sex Selected sex
select_data <- function(data_, age_begin = 0, age_end = 123, sex = "t"){  

  # Fill ID (patient) and STATION (station) when not given in the dataset
  if(is.null(data_$ID) || is.null(data_$STATION)) {
    if(is.null(data_$ID)){
      data_$ID <- seq(1:nrow(data_))
    }
    if(is.null(data_$STATION)){
      data_$STATION <- "NA"
    }
  }

  # Important informations from the data
  suppressWarnings(data_analyte <- data.frame(patient = data_$ID, 
                                              sex = data_$SEX, 
                                              age = as.integer(data_$AGE_YEARS), 
                                              age_days = as.integer(data_$AGE_DAYS), 
                                              value = as.numeric(data_$VALUE), # Non-Numeric Values are NA and deleted later
                                              code = data_$STATION, 
                                              name = data_$ANALYTE))
    
  rows_table_ <- nrow(data_analyte) 

  # Subset the data with the age_end
  data_analyte <- subset(data_analyte, age <= age_end, select = c(patient, sex, age, age_days, value, code, name))

  if(!(age_begin == 0)){
    data_analyte <- subset(data_analyte, age >= age_begin, select = c(patient, sex, age, age_days, value, code, name))}
  
  # Deleted rows because of the subset 
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("*** Information!", rows_table_ - nrow(data_analyte), "values were deleted because of your subset for the age. ***\n"))}

  # Delete NAs
  rows_table_ <- nrow(data_analyte)
  data_analyte <- data_analyte[complete.cases(data_analyte), ]
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("*** Information!", rows_table_ - nrow(data_analyte), "values were NA and are deleted. ***\n"))}

  # Separate data on the basis of the sex
  if(sex == "m"){data_analyte <- subset(data_analyte, sex == "M", select = c(patient, sex, age, age_days, value, code, name))}
  if(sex == "f"){data_analyte <- subset(data_analyte, sex == "F", select = c(patient, sex, age, age_days, value, code, name))}

  return(data_analyte)}


#' Preprocessing of the data: Given age range in days
#'
#' @param data Selected data
#' @param age_begin Range of the data in days (beginning)
#' @param age_end Range of the data in days (end)
#' @param sex Selected sex
select_data_days <- function(data_, age_begin = 0, age_end, sex = "t"){  
  
  # Fill ID (patient) and STATION (station) when not given in the dataset
  if(is.null(data_$ID) || is.null(data_$STATION)) {
    if(is.null(data_$ID)){
      data_$ID <- seq(1:nrow(data_))
    }
    if(is.null(data_$STATION)){
      data_$STATION <- "NA"
    }
  }
  
  # Important informations from the data
  suppressWarnings(data_analyte <- data.frame(patient = data_$ID, 
                                              sex = data_$SEX, 
                                              age = as.integer(data_$AGE_YEARS), 
                                              age_days = as.integer(data_$AGE_DAYS), 
                                              value = as.numeric(data_$VALUE), # Non-Numeric Values are NA and deleted later
                                              code = data_$STATION, 
                                              name = data_$ANALYTE))
  
  rows_table_ <- nrow(data_analyte) 
  
  # Subset the data with the age_end
  data_analyte <- subset(data_analyte, age_days <= age_end, select = c(patient, sex, age, age_days, value, code, name)) 
  
  if(!(age_begin == 0)){
    data_analyte <- subset(data_analyte, age_days >= age_begin, select = c(patient, sex, age, age_days, value, code, name))}
  
  # Deleted rows because of the subset 
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("*** Information!", rows_table_ - nrow(data_analyte) ,"values were deleted because of your subset for the age. ***\n"))}
  
  # Delete NAs
  rows_table_ <- nrow(data_analyte)
  data_analyte <- data_analyte[complete.cases(data_analyte), ]
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("*** Information!", rows_table_ - nrow(data_analyte) ,"values were NA and are deleted. ***\n"))}
  
  # Separate data on the basis of the sex
  if(sex == "m"){data_analyte <- subset(data_analyte, sex == "M", select = c(patient, sex, age, age_days, value, code, name))}
  if(sex == "f"){data_analyte <- subset(data_analyte, sex == "F", select = c(patient, sex, age, age_days, value, code, name))}
  
  return(data_analyte)}

####################################### Statistics ################################################

#' Calculate R^2 
#' 
#' @param y Datapoints 
#' @param y_predict Predicted values
rsq <- function(y,y_predict){
  
  ss_res <- sum((y-y_predict)^2)
  ss_tot <- sum((y-mean(y))^2)
  r_sq <- 1 - (ss_res/ss_tot)
  return(r_sq)
}

#' Calculate Mean absolute error 
#' 
#' @param y Datapoints 
#' @param y_predict Predicted values
mae <- function(y, y_predict){
  1/length(y)* sum(abs(y - y_predict))}


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
  if (x <= 0 | lower.limit <= 0 | upper.limit <= 0){
    stop("(zlog) All parameters must be greater than 0")
  }
  if (upper.limit <= lower.limit){
    stop("(zlog) upper.limit must be greater than lower.limit")
  }
  
  logl <- log(lower.limit)
  logu <- log(upper.limit)
  mu.log <- (logl + logu) / 2
  sigma.log <- (logu - logl) / 3.919928
  
  return((log(x) - mu.log) / sigma.log)
}
