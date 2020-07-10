###################################################################################################
####################################### Script for the Data analysis ##############################
###################################################################################################

####################################### Statistics ################################################

#' Calculate the central 95% of a normal distribution performs algorithm until length of vector does not change anymore
#' Modification of John W. Tukey’s box plot algorithm
#'
#' @param x Expects a vector x of positive numbers without NAs
#' @param perc Value for the Percentiles
#' @param log.mode When the data is lognormal distributed
#' @param print.cycles print the cycles 
#' @param plot.it Plot the modified Tukey
modified.tukey <- function(x, perc = 2.5, log.mode = FALSE, print.cycles = FALSE, plot.it = TRUE){
  
  if(log.mode){
    x <- log(x)}
  
  # Truncate.x: Modified Tukey
  truncate.x <- function(x, i){
    # i = cycle index
    # i = 1: standard quartile factor = qnorm(0.025)/qnorm(0.25)  
    # i > 1: truncated quartile factor = qnorm(0.025)/qnorm(0.2625) 
    qf <- ifelse(i==1, 
                 qnorm(perc/100)/qnorm(0.25), 
                 qnorm(perc/100)/qnorm(0.25*(1-perc/50) + perc/100))
    var1 <- median(x) - quantile(x, 0.25) # the lower half of the box
    var2 <- quantile(x, 0.75) - median(x) # the upper half of the box
    var <- min(var1, var2)                # the smaller half of the box
    lim <- c(median(x) - qf * var, median(x) + qf * var) # two truncation points
    return(subset(x, x >= lim[1] & x <= lim[2]))
  }
  
  print.progress <- function(x, i, log.mode = FALSE){
    if (log.mode){
      x <- exp(x)}
    x <- round(x, 2)
    print(paste("cycle", i, "n =", length(x), "min =", min(x), "max =", max(x))) 
  } 
  
  n0 <- 1
  n1 <- 0
  i  <- 0
  
  if(print.cycles){print.progress(x, i, log.mode = log.mode)}
  
  while (n0 > n1){
    i <- i + 1
    n0 <- length(x)
    x <- truncate.x(x, i)
    n1 <- length(x)
    if(print.cycles){
      print.progress(x, i, log.mode = log.mode)}
  }
  
  if (log.mode){x <- round(exp(x), 2)}
  
  if (plot.it){
    d <- density(x)
    hist(x, freq = FALSE, ylim = c(0, max(d$y) * 1.25), main = "Truncated distribution", xlab = "x", ylab = "Density", 
         col = "lightgrey")
    lines(d$x, d$y)
    legend("topright", bty = "n", legend = paste("Range:", min(x),"-", max(x)))
  }
  return(x)
}


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

#' Returns the Bowley skewness of a range between alpha and 1-alpha
#' 
#' @param x Expects a vector x
#' @param alpha threshold
#' @param digits Value to round the Bowley skewness
bowley <- function(x, alpha=0.25, digits=3){
  xx <- numeric.data(x, no.zero = TRUE)
  if (length(xx) < 10){stop ("The minimum number of values is 10.")}
  q1 <- quantile(xx, alpha)
  q2 <- quantile(xx, 0.5)
  q3 <- quantile(xx, 1-alpha)
  bs <- (q1 - 2 * q2 + q3) / (q3 - q1)
  return(round(bs, digits))
}


#' Compares the Bowley skewness for x and log(x)
#' returns TRUE, if a lognormal distribution should be assumed
#' 
#' @param x Expects a numeric vector x > 0
#' @param cutoff threshold
#' @param alpha threshold
#' @param plot.it Plot the result
def.lognorm <- function(x, cutoff = 0.05, alpha = 0.25, plot.it = TRUE,
                        add.tx = "distribution", setvalues = NULL){
  #xx <- x + 1  #shift avoids numbers < 1
  s <- c(NA,NA)
  s[1] <- bowley(x, alpha=alpha)
  s[2] <- bowley(log(x), alpha=alpha)
  
  if(s[1] < 0){
    logtype <- FALSE}
  else{logtype <- (s[1] - s[2]) >= cutoff}
  
  if (plot.it){
    d <- density(x)
    hist(x, freq = FALSE, ylim = c(0, max(d$y) * 1.2), xlab = "x", ylab = "Density", col = "lightgrey", main = "")
    lines(d$x, d$y)
    if (!is.null(setvalues)){
      abline(v = setvalues, lty = 2)}
    legend("topright", legend = ifelse(logtype, paste("Lognormal", add.tx), paste("Normal", add.tx)), pch = 20, col = "lightgrey")
  }
  return(list("lognormal" = logtype, "BowleySkewness" = s))
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

####################################### Preprocessing #############################################

#' Preprocessing of the data: Given age range in years
#'
#' @param data Selected data
#' @param age_end Range of the data in years
select_data <- function(data_, age_begin = 0, age_end = 123, sex = "t"){  

  # Fill PATISTAMMNR (patient) and EINSCODE (station) when not given in the dataset
  if(is.null(data_$PATISTAMMNR) || is.null(data_$EINSCODE)) {
    if(is.null(data_$PATISTAMMNR)){
      data_$PATISTAMMNR <- seq(1:nrow(data_))
    }
    if(is.null(data_$EINSCODE)){
      data_$EINSCODE <- "NA"
    }
  }

  # Important informations from the data
  suppressWarnings(data_analyte <- data.frame(patient = data_$PATISTAMMNR, 
                                              sex = data_$SEX, 
                                              age = as.integer(data_$ALTER), 
                                              age_days = as.integer(data_$ALTERTAG), 
                                              value = as.numeric(data_$ERGEBNIST1), # Non-Numeric Values are NA and deleted later
                                              code = data_$EINSCODE, 
                                              name = data_$CODE1))
    
  rows_table_ <- nrow(data_analyte) 

  # Subset the data with the age_end
  data_analyte <- subset(data_analyte, age <= age_end, select = c(patient, sex, age, age_days, value, code, name))

  if(!(age_begin == 0)){
    data_analyte <- subset(data_analyte, age >= age_begin, select = c(patient, sex, age, age_days, value, code, name))}
  
  # Deleted rows because of the subset 
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("Information!", rows_table_ - nrow(data_analyte), "values were deleted because of your subset for the age. \n"))}

  # Delete NAs
  rows_table_ <- nrow(data_analyte)
  data_analyte <- data_analyte[complete.cases(data_analyte), ]
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("Information!", rows_table_ - nrow(data_analyte), "values were NA and are deleted. \n"))}

  # Separate data on the basis of the sex
  if(sex == "m"){data_analyte <- subset(data_analyte, sex == "M", select = c(patient, sex, age, age_days, value, code, name))}
  if(sex == "w"){data_analyte <- subset(data_analyte, sex == "W", select = c(patient, sex, age, age_days, value, code, name))}

  return(data_analyte)}


#' Preprocessing of the data: Given age range in days
#'
#' @param data Selected data
#' @param age_end Range of the data in days
select_data_days <- function(data_, age_begin = 0, age_end, sex = "t"){  
  
  # Fill PATISTAMMNR (patient) and EINSCODE (station) when not given in the dataset
  if(is.null(data_$PATISTAMMNR) || is.null(data_$EINSCODE)) {
    if(is.null(data_$PATISTAMMNR)){
      data_$PATISTAMMNR <- seq(1:nrow(data_))
    }
    if(is.null(data_$EINSCODE)){
      data_$EINSCODE <- "NA"
    }
  }
  
  # Important informations from the data
  suppressWarnings(data_analyte <- data.frame(patient = data_$PATISTAMMNR, 
                                              sex = data_$SEX, 
                                              age = as.integer(data_$ALTER), 
                                              age_days = as.integer(data_$ALTERTAG), 
                                              value = as.numeric(data_$ERGEBNIST1), # Non-Numeric Values are NA and deleted later
                                              code = data_$EINSCODE, 
                                              name = data_$CODE1))
  
  rows_table_ <- nrow(data_analyte) 
  
  # Subset the data with the age_end
  data_analyte <- subset(data_analyte, age_days <= age_end, select = c(patient, sex, age, age_days, value, code, name)) 
  
  if(!(age_begin == 0)){
    data_analyte <- subset(data_analyte, age_days >= age_begin, select = c(patient, sex, age, age_days, value, code, name))}
  
  # Deleted rows because of the subset 
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("Information!", rows_table_ - nrow(data_analyte) ,"values were deleted because of your subset for the age. \n"))}
  
  # Delete NAs
  rows_table_ <- nrow(data_analyte)
  data_analyte <- data_analyte[complete.cases(data_analyte), ]
  if(!(rows_table_ == nrow(data_analyte))){
    cat(paste("Information!", rows_table_ - nrow(data_analyte) ,"values were NA and are deleted. \n"))}
  
  # Separate data on the basis of the sex
  if(sex == "m"){data_analyte <- subset(data_analyte, sex == "M", select = c(patient, sex, age, age_days, value, code, name))}
  if(sex == "w"){data_analyte <- subset(data_analyte, sex == "W", select = c(patient, sex, age, age_days, value, code, name))}
  
  return(data_analyte)}


####################################### GAMLSS Models #############################################

#' Build the GAMLSS models
#'
#' @param data_analyte Dataset
#' 
#'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param age_end Range of the data
#' @param family_gamlss Distribution of the GAMLSS models
#' @param epochs Epochs for the building of the models
#' @param method CG or RS algorithm
make_gamlss <- function(data_analyte, age_end, family_gamlss, epochs, method){
  
  helper_make_gamlss <- function(x) {return(eval(parse(text=x)))} 
  
  data_analyte <<- data_analyte
  
  model_gamlss_pb <- {paste("gamlss(value ~pb(age_days), sigma.formula = ~pb(age_days),
                            nu.formula = ~pb(age_days), tau.formula = ~pb(age_days), family =",family_gamlss,",
                            data = data_analyte, method = ",method,"(",epochs,"))")}
  pb_ <<- helper_make_gamlss(noquote(model_gamlss_pb))

  model_gamlss_cs <- {paste("gamlss(value ~ cs(age_days), sigma.formula = ~cs(age_days), 
                            nu.formula = ~cs(age_days), tau.formula = ~cs(age_days), family =",family_gamlss,",
                            data = data_analyte, method = ",method,"(",epochs,"))")}
  cs_ <<- helper_make_gamlss(noquote(model_gamlss_cs))

  model_gamlss_poly <- {paste("gamlss(value ~ poly(age_days,3), sigma.formula = ~poly(age_days,3), 
                              nu.formula = ~poly(age_days,3), tau.formula = ~poly(age_days,3), family =",family_gamlss,",
                              data = data_analyte, method = ",method,"(",epochs,"))")}
  poly_ <<- helper_make_gamlss(noquote(model_gamlss_poly))

  model_gamlss_poly4 <- {paste("gamlss(value ~ poly(age_days,4), sigma.formula = ~poly(age_days,4),
                               nu.formula = ~poly(age_days,4), tau.formula = ~poly(age_days,4), family =",family_gamlss,",
                               data = data_analyte, method = ",method,"(",epochs,"))")}
  poly4_ <<- helper_make_gamlss(noquote(model_gamlss_poly4))
  
  # Minsplit by 360 to became at least 120 values at the last leaf from the Decision Tree (only for mu, because it has the most age-dependent
  # impact)
  model_gamlss_tr <- {paste("gamlss(value ~ tr(~age_days, control = rpart.control(minsplit = 360)), sigma.formula = ~age_days, 
                            nu.formula = ~age_days, tau.formula = ~age_days, family =",family_gamlss,", 
                            data = data_analyte, method = ",method,"(",epochs,"))")}
  tr_ <<- helper_make_gamlss(noquote(model_gamlss_tr))
  
  # With 3 Hidden Units for each parameter
  model_gamlss_nn <- {paste("gamlss(value ~ nn(~age_days,size=3, decay=0.1), sigma.formula = ~nn(~age_days,size=3, decay=0.1),
                            nu.formula = ~nn(~age_days,size=3, decay=0.1), tau.formula = ~nn(~age_days,size=3, decay=0.1),
                            family =",family_gamlss,", data = data_analyte, method = ",method,"(",epochs,"))")}
  nn_ <<- helper_make_gamlss(noquote(model_gamlss_nn))
}


####################################### Decision Tree #############################################

#' Build a Decision Tree from the package rpart
#'
#' @param data_analyte the data
#' 
#' #'     patient sex age age_days value code name
#' 25 35714677   W   2      905  40.9  3.2 ALBS
#' 30 35746528   M   0       18  34.1  3.3 ALBS
#' 66 35746170   W   5     2157  27.2  3.3 ALBS
#' 68 35725954   M   1      572  41.8  3.1 ALBS
#' 71 35744894   M   0       39  27.0  3.3 ALBS
#' 73 35746153   W  16     5981  50.8  0.5 ALBS
#' 
#' @param minsplit_tree Minimum number of observations that must exist 
#' in a node in order for a split to be attempted
make_rpart <- function(data_analyte, minsplit_tree = 360){
  rpart_ <<- rpart(value~age_days, data = data_analyte, method = "anova", 
                   control = rpart.control(minsplit = minsplit_tree, cp = 0.01))
}

####################################### Residuals #################################################

#' Get the residuals of the model and fit new model with data with the cut datasets with high residuals
#' 
#' @param x Dataset 
#' 
#' #'    sex age age_days value
#' 25   W   2      905  40.9
#' 30   M   0       18  34.1
#' 66   W   5     2157  27.2
#' 68   M   1      572  41.8
#' 71   M   0       39  27.0
#' 73   W  16     5981  50.8
#' 
#' @param gamlss_family Distribution of the GAMLSS models
#' @param epochs Epochs for the building of the models
#' @param method CG or RS algorithm
#' @param residuals_cut Threshold for the Residuals
outliers_residuals <- function(data_analyte, gamlss_family, epochs, method, residuals_cut = 1.5){
  
  helper_make_gamlss <- function(x) {return(eval(parse(text=x)))} 
  
  # Save the residuals to each value and day
  residuals_pb <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, resid = pb_$residuals, 
                             patient = data_analyte$patient, sex = data_analyte$sex, age = data_analyte$age, name = data_analyte$name)
  residuals_cs <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, resid = cs_$residuals, 
                             patient = data_analyte$patient, sex = data_analyte$sex, age = data_analyte$age, name = data_analyte$name)
  residuals_poly <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, resid = poly_$residuals, 
                               patient = data_analyte$patient, sex = data_analyte$sex, age = data_analyte$age, name = data_analyte$name)
  residuals_poly4 <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, resid = poly4_$residuals, 
                                patient = data_analyte$patient, sex = data_analyte$sex, age = data_analyte$age, name = data_analyte$name)
  residuals_nn <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, resid = nn_$residuals, 
                             patient = data_analyte$patient, sex = data_analyte$sex, age = data_analyte$age, name = data_analyte$name)
  residuals_tr <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, resid = tr_$residuals, 
                             patient = data_analyte$patient, sex = data_analyte$sex, age = data_analyte$age, name = data_analyte$name)
  
  # Select data, where the residuals are <= -residuals_cut and >= residuals_cut
  outlierfree_pb <<- subset(residuals_pb, !(xor(resid >= residuals_cut, resid <= -residuals_cut)), 
                            select = c(patient, sex, age, age_days, value, resid, name))
  outlierfree_cs <<- subset(residuals_cs, !(xor(resid >= residuals_cut, resid <= -residuals_cut)), 
                            select = c(patient, sex, age, age_days, value, resid, name))
  outlierfree_poly <<- subset(residuals_poly, !(xor(resid >= residuals_cut, resid <= -residuals_cut)), 
                              select = c(patient, sex, age, age_days, value, resid, name))
  outlierfree_poly4 <<- subset(residuals_poly4, !(xor(resid >= residuals_cut, resid <= -residuals_cut)), 
                               select = c(patient, sex, age, age_days, value, resid, name))
  outlierfree_nn <<- subset(residuals_nn, !(xor(resid >= residuals_cut, resid <= -residuals_cut)), 
                            select = c(patient, sex, age, age_days, value, resid, name))
  outlierfree_tr <<- subset(residuals_tr, !(xor(resid >= residuals_cut, resid <= -residuals_cut)), 
                            select = c(patient, sex, age, age_days, value, resid, name))
  
  # Fit new models with the cutted data
  model_gamlss_pb <- {paste("gamlss(value ~ pb(age_days), sigma.formula = ~pb(age_days),
                            nu.formula = ~pb(age_days), tau.formula = ~pb(age_days), family =",gamlss_family,",
                            data = outlierfree_pb, method = ",method,"(",epochs,"))")}
  opb_ <<- helper_make_gamlss(noquote(model_gamlss_pb))
  
  model_gamlss_cs <- {paste("gamlss(value ~ cs(age_days), sigma.formula = ~cs(age_days),
                            nu.formula = ~cs(age_days), tau.formula = ~cs(age_days), family =",gamlss_family,",
                            data = outlierfree_cs, method = ",method,"(",epochs,"))")}
  ocs_ <<- helper_make_gamlss(noquote(model_gamlss_cs))
  
  model_gamlss_poly <- {paste("gamlss(value ~ poly(age_days,3), sigma.formula = ~poly(age_days,3),
                              nu.formula = ~poly(age_days,3), tau.formula = ~poly(age_days,3), family =",gamlss_family,",
                              data = outlierfree_poly, method = ",method,"(",epochs,"))")}
  opoly_ <<- helper_make_gamlss(noquote(model_gamlss_poly))
  
  model_gamlss_poly4 <- {paste("gamlss(value ~ poly(age_days,4), sigma.formula = ~poly(age_days,4),
                               nu.formula = ~poly(age_days,4), tau.formula = ~poly(age_days,4), family =",gamlss_family,",
                               data = outlierfree_poly4, method = ",method,"(",epochs,"))")}
  opoly4_ <<- helper_make_gamlss(noquote(model_gamlss_poly4))
  
  model_gamlss_tr <- {paste("gamlss(value ~ tr(~age_days, control = rpart.control(minsplit = 360)), sigma.formula = ~age_days,
                            nu.formula = ~age_days, tau.formula = ~age_days, family =",gamlss_family,",
                            data = outlierfree_tr, method = ",method,"(",epochs,"))")}
  otr_ <<- helper_make_gamlss(noquote(model_gamlss_tr))
  
  model_gamlss_nn <- {paste("gamlss(value ~ nn(~age_days,size=3, decay=0.1), sigma.formula = ~nn(~age_days,size=3, decay=0.1),
                            nu.formula = ~nn(~age_days,size=3, decay=0.1), tau.formula = ~nn(~age_days,size=3, decay=0.1),
                            family =",gamlss_family,", data = outlierfree_nn, method = ",method,"(",epochs,"))")}
  onn_ <<- helper_make_gamlss(noquote(model_gamlss_nn))
}

####################################### Discrete Model ############################################

#' Make discrete model from the GAMLSS model with a given deviation for the upper and low 
#' reference intervals. When a change occurs in the 2.5% or 97.5% RI the age group is split
#' and the mean and RI from this agegroups is calculated
#' 
#' @param model predicted GAMLSS model
#' @param deviation Deviation from the upper/lower Reference Intervals
split_gamlss <- function(model, deviation = 0.1){
  
  splitsgamlss <- data.frame()
  para_split <- 1
  i <- 1
  
  # Go through the data to the end and check if the Reference Intervals are changed
  while (i < nrow(model)){
    
    if (xor(model$C2.5[para_split]/model$C2.5[i+1] < 1-deviation, model$C2.5[para_split]/model$C2.5[i+1] > 1+deviation) ||
        xor(model$C97.5[para_split]/model$C97.5[i+1] < 1-deviation, model$C97.5[para_split]/model$C97.5[i+1] > 1+deviation)){
      
      para_split <- i # change para_split for the next age group
      splitsgamlss <- rbind(splitsgamlss, para_split)}
    
    i = i+1
  }
  return(splitsgamlss)
}