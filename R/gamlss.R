###################################################################################################
####################################### Script for the Data analysis ##############################
###################################################################################################

helper_make_gamlss <- function(x) {return(eval(parse(text=x)))} 

####################################### GAMLSS Models #############################################

#' Build the GAMLSS models
#'
#' @param data_analyte Dataset
#' @param age_end Range of the data
#' @param family_gamlss Distribution of the GAMLSS models
#' @param epochs Epochs for the building of the models
#' @param method CG or RS algorithm
make_gamlss <- function(data_analyte, age_end, family_gamlss, epochs, method){
  
  set.seed(1)
  data_analyte <<- data_analyte
  
  model_gamlss_pb <- {paste("gamlss(value ~pb(age_days), sigma.formula = ~pb(age_days), nu.formula = ~pb(age_days), tau.formula = ~pb(age_days), 
                            family =",family_gamlss,",data = data_analyte, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - P-Splines: ***\n"))
  pb_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_pb))})

  model_gamlss_cs <- {paste("gamlss(value ~cs(age_days), sigma.formula = ~cs(age_days), nu.formula = ~cs(age_days), tau.formula = ~cs(age_days), 
                            family =",family_gamlss,", data = data_analyte, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Cubic Splines: ***\n"))
  cs_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_cs))})

  model_gamlss_poly3 <- {paste("gamlss(value ~poly(age_days,3), sigma.formula = ~poly(age_days,3), nu.formula = ~poly(age_days,3), tau.formula = ~poly(age_days,3), 
                               family =",family_gamlss,", data = data_analyte, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Polynomials Degree 3: ***\n"))
  poly_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_poly3))})

  model_gamlss_poly4 <- {paste("gamlss(value ~poly(age_days,4), sigma.formula = ~poly(age_days,4), nu.formula = ~poly(age_days,4), tau.formula = ~poly(age_days,4), 
                               family =",family_gamlss,", data = data_analyte, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Polynomials Degree 4: ***\n"))
  poly4_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_poly4))})
  
  model_gamlss_tr <- {paste("gamlss(value ~tr(~age_days), sigma.formula = ~age_days, nu.formula = ~age_days, tau.formula = ~age_days, 
                            family =",family_gamlss,", data = data_analyte, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Decision Tree: ***\n"))
  tr_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_tr))})
  

  model_gamlss_nn <- {paste("gamlss(value ~nn(~age_days,size=4, decay=0.1), sigma.formula = ~nn(~age_days,size=3, decay=0.1), nu.formula = ~nn(~age_days,size=1, decay=0.1), tau.formula = ~nn(~age_days,size=1, decay=0.1), 
                            family =",family_gamlss,", data = data_analyte, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Neural Network: ***\n"))
  nn_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_nn))})
}

make_lms <- function(data_analyte){
  cat(paste("*** LMS ***\n"))
  new_lms_data <<- data.frame(value_lms = data_analyte[[5]], age_lms = data_analyte[[4]])
  suppressWarnings({lms_ <<- lms(value_lms, age_lms, k=2, data = new_lms_data, cent=c(2.5,50,97.5), trans.x = TRUE)})
}

####################################### Residuals #################################################

#' Get the residuals of the model and fit new model with data with the cut datasets with high residuals
#' 
#' @param data_analyte Dataset
#' @param gamlss_family Distribution of the GAMLSS models
#' @param epochs Epochs for the building of the models
#' @param method CG or RS algorithm
#' @param residuals_cut Threshold for the Residuals
outliers_residuals <- function(data_analyte, gamlss_family, epochs, method, residuals_cut = 1.5){
  
  set.seed(1)
  
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
  model_gamlss_pb <- {paste("gamlss(value ~pb(age_days), sigma.formula = ~pb(age_days), nu.formula = ~pb(age_days), tau.formula = ~pb(age_days), 
                            family =",gamlss_family,", data = outlierfree_pb, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - P-Splines: ***\n"))
  opb_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_pb))})
  
  model_gamlss_cs <- {paste("gamlss(value ~cs(age_days), sigma.formula = ~cs(age_days), nu.formula = ~cs(age_days), tau.formula = ~cs(age_days), 
                            family =",gamlss_family,", data = outlierfree_cs, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Cubic Splines: ***\n"))
  ocs_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_cs))})
  
  model_gamlss_poly <- {paste("gamlss(value ~poly(age_days,3), sigma.formula = ~poly(age_days,3), nu.formula = ~poly(age_days,3), tau.formula = ~poly(age_days,3), 
                              family =",gamlss_family,", data = outlierfree_poly, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Polynomials Degree 3: ***\n"))
  opoly_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_poly))})
  
  model_gamlss_poly4 <- {paste("gamlss(value ~poly(age_days,4), sigma.formula = ~poly(age_days,4), nu.formula = ~poly(age_days,4), tau.formula = ~poly(age_days,4),
                               family =",gamlss_family,", data = outlierfree_poly4, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Polynomials Degree 4: ***\n"))
  opoly4_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_poly4))})
  
  model_gamlss_tr <- {paste("gamlss(value ~tr(~age_days), sigma.formula = ~age_days, nu.formula = ~age_days, tau.formula = ~age_days, 
                            family =",gamlss_family,", data = outlierfree_tr, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Decision Tree: ***\n"))
  otr_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_tr))})
  
  model_gamlss_nn <- {paste("gamlss(value ~nn(~age_days,size=4, decay=0.1), sigma.formula = ~nn(~age_days,size=3, decay=0.1), nu.formula = ~nn(~age_days,size=1, decay=0.1), tau.formula = ~nn(~age_days,size=1, decay=0.1), 
                            family =",gamlss_family,", data = outlierfree_nn, method = ",method,"(",epochs,"), control = gamlss.control(n.cyc = 100))")}
  cat(paste("*** GAMLSS - Neural Network: ***\n"))
  onn_ <<- suppressWarnings({helper_make_gamlss(noquote(model_gamlss_nn))})
}

####################################### Discrete Model ############################################

#' Make discrete model from the GAMLSS model with the zlog value for the upper and low 
#' reference intervals. When a change occurs in the 2.5% or 97.5% RI the age group is split
#' and the mean and RI from this age groups is calculated. The zlog value should be between 1.96 and -1.96.
#' 
#' @param model predicted GAMLSS model
#' @param deviation Deviation from the upper/lower Reference Intervals
split_gamlss <- function(model, max_zlog_value = 1.96){
  
  splitsgamlss <- data.frame()
  splitsgamlss_jump <- 1
  para_split <- 1
  i <- 1
  
  # Go through the data to the end and check if the Reference Intervals are changed
  while (i < nrow(model)){

    if(length(splitsgamlss) == 0){

      next.lower.zlog <- abs(zlog(mean(model$`2.5`[para_split:(i+1)]), 
                                  model$`2.5`[para_split], 
                                  model$`97.5`[para_split]))
      previous.lower.zlog <- abs(zlog(mean(model$`2.5`[para_split]),
                                      model$`2.5`[i+1], 
                                      model$`97.5`[i+1]))
      next.upper.zlog <- abs(zlog(mean(model$`97.5`[para_split:(i+1)]), 
                                  model$`2.5`[para_split], 
                                  model$`97.5`[para_split]))
      previous.higher.zlog <- abs(zlog(mean(model$`97.5`[para_split]), 
                                       model$`2.5`[i+1], 
                                       model$`97.5`[i+1]))
      
    if (next.lower.zlog > max_zlog_value ||
        previous.lower.zlog > max_zlog_value ||
        next.upper.zlog > max_zlog_value ||
        previous.higher.zlog > max_zlog_value ){

      para_split <- i # change para_split for the next age group
      splitsgamlss <- rbind(splitsgamlss, para_split)
      }
    } else{

      next.lower.zlog <- abs(zlog(mean(model$`2.5`[para_split:(i+1)]), 
                                  mean(model$`2.5`[splitsgamlss[splitsgamlss_jump, 1]:para_split]), 
                                  mean(model$`97.5`[splitsgamlss[splitsgamlss_jump, 1]:para_split])))
      previous.lower.zlog <- abs(zlog(mean(model$`2.5`[splitsgamlss[splitsgamlss_jump, 1]:para_split]), 
                                      mean(model$`2.5`[para_split:(i+1)]), 
                                      mean(model$`97.5`[para_split:(i+1)])))
      next.upper.zlog <- abs(zlog(mean(model$`97.5`[para_split:(i+1)]), 
                                  mean(model$`2.5`[splitsgamlss[splitsgamlss_jump, 1]:para_split]), 
                                  mean(model$`97.5`[splitsgamlss[splitsgamlss_jump, 1]:para_split])))
      previous.higher.zlog <- abs(zlog(mean(model$`97.5`[splitsgamlss[splitsgamlss_jump, 1]:para_split]),
                                       mean(model$`2.5`[para_split:(i+1)]), 
                                       mean(model$`97.5`[para_split:(i+1)])))
      
      if (next.lower.zlog > max_zlog_value ||
          previous.lower.zlog > max_zlog_value ||
          next.upper.zlog > max_zlog_value ||
          previous.higher.zlog > max_zlog_value ){

        para_split <- i # change para_split for the next age group
        splitsgamlss_jump = splitsgamlss_jump + 1
        splitsgamlss <- rbind(splitsgamlss, para_split)
        }
    }
    i = i+1
  }
  return(splitsgamlss)
}