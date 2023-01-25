###################################################################################################
####################################### Script for the Data analysis ##############################
###################################################################################################

####################################### Statistics ################################################

truncate.x <- function(x, qf){
  #qf = quantile factor to derive qnorm(0.025) from qnorm(0.25)
  Q <- quantile(x, c(0.25, 0.5, 0.75))
  var1 <- Q[2] - Q[1]
  var2 <- Q[3] - Q[2]
  var <- min(var1, var2)
  lim <- c(Q[2] - qf * var, Q[2] + qf * var)
  return(subset(x, x >= lim[1] & x <= lim[2]))
}


print.progress <- function(x, i, log.mode = FALSE){
  if (log.mode){x <- exp(x)}
  x <- round(x, 2)
  print(paste("cycle", i, "n =", length(x), "min =", min(x), "max =", max(x)))
} #print.progress


#' Calculate the central 95% of a normal distribution performs algorithm until length of vector does not change anymore
#' Modification of John W. Tukey’s box plot algorithm
#'
#' @param x Expects a vector x of positive numbers without NAs
#' @param perc Value for the Percentiles
#' @param log.mode When the data is lognormal distributed
#' @param print.cycles print the cycles 
#' @param plot.it Plot the modified Tukey
#' 
#' 
iBoxplot95 <- function(x, lognorm = FALSE, plot.it = TRUE){
  #sets starting parameters
  n0 <- 1
  n1 <- 0
  qf <- 2.906 #qnorm(0.025) / qnorm(0.25)
  i <- 1
  if(lognorm){x <- log(x)}
  #truncates x repeatedly until no more outliers are detected
  while (n0 > n1){
    n0 <- length(x)
    x <- truncate.x(x, qf = qf)
    n1 <- length(x)
    qf <- 3.083 #qnorm(0.025)/qnorm(0.25 * 0.95 + 0.025) = qtruncnorm(0.025)/qtruncnorm(0.25)
  }
  if (lognorm){x <- exp(x)}
  
  if (plot.it){
    d <- density(x)
    hist(x, freq = FALSE, ylim = c(0, max(d$y) * 1.25), main = "Truncated distribution", xlab = "x", ylab = "Density", 
         col = "lightgrey")
    lines(d$x, d$y)
    legend("topright", bty = "n", legend = paste("Range:", min(x),"-", max(x)))
  }
  return(x)
}

#' Expects a numeric vector x without NAs (usually central 95% of normal distribution)
#' Returns intercept and slope of a robust QQ-line plus calculated lower and upper limits of reference interval
#'
#' @param x represents a central percentage of a normal distribution defined by perc standard is central 
#' 95% (perc means: from 2.5th to 97.5th percentile)
#' @param perc Value for the Percentiles
#' @param log.mode When the data is lognormal distributed
#' @param print.cycles print the cycles 
#' @param plot.it Plot the QQ-Plot
#' 
reflim <- function(x, log.mode = NULL, n.quantiles = 100, n.min = 200, plot.it = TRUE,
                   main = "Q-Q plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles"){
  
  xx <- na.omit(x)
  if(!is.numeric(xx)){stop("(reflim) x must be numeric.")}
  if(min(xx) < 0){stop("(reflim) only positive values allowed.")}
  if(length(xx) < n.min){return(rep(NA, 4))}
  digits <- 2 - floor(log10(median(xx)))
  if(digits < 1){digits <- 1}
  
  if(is.null(log.mode)){
    log.mode <- def.distribution(xx)$lognormal
  }
  if(log.mode){xx <- log(xx)}
  
  # truncates xx
  ## Note that log.mode must be FALSE here
  x.trunc <- iBoxplot(xx, log.mode = F, n.min = n.quantiles, print.cycles = F)$trunc
  
  p1 <- seq(from = 0.025, to = 0.975, length.out = n.quantiles)   #x.axis
  p2 <- seq(from = 0, to = 1, length.out = n.quantiles)           #y.axis
  x.ax <- qnorm(p1)             #quantiles of standard normal distribution
  y.ax <-quantile(x.trunc, p2)  #quantiles of sample distribution
  
  #calulates linear regression coefficients from the central 50% of the curve
  central.part <- floor((0.25 * n.quantiles) : ceiling(0.75 * n.quantiles))
  reg <- lm(y.ax[central.part] ~ x.ax[central.part])
  a <- reg$coefficients[2]
  b <- reg$coefficients[1]
  result <- c(b, a, b - 1.96 * a, b + 1.96 * a)
  result <- setNames(result, c("mean", "sd", "lower.lim", "upper.lim"))
  if(log.mode){
    names(result)[1 : 2] <- paste0(names(result)[1 : 2], "log")
    result[1 : 2] <- round(result[1 : 2], 3)
    result[3 : 4] <- round(exp(result[3 : 4]), digits)
  } else {
    result <- round(result, digits)
  }
  if(result[3] < 0){result[3] <- 0}
  
  # draws Q-Q plot
  if (plot.it){
    if(!log.mode){
      ll <- result[3]
      ul <- result[4]
      diff <- ul - ll
      plot(y.ax ~ x.ax, xlim = c(-3, 3),
           ylim = c(ll - 0.2 * diff, ul + 0.2 * diff),
           main = main, xlab = xlab, ylab = ylab)
    }else{
      ll <- log(result[3])
      ul <- log(result[4])
      diff <- ul - ll
      plot(y.ax ~ x.ax, yaxt = "n", xlim = c(-3, 3),
           ylim = c(ll - 0.2 * diff, ul + 0.2 * diff),
           main = main, xlab = xlab, ylab = ylab)
      y.pos <- quantile(y.ax, c(0.01, 0.2, 0.5, 0.8, 0.99))
      axis(2, at = y.pos,labels  = round(exp(y.pos), digits - 1))
    }
    grid()
    abline(v = 0)
    abline(v = c(-1.96, 1.96), lty = 2)
    abline(h = c(ll, ul),
           col = "green", lwd = 2)
    abline(reg$coefficients, lwd = 2, col = "blue")
    legend("topleft",
           paste(formatC(result[3], digits - 1, format = "f"), "-",
                 formatC(result[4], digits - 1, format = "f")),
           bty = "n", cex = 1.5)
  }
  
  return(result)
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
bowley <- function(x, alpha = 0.25){
  q <- quantile(x, c(alpha, 0.5, 1 - alpha))
  return ((q[1] - 2 * q[2] + q[3]) / (q[3] - q[1]))
}


#' Compares the Bowley skewness for x and log(x)
#' returns TRUE, if a lognormal distribution should be assumed
#' 
#' @param x Expects a numeric vector x > 0
#' @param cutoff threshold
#' @param alpha threshold
#' @param plot.it Plot the result
#' 
def.distribution <- function(x, cutoff = 0.05, alpha = 0.25, digits = 3, plot.it = TRUE, add.tx = "distribution", setvalues = NULL,
                             main = ""){
  
  xx <- x[!is.na(x)]
  
  if(!is.numeric(xx)){stop("(def.distribution) x must be numeric.")}
  if(length(xx) < 2){stop("(def.distribution) x must be a vector of at least 2 numeric values.")}
  if(min(xx) <= 0){stop("(def.distribution) negative values not allowed.")}
  
  s <- rep(NA, 2)
  s[1] <- bowley(xx, alpha)
  s[2] <- bowley(log(xx), alpha)
  if(s[1] < 0){lognorm <- FALSE} else {lognorm <- (s[1] - s[2]) >= cutoff}
  names(s) <- c("normal", "lognormal")
  if(!is.na(digits)){s <- round(s, digits)}
  
  if (plot.it){
    d <- density(x)
    hist(x, freq = FALSE, ylim = c(0, max(d$y) * 1.2), xlab = "x", ylab = "Density", 
         main = paste(ifelse(lognorm, paste("Lognormal-", add.tx), paste("Normal-", add.tx)), main))
    lines(d$x, d$y)
    if (!is.null(setvalues)){
      abline(v = setvalues, lty = 2)}
  }
  
  return(list("lognormal" = lognorm, "BowleySkewness" = s))
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

adjust.digits <- function(x){
  digits <- 2 - floor(log10(x))
  if(digits < 1){digits <- 1}
  return(digits)
}