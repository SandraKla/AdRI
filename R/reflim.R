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

#' Expects a numeric vector x without NAs (usually central 95% of normal distribution)
#' Returns intercept and slope of a robust QQ-line plus calculated lower and upper limits of reference interval
#'
#' @param x represents a central percentage of a normal distribution defined by perc standard is central 
#' 95% (perc means: from 2.5th to 97.5th percentile)
#' @param perc Value for the Percentiles
#' @param log.mode When the data is lognormal distributed
#' @param print.cycles print the cycles 
#' @param plot.it Plot the QQ-Plot
modified.qqplot <- function(x, perc = 2.5, n.dots = NULL, log.mode = FALSE, plot.it = TRUE, 
                            main = "Q-Q plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles"){

  
  #digits <- get.decimals(median(x))
  if (log.mode){x <- log(x)}
  digits <- 3
  #calculates quantiles for Q-Q plot
  if (is.null(n.dots)){n.dots = length(x)}
  
  p1 <- seq(from = perc / 100, to = 1 - perc / 100, length.out = n.dots)    #x.axis
  p2 <- seq(from = 0, to = 1, length.out = n.dots)                          #y.axis
  x.ax <- qnorm(p1)        #quantiles of standard normal distribution
  y.ax <-quantile(x, p2)   #quantiles of sample distribution
  
  #calulates linear regression coefficients from the central 50% of the curve
  central.part <- floor((0.25 * n.dots) : ceiling(0.75 * n.dots))
  coeff <- lm(y.ax[central.part] ~ x.ax[central.part])  
  result <- data.frame(rep(0, 4))
  colnames(result) <- "results"
  rownames(result) <- c("mean", "sd", "lower limit", "upperlimit")
  if(log.mode){rownames(result)[1 : 2] <- paste0(rownames(result)[1 : 2],"log")}
  result[1 : 2, 1] <- coeff$coefficients[1 : 2]
  result[3, 1] <- result[1, 1] - 1.96 * result[2, 1]
  result[4, 1] <- result[1, 1] + 1.96 * result[2, 1]
  if(log.mode){
    result[1 : 2, 1] <- round(result[1 : 2, 1], 3)
    result[3 : 4, 1] <- exp(result[3 : 4, 1])
  }
  result[3 : 4, 1] <- round(result[3 : 4, 1], digits)
  if(result[3, 1] < 0){result[3, 1] <- 0}
  
  if (plot.it){
    if(!log.mode){
      plot(y.ax ~ x.ax, xlim = c(-3, 3), ylim = c(0.8 * min(y.ax), 1.2 * max(y.ax)), 
           main = main, xlab = xlab, ylab = ylab)
    }else{
      plot(y.ax ~ x.ax, yaxt = "n", xlim = c(-3, 3), ylim = c(0.8 * min(y.ax), 1.2 * max(y.ax)), 
           main = main, xlab = xlab, ylab = ylab)
      y.pos <- quantile(y.ax, c(0.01, 0.2, 0.5, 0.8, 0.99))
      if (digits > 0){digits <- digits - 1}
      axis(2, at = y.pos,labels  = round(exp(y.pos), digits))
    }
    grid()
    abline(v = 0)
    abline(v = c(-1.96, 1.96), lty = 2)
    abline(h = c(result[1, 1] - 1.96 * result[2, 1], result[1, 1] + 1.96 * result[2, 1]), 
           col = "green", lwd = 2)
    abline(coeff, lwd = 2, col = "blue")
    legend("topright", paste(result[3, 1], "-", result[4, 1]))
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