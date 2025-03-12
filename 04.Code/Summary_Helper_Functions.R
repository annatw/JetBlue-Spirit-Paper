star_get <- function(p){
  if(p > 0.1){
    return("")
  } else if(p > 0.05){
    return("*")
  } else if(p > 0.01){
    return("**")
  } else{
    return("***")
  }
}

starmake <- function(coeff, se){
  q10 <- qnorm(p = 0.95, mean = 0, sd = 1)
  q05 <- qnorm(p = 0.975, mean = 0, sd = 1)
  q01 <- qnorm(p = 0.995, mean = 0, sd = 1)
  
  if(is.nan(se)){
    return(signif(coeff, digits = 3))
  } else if(coeff < 0){
    if(coeff + q01 * se < 0){
      return(paste(signif(coeff, digits = 3), "***", sep = ""))
    } else if(coeff + q05 < 0){
      return(paste(signif(coeff, digits = 3), "**", sep = ""))
    } else if(coeff + q01 < 0){
      return(paste(signif(coeff, digits = 3), "*", sep = ""))
    } else {
      return(signif(coeff, digits = 3))
    }
  } else if (coeff > 0){
    if(coeff - q01 * se > 0){
      return(paste(signif(coeff, digits = 3), "***", sep = ""))
    } else if(coeff - q05 > 0){
      return(paste(signif(coeff, digits = 3), "**", sep = ""))
    } else if(coeff - q01 > 0){
      return(paste(signif(coeff, digits = 3), "*", sep = ""))
    } else {
      return(signif(coeff, digits = 3))
    }
  } else if (coeff == 0){
    return(coeff)
  }
}


compare_row_make <- function(name, vec1, vec2){
  new_row <- c(name, round(mean(vec1, na.rm = TRUE), digits = 2),
               paste("(", round(sd(vec1, na.rm = TRUE), digits = 2), ")", sep = ""),
               round(mean(vec2, na.rm = TRUE), digits = 2),
               paste("(", round(sd(vec2, na.rm = TRUE), digits = 2), ")", sep = ""),
               paste(round(as.numeric(t.test(vec1, vec2)$statistic), digits = 2),
                     star_get(test$p.value), sep = ""))
  
  return(new_row)
}

five_statistic_row_make <- function(name, vector,
                                    weight_variable = NULL){
  if(is.null(weight_variable)){
    new_row <- c(name, round(mean(vector, na.rm = TRUE), digits = 2),
                 paste("(", round(sd(vector, na.rm = TRUE), digits = 2), ")", sep = ""),
                 round(min(vector, na.rm = TRUE), digits = 2),
                 round(median(vector,na.rm = TRUE), digits = 2),
                 round(max(vector, na.rm = TRUE), digits = 2))
  } else {
    new_row <- c(name, round(sum(vector*weight_variable)/sum(weight_variable), digits = 2),
                 paste("(", round(sd(vector, na.rm = TRUE), digits = 2), ")", sep = ""),
                 round(min(vector, na.rm = TRUE), digits = 2),
                 round(median(vector,na.rm = TRUE), digits = 2),
                 round(max(vector, na.rm = TRUE), digits = 2))
  }
  return(new_row)
}

six_statistic_row_make <- function(name, vector,
                                    weight_variable = NULL){
  if(is.null(weight_variable)){
    new_row <- c(name, length(vector), round(mean(vector, na.rm = TRUE), digits = 2),
                 paste("(", round(sd(vector, na.rm = TRUE), digits = 2), ")", sep = ""),
                 round(min(vector, na.rm = TRUE), digits = 2),
                 round(median(vector,na.rm = TRUE), digits = 2),
                 round(max(vector, na.rm = TRUE), digits = 2))
  } else {
    new_row <- c(name, length(vector), round(sum(vector*weight_variable)/sum(weight_variable), digits = 2),
                 paste("(", round(sd(vector, na.rm = TRUE), digits = 2), ")", sep = ""),
                 round(min(vector, na.rm = TRUE), digits = 2),
                 round(median(vector,na.rm = TRUE), digits = 2),
                 round(max(vector, na.rm = TRUE), digits = 2))
  }
  return(new_row)
}