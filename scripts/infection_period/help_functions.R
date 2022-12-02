library(hdrcde)
## functions for the simulation
get_misc_infection_density <- function(symptom_dates, number_of_persons,
                                       dist_type, param1, param2){
  df <- get_infection_density(symptom_dates[1], dist_type, param1, param2)
  
  if (length(symptom_dates) > 1)
  {
    for (i in 2:length(symptom_dates))
    {
      df_cur <- get_infection_density(symptom_dates[i], dist_type, param1, param2)
      names(df_cur)[names(df_cur) == "distribution"] <- paste0("distribution_", i)
      df <- merge(x = df, y = df_cur, by = "dates", all = TRUE)
    }
  }
  
  df[is.na(df)] <- 0
  
  df_misc <- data.frame("dates" = df$dates, "distribution" = df[,2]*number_of_persons[1]/sum(number_of_persons) )
  
  if (length(symptom_dates) > 1)
  {
    for (i in 2:length(symptom_dates))
    {
      df_misc$distribution <- df_misc$distribution + df[,i + 1]*number_of_persons[i]/sum(number_of_persons)
    }
  }
  
  return(df_misc)
}

get_infection_density <- function(symptom_begin_date, dist_type, param1, param2){
  infection_period_start <- symptom_begin_date - 17
  dates <- seq(as.POSIXct(infection_period_start, tz = "CET"),
               by = "hour",
               length.out = 17 * 24)
  distr <- rev(do.call(dist_type, c(list(seq(0, 17 - (1 / 24), by = 1 / 24)), param1, param2)))
  return(data.frame("dates" = dates, "distribution" = distr))
}

calculate_qstart_qend <- function(probability, df) {
  set.seed(123) # because hdr random simulation
  hdr_df <- hdr(den = data.frame(x = 1:length(df$distribution), y = df$distribution), prob = probability*100)$hdr
  qstart <- (hdr_df[1:(length(hdr_df)/2)*2] - 1)/24
  qend  <- (hdr_df[1:(length(hdr_df)/2)*2 - 1] - 1)/24
  return(list("qstart" = qstart, "qend" = qend))
}

## function to calculate metrics
# 1-IoU for 80% HDR
calculate_iou <- function(hdileft_old, hdiright_old, hdileft_new, hdiright_new) {
  # union of the two sets is empty
  if (hdileft_new >= hdiright_old || hdileft_old >= hdiright_new) {
    intersection <- 0
    # union of the two sets is not empty
  } else {
    intersection <- min(hdiright_new, hdiright_old) - max(hdileft_new, hdileft_old)
  }
  # union <- (hdiright_new - hdileft_new) - intersection + (hdiright_old - hdileft_old)
  union <- max(hdiright_old, hdiright_new) - min(hdileft_old, hdileft_new)
  return(1 - intersection/union)
}
# Wasserstein metric for distributions
calculate_w1 <- function(distribution_old, distribution_new) {
  df_merged <- merge(distribution_old, distribution_new, by = "dates", all = TRUE)
  diff <- vector("numeric")
  for (i in 1:nrow(df_merged)){
    diff[i] <- abs(sum(df_merged$distribution.x[1:i])/24-sum(df_merged$distribution.y[1:i])/24) # /24 because we have hours and want the cdfs in days
  }
  return(sum(diff)/24) # /24 because we have differences for each hour and want the result in days
}

