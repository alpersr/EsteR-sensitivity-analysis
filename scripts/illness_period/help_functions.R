library(hdrcde)


## functions for the simulation
get_serial_interval_density <- function(symptom_begin_date, max_days, dist_type, param1, param2){
  symptom_begin_date_posixct <- as.POSIXct(symptom_begin_date, tz = "CET")
  dates <- seq(symptom_begin_date_posixct,
               by = "hour",
               length.out = max_days * 24)
  distr <- do.call(dist_type, c(list(seq(0, max_days - (1 / 24), by = 1 / 24)), param1, param2))

  return(data.frame("dates" = dates, "distribution" = distr))
}


calculate_qstart_qend <- function(probability, df) {
  set.seed(123) # because hdr random simulation
  hdr_df <- hdr(den = data.frame(x = 1:length(df$distribution), y = df$distribution), prob = probability*100)$hdr
  qstart <- (hdr_df[1,1] - 1)/24
  qend   <- (hdr_df[1,2] - 1)/24
  return(list("qstart" = qstart, "qend" = qend))
}


## functions to calculate metrics
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