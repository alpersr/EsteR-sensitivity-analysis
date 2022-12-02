library(dplyr)

# source help functions
snakemake@source("help_functions.R")

# load simulation results
simulation_results <- readRDS(snakemake@input[[1]])

# calculate metrics
metric_results <- data.frame()
metric_iou <- c()
metric_w1 <- c()
for (i in 1:length(simulation_results)) {
  if (!is.logical(simulation_results[[i]]$distribution)){
    metric_results <- rbind(metric_results,
                            data.frame("parameter1" = simulation_results[[i]]$parameter1,
                                       "parameter2" = simulation_results[[i]]$parameter2,
                                       "hdr80_left" = simulation_results[[i]]$distribution$dates[1] + simulation_results[[i]]$hdr80$qstart*60*60*24,
                                       "hdr80_right" = simulation_results[[i]]$distribution$dates[1] + simulation_results[[i]]$hdr80$qend*60*60*24))
    metric_iou <- c(metric_iou, calculate_iou(as.numeric(metric_results$hdr80_left[1]), as.numeric(metric_results$hdr80_right[1]),
                                  as.numeric(metric_results$hdr80_left[nrow(metric_results)]), as.numeric(metric_results$hdr80_right[nrow(metric_results)])))
    metric_w1 <- c(metric_w1, calculate_w1(simulation_results[[1]]$distribution, simulation_results[[i]]$distribution))
  }
}

metric_results <- mutate(metric_results, "iou" = metric_iou, "w1" = metric_w1)

write.csv2(metric_results, snakemake@output[[1]])