library(dplyr)

# source help functions
snakemake@source("help_functions.R")

# load simulation results
simulation_results <- readRDS(snakemake@input[[1]])

# calculate metrics
metric_results <- data.frame()
metric_iou_1 <- c()
metric_w1_1 <- c()
metric_iou_2 <- c()
metric_w1_2 <- c()
metric_iou_3 <- c()
metric_w1_3 <- c()
for (i in 1:length(simulation_results)) {
  if (!is.logical(simulation_results[[i]]$distribution1)){
    metric_results <- rbind(metric_results,
                            data.frame("parameter1" = simulation_results[[i]]$parameter1,
                                       "parameter2" = simulation_results[[i]]$parameter2,
                                       "hdr80_1_left" = simulation_results[[i]]$distribution1$dates[1] + simulation_results[[i]]$hdr80_1$qstart*60*60*24,
                                       "hdr80_1_right" = simulation_results[[i]]$distribution1$dates[1] + simulation_results[[i]]$hdr80_1$qend*60*60*24,
                                       "hdr80_2_left" = simulation_results[[i]]$distribution2$dates[1] + simulation_results[[i]]$hdr80_2$qstart*60*60*24,
                                       "hdr80_2_right" = simulation_results[[i]]$distribution2$dates[1] + simulation_results[[i]]$hdr80_2$qend*60*60*24,
                                       "hdr80_3_left" = simulation_results[[i]]$distribution3$dates[1] + simulation_results[[i]]$hdr80_3$qstart*60*60*24,
                                       "hdr80_3_right" = simulation_results[[i]]$distribution3$dates[1] + simulation_results[[i]]$hdr80_3$qend*60*60*24))
    metric_iou_1 <- c(metric_iou_1, calculate_iou(as.numeric(metric_results$hdr80_1_left[1]), as.numeric(metric_results$hdr80_1_right[1]),
                                  as.numeric(metric_results$hdr80_1_left[nrow(metric_results)]), as.numeric(metric_results$hdr80_1_right[nrow(metric_results)])))
    metric_w1_1 <- c(metric_w1_1, calculate_w1(simulation_results[[1]]$distribution1, simulation_results[[i]]$distribution1))
    metric_iou_2 <- c(metric_iou_2, calculate_iou(as.numeric(metric_results$hdr80_2_left[1]), as.numeric(metric_results$hdr80_2_right[1]),
                                  as.numeric(metric_results$hdr80_2_left[nrow(metric_results)]), as.numeric(metric_results$hdr80_2_right[nrow(metric_results)])))
    metric_w1_2 <- c(metric_w1_2, calculate_w1(simulation_results[[1]]$distribution2, simulation_results[[i]]$distribution2))
    metric_iou_3 <- c(metric_iou_3, calculate_iou(as.numeric(metric_results$hdr80_3_left[1]), as.numeric(metric_results$hdr80_3_right[1]),
                                  as.numeric(metric_results$hdr80_3_left[nrow(metric_results)]), as.numeric(metric_results$hdr80_3_right[nrow(metric_results)])))
    metric_w1_3 <- c(metric_w1_3, calculate_w1(simulation_results[[1]]$distribution3, simulation_results[[i]]$distribution3))
  }
}

metric_results <- mutate(metric_results, "iou_generation1" = metric_iou_1, "w1_generation1" = metric_w1_1,
                         "iou_generation2" = metric_iou_2, "w1_generation2" = metric_w1_2,
                         "iou_generation3" = metric_iou_3, "w1_generation3" = metric_w1_3)

write.csv2(metric_results, snakemake@output[[1]])
