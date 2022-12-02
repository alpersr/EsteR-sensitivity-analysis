library(ggplot2)
library(dplyr)
metric_results <- read.csv2(snakemake@input[[1]])
study_params <- read.csv2(snakemake@input[[2]])
defaults <- study_params[25,]
study_params <- study_params[-25,]
gen <- ifelse(snakemake@wildcards[["scenario"]] == "generation1", "first",
               ifelse(snakemake@wildcards[["scenario"]] == "generation2", "second", "third"))

if (snakemake@wildcards[["metric"]] == "iou") {
  plot_iou <- ggplot(data.frame("x" = metric_results$parameter1, "y" = metric_results$parameter2,
                                "z" = metric_results[,paste0("iou_", snakemake@wildcards[["scenario"]])]), aes(x,y)) +
              geom_tile(aes(fill = z, height = sort(unique(y))[2] - sort(unique(y))[1], width = sort(unique(x))[2] - sort(unique(x))[1])) +
              scale_fill_gradient2(low="lightblue", high="orange", guide = guide_colorbar(order=1)) +
              geom_contour(aes(z = metric_results[,paste0("iou_", snakemake@wildcards[["scenario"]])]), binwidth = 0.2, color = "gray") +
              geom_point(data = study_params, aes(x = param1, y = param2, group = data_collection_end, shape = data_collection_end), size = 2)+
              geom_point(data = filter(study_params, data_collection_end == "20_2"), aes(x=param1, y=param2), shape = 0, size = 2) + # all studies that ended in 20_2 started in 20_1
              geom_point(aes(x = defaults$param1, y = defaults$param2), shape = 0, color = "red", stroke = 1.2)+
              scale_shape_manual(labels = c("12/19-06/20", "07/20-12/20", "07/21-12/21"), values=c(0,4,5))+
              labs(x = "Parameter 1: E(X)", y = "Parameter 2: SD(X)", fill = "1-IoU", shape = "Time of \ndata collection",
                   caption = paste("1-IoU of the 80% HDR for the symptom begin of the", gen, "contact generation. \nThe reference parameters are marked in red.")) +
              theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),
                    axis.title=element_text(size=12))

  ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")

} else if (snakemake@wildcards[["metric"]] == "w1") {
  plot_w1 <- ggplot(data.frame("x" = metric_results$parameter1, "y" = metric_results$parameter2,
                                "z" = metric_results[,paste0("w1_", snakemake@wildcards[["scenario"]])]), aes(x,y)) +
              geom_tile(aes(fill = z, height = sort(unique(y))[2] - sort(unique(y))[1], width = sort(unique(x))[2] - sort(unique(x))[1])) +
              scale_fill_gradient2(low="lightblue", high="orange", guide = guide_colorbar(order=1)) +
              geom_contour(aes(z = metric_results[,paste0("w1_", snakemake@wildcards[["scenario"]])]), binwidth = 1, color = "gray") +
              geom_point(data = study_params, aes(x = param1, y = param2, group = data_collection_end, shape = data_collection_end), size = 2)+
              geom_point(data = filter(study_params, data_collection_end == "20_2"), aes(x=param1, y=param2), shape = 0, size = 2) + # all studies that ended in 20_2 started in 20_1
              geom_point(aes(x = defaults$param1, y = defaults$param2), shape = 0, color = "red", stroke = 1.2)+
              scale_shape_manual(labels = c("12/19-06/20", "07/20-12/20", "07/21-12/21"), values=c(0,4,5))+
              labs(x = "Parameter 1: E(X)", y = "Parameter 2: SD(X)", fill = expression(W[1]), shape = "Time of \ndata collection",
                   caption = paste("Wasserstein metric for the distribution of the symptom begin of the", gen, "contact generation. \nThe reference parameters are marked in red.")) +
              theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),
                    axis.title=element_text(size=12))

  ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")
}