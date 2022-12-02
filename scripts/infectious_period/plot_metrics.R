library(ggplot2)
library(dplyr)
metric_results <- read.csv2(snakemake@input[[1]])

if (snakemake@wildcards[["metric"]] == "iou") {
  plot_iou <- ggplot(data.frame("x" = metric_results$parameter1, "y" = metric_results$parameter2,
                                "z" = metric_results$iou), aes(x,y)) +
              geom_tile(aes(fill = z, height = sort(unique(y))[2] - sort(unique(y))[1], width = sort(unique(x))[2] - sort(unique(x))[1])) +
              scale_fill_gradient2(low="lightblue", high="orange", guide = guide_colorbar(order=1)) +
              geom_contour(aes(z = metric_results$iou), binwidth = 0.2, color = "gray") +
              geom_point(aes(x = metric_results$parameter1[1], y = metric_results$parameter2[1]), shape = 0, color = "red", stroke = 1.2)+
              labs(x = "Parameter 1: E(X)", y = "Parameter 2: SD(X)", fill = "1-IoU", shape = "Time of \ndata collection",
                   caption = paste("1-IoU of the 80% HDR for the infectious period. The reference parameters are marked in red.")) +
              theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),
                    axis.title=element_text(size=12))

  ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")

} else if (snakemake@wildcards[["metric"]] == "w1") {
  plot_w1 <- ggplot(data.frame("x" = metric_results$parameter1, "y" = metric_results$parameter2,
                                "z" = metric_results$w1), aes(x,y)) +
              geom_tile(aes(fill = z, height = sort(unique(y))[2] - sort(unique(y))[1], width = sort(unique(x))[2] - sort(unique(x))[1])) +
              scale_fill_gradient2(low="lightblue", high="orange", guide = guide_colorbar(order=1)) +
              geom_contour(aes(z = metric_results$w1), binwidth = 1, color = "gray") +
              geom_point(aes(x = metric_results$parameter1[1], y = metric_results$parameter2[1]), shape = 0, color = "red", stroke = 1.2)+
              labs(x = "Parameter 1: E(X)", y = "Parameter 2: SD(X)", fill = expression(W[1]), shape = "Time of \ndata collection",
                   caption = paste("Wasserstein metric for the infectious period distribution. The reference parameters are marked in red.")) +
              theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),
                    axis.title=element_text(size=12))

  ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")
}