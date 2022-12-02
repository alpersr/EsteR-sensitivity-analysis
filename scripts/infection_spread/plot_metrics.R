library(ggplot2)
metric_results <- read.csv2(snakemake@input[[1]])
study_params <- read.csv2(snakemake@input[[2]])
defaults <- study_params[25,]
study_params <- rbind(study_params[-25,], study_params[5,])
study_params$data_collection_start[28] <- "20_2"

if (snakemake@wildcards[["metric"]] == "deltapred") {
  plot_iou <- ggplot(data.frame("x" = metric_results$parameter1, "y" = metric_results$parameter2, "z" =metric_results$deltapred), aes(x,y)) +
              geom_tile(aes(fill = z, height = sort(unique(y))[2] - sort(unique(y))[1], width = sort(unique(x))[2] - sort(unique(x))[1])) +
              scale_fill_gradient2(low="lightblue", high="orange", guide = guide_colorbar(order=1)) +
              geom_point(data = study_params, aes(x = param1, y = param2, group = data_collection_end, shape = data_collection_start), size = 2)+
              geom_point(aes(x = study_params$param1[5], y = study_params$param2[5]), shape = 2, size = 2)+
              geom_point(aes(x = defaults$param1, y = defaults$param2), shape = 0, color = "red", stroke = 1.2)+
              scale_shape_manual(labels = c("12/19-06/20", "07/20-12/20", "01/21-06/21"), values=c(0,4,2))+
              labs(x = "Parameter 1: E(X)", y = expression("Parameter 2:"~mu^{"*"}), fill = expression(paste(Delta,"pred")), shape = "Time of \ndata collection",
                   caption = "Difference of predicted total infections after a group event. The reference parameters are marked in red.") +
              theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),
                    axis.title=element_text(size=12))

  ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")
}
