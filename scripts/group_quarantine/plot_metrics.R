library(ggplot2)
library(dplyr)
metric_results <- read.csv2(snakemake@input[[1]]) %>%
  filter(scenario == snakemake@wildcards[["scenario"]])

if (snakemake@wildcards[["metric"]] == "deltaprob") {
  if (snakemake@wildcards[["scenario"]] == "sensitivity"){
    p <- ggplot(metric_results, aes(x=offset, y=deltaprob)) +
        geom_line() +
        labs(x = "test sensitivity off-set", y = expression(paste(Delta,"prob")),
             caption = "Difference of probability resulting from a shift of the PCR-test sensitivity.") +
        theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),axis.title=element_text(size=12))

    ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")
  } else {
    p <- ggplot(data.frame("x" = metric_results$parameter1, "y" = metric_results$parameter2, "z" = metric_results$deltaprob), aes(x,y)) +
          geom_tile(aes(fill = z, height = sort(unique(y))[2] - sort(unique(y))[1], width = sort(unique(x))[2] - sort(unique(x))[1])) +
          scale_fill_gradient2(low="lightblue", high="orange", guide = guide_colorbar(order=1)) +
          geom_contour(aes(z = metric_results$deltaprob), binwidth = 0.05, color = "gray") +
          geom_point(aes(x = metric_results$parameter1[1], y = metric_results$parameter2[1]), shape = 4, color = "red", stroke = 1.2)+
          labs(x = "Parameter 1: E(X)", y = expression("Parameter 2:"~mu^{"*"}), fill = expression(paste(Delta,"prob")), shape = "Time of \ndata collection",
               caption = paste("Difference of probability in the", snakemake@wildcards[["scenario"]], "scenario. The reference parameters are marked in red.")) +
          theme(plot.caption = element_text(hjust = 0), plot.caption.position = "plot", axis.text=element_text(size=11),
                axis.title=element_text(size=12))

    ggsave(snakemake@output[[1]], width = 5.5, height = 3.8, units = "in")
  }
}
