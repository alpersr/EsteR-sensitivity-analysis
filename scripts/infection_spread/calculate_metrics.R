library(dplyr)

# load simulation results
simulation_results <- readRDS(snakemake@input[[1]])

metric_results <- bind_rows(simulation_results) %>%
  mutate("deltapred" = prediction-prediction[1]) %>%
  na.omit()

write.csv2(metric_results, snakemake@output[[1]])