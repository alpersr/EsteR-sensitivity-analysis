library(dplyr)

# load simulation results
simulation_results <- readRDS(snakemake@input[[1]])

childcare_metric_results <- bind_rows(simulation_results[["childcare"]]) %>%
  mutate("offset" = NA, .before = "probability") %>%
  mutate("deltaprob" = probability-probability[1], "scenario" = "childcare")
school_metric_results <- bind_rows(simulation_results[["school"]]) %>%
  mutate("offset" = NA, .before = "probability") %>%
  mutate("deltaprob" = probability-probability[1], "scenario" = "school")
sensitivity_metric_results <- bind_rows(simulation_results[["sensitivity"]]) %>%
  mutate("parameter1" = NA, "parameter2" = NA, .before = "offset") %>%
  mutate("deltaprob" = probability-probability[1], "scenario" = "sensitivity")

metric_results <- rbind(childcare_metric_results, school_metric_results, sensitivity_metric_results)
write.csv2(metric_results, snakemake@output[[1]])