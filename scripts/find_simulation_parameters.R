library(dplyr)

# load study parameters
study_params <- read.csv2(snakemake@input[[1]])

if (snakemake@wildcards[[1]] == "incubation_time") {
  # calculate MSE of model parameters mu and sigma
  for (i in 1:nrow(study_params)) {
  study_params$MSE[i] <- mean((study_params$modelparam1[i] - study_params$modelparam1)^2 + (study_params$modelparam2[i] - study_params$modelparam2)^2)
  }

  # find default values for mean and median as argmin from MSE
  default_param1 <- study_params %>% filter(MSE == min(study_params$MSE)) %>% select(param1) %>% as.numeric()
  default_param2 <- study_params %>% filter(MSE == min(study_params$MSE)) %>% select(param2) %>% as.numeric()
} else if (snakemake@wildcards[[1]] == "serial_interval"){
  # define cluster
  study_params_cluster <- filter(study_params, modelparam1 < 4 & modelparam2 < 0.8)

  # calculate MSE of model parameters alpha and beta
  for (i in 1:nrow(study_params_cluster)) {
  study_params_cluster$MSE[i] <- mean((study_params_cluster$modelparam1[i] - study_params_cluster$modelparam1)^2 + (study_params_cluster$modelparam2[i] - study_params_cluster$modelparam2)^2)
  }

  # find default values for mean and standard deviation as argmin from MSE
  default_param1 <- study_params_cluster %>% filter(MSE == min(study_params_cluster$MSE)) %>% select(param1) %>% as.numeric()
  default_param2 <- study_params_cluster %>% filter(MSE == min(study_params_cluster$MSE)) %>% select(param2) %>% as.numeric()
}

# define simulation range by min and max (for mean and median or standard deviation)
simulation_params <- data.frame("default" = c(default_param1, default_param2),
                                "min" = c(min(study_params$param1), min(study_params$param2)),
                                "max" = c(max(study_params$param1), max(study_params$param2)))

# save simulation parameters at respective location
write.csv2(simulation_params, snakemake@output[[1]])
