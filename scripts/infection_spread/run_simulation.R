# load parameters
simulation_params <- read.csv2(snakemake@input[[1]])

# model parameters to test in simulation
mean <- c(simulation_params$default[1], # from study used in the EsteR toolkit
          seq(simulation_params$min[1],simulation_params$max[1], length.out = 100)) # grid from all other studies
median <- c(simulation_params$default[2], # from study used in the EsteR toolkit
          seq(simulation_params$min[2],simulation_params$max[2], length.out = 100)) # grid from all other studies

# scenario: 20 people met on March 22nd, 2022 and 3 develop symptoms until March 26th, 2022
event_date = as.Date("2022-03-22")
groupsize = 20
observed_infected = 3
last_observed_date = as.Date("2022-03-26")

results <- list()
for (i in 1:length(mean)) {
  for(j in 1:length(median)) {
    if (mean[i] <= median[j]) { # in this case no lognormal distribution can be derived
      results[[(i-1)*101+j]] <- data.frame("parameter1" = mean[i],
                           "parameter2" = median[j],
                           "prediction" = NA)
    } else {
      ## lognormal parameters
      mu <- log(median[j])
      sigma <- sqrt(2*(log(mean[i])-log(median[j])))

      ## expected total infections from observation
      days_after_event <- as.numeric(last_observed_date - event_date)
      proportion <- plnorm(days_after_event, mu, sigma)
      prediction <- min(ceiling(observed_infected / proportion), groupsize)

      results[[(i-1)*101+j]] <- data.frame("parameter1" = mean[i],
                           "parameter2" = median[j],
                           "prediction" = prediction)
    }
  }
}

saveRDS(results, snakemake@output[[1]])