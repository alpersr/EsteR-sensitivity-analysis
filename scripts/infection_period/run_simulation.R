# source help functions
snakemake@source("help_functions.R")

# load parameters
simulation_params <- read.csv2(snakemake@input[[1]])

# model parameters to test in simulation
mean <- c(simulation_params$default[1], # from study used in the EsteR toolkit
          seq(simulation_params$min[1],simulation_params$max[1], length.out = 100)) # grid from all other studies
median <- c(simulation_params$default[2], # from study used in the EsteR toolkit
          seq(simulation_params$min[2],simulation_params$max[2], length.out = 100)) # grid from all other studies


# scenario: 1 person starts to show symptoms on March 22nd, 2022
symptom_dates <- as.Date("2022-03-22")
number_of_persons <- 1


results <- list()
for (i in 1:length(mean)) {
  for(j in 1:length(median)) {
    if (mean[i] <= median[j]) { # in this case no lognormal distribution can be derived
      results[[(i-1)*101+j]] <- list("parameter1" = mean[i],
                           "parameter2" = median[j],
                           "distribution" = NA,
                           "hdr80" = NA)
    } else {
      ## lognormal parameters
      mu <- log(median[j])
      sigma <- sqrt(2*(log(mean[i])-log(median[j])))

      ## distribution
      df <- get_misc_infection_density(symptom_dates, number_of_persons, dlnorm,
                                       mu, sigma)

      ## 80% HDR
      hdr80 <- calculate_qstart_qend(0.8, df)

      results[[(i-1)*101+j]] <- list("parameter1" = mean[i],
                           "parameter2" = median[j],
                           "distribution" = df,
                           "hdr80" = hdr80)
      }
  }
}

saveRDS(results, snakemake@output[[1]])
