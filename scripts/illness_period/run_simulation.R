# source help functions
snakemake@source("help_functions.R")

# load parameters
simulation_params <- read.csv2(snakemake@input[[1]])

# model parameters to test in simulation
mean <- c(simulation_params$default[1], # from study used in the EsteR toolkit
          seq(simulation_params$min[1],simulation_params$max[1], length.out = 100)) # grid from all other studies
sd <- c(simulation_params$default[2], # from study used in the EsteR toolkit
          seq(simulation_params$min[2],simulation_params$max[2], length.out = 100)) # grid from all other studies


# scenario: a person starts to show symptoms on March 22nd, 2022
symptom_begin_date <- as.Date("2022-03-22")

results <- list()
for (i in 1:length(mean)) {
  for(j in 1:length(sd)) {
    if (mean[i] < sd[j]) { # in this case no lognormal distribution can be derived
      results[[(i-1)*101+j]] <- list("parameter1" = mean[i],
                                    "parameter2" = sd[j],
                                    "distribution1" = NA,
                                    "distribution2" = NA,
                                    "distribution3" = NA,
                                    "hdr80_1" = NA,
                                    "hdr80_2" = NA,
                                    "hdr80_3" = NA)
    } else {
      ## gamma parameters
      alpha <- (mean[i]/sd[j])^2
      beta <- mean[i]/sd[j]^2

      ## distributions
      df1 <- get_serial_interval_density(symptom_begin_date, 25, dgamma, alpha, beta)
      df2 <- get_serial_interval_density(symptom_begin_date, 35, dgamma, alpha*2, beta)
      df3 <- get_serial_interval_density(symptom_begin_date, 40, dgamma, alpha*3, beta)

      ## 80% HDRs
      hdr80_1 <- calculate_qstart_qend(0.8, df1)
      hdr80_2 <- calculate_qstart_qend(0.8, df2)
      hdr80_3 <- calculate_qstart_qend(0.8, df3)

      results[[(i-1)*101+j]] <- list("parameter1" = mean[i],
                                    "parameter2" = sd[j],
                                    "distribution1" = df1,
                                    "distribution2" = df2,
                                    "distribution3" = df3,
                                    "hdr80_1" = hdr80_1,
                                    "hdr80_2" = hdr80_2,
                                    "hdr80_3" = hdr80_3)

    }
  }
}

saveRDS(results, snakemake@output[[1]])