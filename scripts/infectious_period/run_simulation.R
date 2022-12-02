# source help functions
snakemake@source("help_functions.R")

# model parameters to test in simulation
mean <- c(12.89, # from study used in the EsteR toolkit
          seq(8,18, length.out = 100)) # grid defined by us
sd <- c(2.84, # from study used in the EsteR toolkit
        seq(1,5, length.out = 100)) # grid defined by us

# scenario: a person starts to show symptoms on March 22nd, 2022
symptom_begin_date <- as.Date("2022-03-22")

results <- list()
for (i in 1:length(mean)) {
  for(j in 1:length(sd)) {
    if (mean[i] < sd[j]) { # in this case no lognormal distribution can be derived
      results[[(i-1)*101+j]] <- list("parameter1" = mean[i],
                                    "parameter2" = sd[j],
                                    "distribution" = NA,
                                    "hdr80" = NA)
    } else {
      ## gamma parameters
      alpha <- (mean[i]/sd[j])^2
      beta <- mean[i]/sd[j]^2

      ## distribution
      df <- get_infectiousness_density(symptom_begin_date, dgamma, alpha, beta)

      ## 80% HDR
      hdr80 <- calculate_qstart_qend(0.8, df)

      results[[(i-1)*101+j]] <- list("parameter1" = mean[i],
                                    "parameter2" = sd[j],
                                    "distribution" = df,
                                    "hdr80" = hdr80)

    }
  }
}

saveRDS(results, snakemake@output[[1]])