# source help functions
snakemake@source("help_functions.R")

# model parameters to test in simulation
## childcare setting
transmission_prob_care <- c(0.3, # from study used in the EsteR toolkit
                           seq(0.24, 0.37, length.out = 100)) # 95% CI from study used in the EsteR toolkit
cond_expectation_care <- c(3.3, # from study used in the EsteR toolkit
                           seq(1, 8, length.out = 100)) # 95% CI from study used in the EsteR toolkit
## school setting
transmission_prob_school <- c(0.12, # from study used in the EsteR toolkit
                            seq(0.05, 0.21, length.out = 100)) # 95% CI from study used in the EsteR toolkit
cond_expectation_school <- c(1.77, # from study used in the EsteR toolkit
                            seq(1, 8, length.out = 100)) # 95% CI from study used in the EsteR toolkit

## test sensitivity
sePCR <- 1 - c(1, 1, 1, 0.96, 0.66, 0.36, 0.24, 0.21, 0.20, 0.21, 0.23, 0.26,
                0.29, 0.33, 0.375, 0.42, 0.465, 0.51, 0.56, 0.60, 0.63, 0.67)
seAntigen <- c(0, 0, 0, 0.03, 0.15, 0.45,  0.73, 0.79, 0.8, 0.79, 0.71, 0.59,
                 0.46, 0.33, 0.24, 0.19, 0.15, 0.12, 0.095, 0.075, 0.055, 0.04)
scaleFactor <- sum(sePCR[7:13])/sum(seAntigen[7:13])*0.8
seAntigen <- seAntigen*scaleFactor
test_sensitivity <- lapply(c(0,  # from study used in the EsteR toolkit
                             seq(-0.2, -0.01, 0.01), seq(0.01, 0.2, 0.01)), function(x){data.frame(t = 0:21, PCR = sePCR+x, Antigen = seAntigen)}) # offset defined by us


# childcare scenario: 15 people, of which 1 was infected. 7 people got tested negatively with a PCR test 5 days after the last meeting with the infected person.
number_remaining <- 14
number_infected <- 1
test_infos <- matrix(nrow = 1, ncol = 2)
test_infos[1, 1] <- 1
test_infos[1, 2] <- 5
test_types <- matrix(nrow = 1, ncol = 1)
test_types[1, 1] <- "PCR"
day_size <- c(7)

childcare_results <- list()
for (i in 1:length(transmission_prob_care)){
  for (j in 1:length(cond_expectation_care)) {
    ## calculate probability
    prior <- calculate_prior_infections(number_remaining, number_infected, "care", transmission_prob_care[i], cond_expectation_care[j])
    probability <- calculate_posterior_no_infections(number_remaining, number_infected, event_type, test_infos, test_types, day_size, distribution = prior, test_sensitivity[[1]])

    childcare_results[[(i-1)*101+j]] <- data.frame("parameter1" = transmission_prob_care[i],
                                                   "parameter2" = cond_expectation_care[j],
                                                   "probability" = probability)
  }
}

test_sensitivity_results <- list()
for (i in 1:length(test_sensitivity)) {
  ## calculate probability
    prior <- calculate_prior_infections(number_remaining, number_infected, "care", transmission_prob_care[1], cond_expectation_care[1])
    probability <- calculate_posterior_no_infections(number_remaining, number_infected, event_type, test_infos, test_types, day_size, distribution = prior, test_sensitivity[[i]])

    test_sensitivity_results[[(i-1)*101+j]] <- data.frame("offset" = test_sensitivity[[i]]$PCR[5] - test_sensitivity[[1]]$PCR[5],
                                                   "probability" = probability)
}

# school scenario: 29 people, of which 2 were infected. 10 people got tested negatively with a PCR test 2 days after the last meeting with the infected person
# and 6 people got tested negatively with a PCR test days and a Antigen test 6 days after the last meeting with the infected person.
number_remaining <- 27
number_infected <- 2

test_infos <- matrix(nrow = 2, ncol = 3)
test_infos[1, 1] <- 1
test_infos[1, 2] <- 2
test_infos[2, 1] <- 2
test_infos[2, 2] <- 2
test_infos[2, 3] <- 6

test_types <- matrix(nrow = 2, ncol = 2)
test_types[1, 1] <- "PCR"
test_types[2, 1] <- "PCR"
test_types[2, 2] <- "Antigen"

day_size <- c(10, 6)

school_results <- list()
for (i in 1:length(transmission_prob_school)){
  for (j in 1:length(cond_expectation_school)) {
    ## calculate probability
    prior <- calculate_prior_infections(number_remaining, number_infected, "school", transmission_prob_school[i], cond_expectation_school[j])
    probability <- calculate_posterior_no_infections(number_remaining, number_infected, event_type, test_infos, test_types, day_size, distribution = prior, test_sensitivity[[1]])

    school_results[[(i-1)*101+j]] <- data.frame("parameter1" = transmission_prob_school[i],
                                                   "parameter2" = cond_expectation_school[j],
                                                   "probability" = probability)
  }
}

results <- list("childcare" = childcare_results,
                "school" = school_results,
                "sensitivity" = test_sensitivity_results)

saveRDS(results, snakemake@output[[1]])