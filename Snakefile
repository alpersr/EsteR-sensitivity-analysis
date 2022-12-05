# To run all scripts and create all available plots
rule all:
    input:
        infection_period=expand("results/plots/infection_period___{metric}.png",
            metric=["iou", "w1"]),
        infection_spread="results/plots/infection_spread___deltapred.png",
        illness_period=expand("results/plots/illness_period_{scenario}_{metric}.png",
            scenario=["generation1", "generation2", "generation3"],
            metric=["iou", "w1"]),
        infectious_period=expand("results/plots/infectious_period___{metric}.png",
            metric=["iou", "w1"]),
        group_quarantine=expand("results/plots/group_quarantine_{scenario}_deltaprob.png",
            scenario=["childcare", "school", "sensitivity"])


# First step: identify default parameters and parameter ranges for use cases using
# the COVID-19 properties incubation time and serial interval
rule find_simulation_parameters_:
    input: "data/{property}.csv"
    output: "data/simulation_params_{property}.csv"
    script: "scripts/find_simulation_parameters.R"


# Second step: run the simulations for varying the model parameters with fixed input data scenarios
def input_simulation(wildcards):
    if wildcards[0] in ["infection_period", "infection_spread"]:
        return["data/simulation_params_incubation_time.csv"]
    elif wildcards[0] == "illness_period":
       return["data/simulation_params_serial_interval.csv"]
    return []

rule run_simulation_:
    input: input_simulation
    output: "results/simulation_results/{use_case}.RDS"
    script: "scripts/{wildcards.use_case}/run_simulation.R"


# Third step: calculate the metrics of the outcomes from the simulations
rule calculate_metrics_:
    input: "results/simulation_results/{use_case}.RDS"
    output: report("results/metric_results/{use_case}.csv", category = "Metric Values")
    script: "scripts/{wildcards.use_case}/calculate_metrics.R"


# Fourth step: plot the metrics depending on the model parameters
def input_plot_metrics(wildcards):
    inputs = [f"results/metric_results/{wildcards[0]}.csv"]
    if wildcards[0] in ["infection_period", "infection_spread"]:
        inputs.append("data/incubation_time.csv")
    elif wildcards[0] == "illness_period":
        inputs.append("data/serial_interval.csv")
    return inputs

rule plot_metrics_:
    input: input_plot_metrics
    output: report("results/plots/{use_case}_{scenario}_{metric}.png", category="Metric Plots", subcategory="{use_case}")
    script: "scripts/{wildcards.use_case}/plot_metrics.R"
