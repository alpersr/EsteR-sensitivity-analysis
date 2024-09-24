# Evaluation of the EsteR Toolkit: Sensitivity Analysis of the Statistical Models

This repository contains the data and code for the sensitivity analysis of the statistical models used in the EsteR 
toolkit which is described in Alpers et al. 2022: *Evaluation of the Ester Toolkit for COVID-19 Decision Support:
Sensitivity Analysis and Usability Study*. JMIR Formative Research. DOI: 10.2196/preprints.44549.
With this code, the plots in the simulation report in Multimedia Appendix 1 were created.

## Reproducing the Sensitivity Analysis with Snakemake
### Requirements
The requirements.yml file can be used to create a conda environment with the snakemake package for workflow management
and all required R packages for the analysis. The environment can be created via

    mamba env create -f requirements.yml
(or *conda* if *mamba* is not installed) and activated via

    conda activate ester_env

### Running the whole analysis automatically
![Figure 1. DAG of Snakemake jobs.](dag.svg)

Above you can see the graph of jobs with their respective wildcards and the dependencies between them to recreate 
the plots from the simulation report. In order to create the DAG type

    snakemake --dag | dot -Tsvg > dag.svg
and to execute all jobs type 

    snakemake -c1 all 

into your terminal. (-c1 can be replaced by any other number of cores you want to use. With one core, 
the analysis takes about 15-20 minutes to complete.)

Inside the *EsteR-sensitivity-analysis* folder, Snakemake will automatically create a new folder called *results* 
where all simulation outcomes, calculated metrics and plots will be saved.

### Going through the analysis step-by-step
If you want to execute only certain jobs from the Snakefile, we will shortly describe the functionality of each Snakemake rule and give 
a list of accepted wildcards. For more detailed information about the use cases, scenarios and metrics see the main paper.
To run a specific job, you can type "snakemake -c1 path-to-desired-output" into the terminal, where the wildcards need 
to be replaced by a valid expression. E.g. to create the plot for the use case infection spread (with the only valid 
scenario _ and metric deltapred) type

    snakemake -c1 results/plots/infection_spread___deltapred.png

1. **find_simulation_parameters_**: The sensitivity analysis of three use cases uses data of the incubation time or 
serial interval of a COVID-19 infection. With this rule the default parameters for the web application and the 
parameter ranges for the sensitivity analysis can be identified.
   * {property}: *incubation_time* or *serial_interval*
2. **run_simulation_**: In the simulations, for each use case the respective model parameters are varied over specific 
ranges to calculate the outcomes in one or more scenarios. The simulations of the first three use cases depend on the 
previous step;  for the other two use cases the parameters and their ranges are directly integrated in the scripts.
   * {use_case}: *infection_period* or *infection_spread* or *illness_period* or *infectious_period* or 
   *group_quarantine*
3. **calculate_metrics_**: The outcomes for different model parameters are compared to the outcomes with default 
parameters in each use case by calculating different metrics.
   * {use_case}: *infection_period* or *infection_spread* or *illness_period* or *infectious_period* or 
   *group_quarantine*
4. **plot_metrics_**: For visualization, the values from the previous step are plotted for each scenario and metric 
in each use case separately. Only specific combinations of wildcards are allowed:
   * {use_case}: *infection_period*
     * {scenario}: *_*
     * {metric}: *iou* or *w1*
   * {use_case}: *infection_spread*
     * {scenario}: *_*
     * {metric}: *deltapred*
   * {use_case}: *illness_period*
     * {scenario}: *generation1* or *generation2* or *generation3*
     * {metric}: *iou* or *w1*
   * {use_case}: *infectious_period*
     * {scenario}: *_*
     * {metric}: *iou* or *w1*
   * {use_case}: *group_quarantine*
     * {scenario}: *childcare* or *school* or *sensitivity*
     * {metric}: *deltaprob*
