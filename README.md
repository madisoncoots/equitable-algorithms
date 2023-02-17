# Designing Equitable Algorithms

This is the code base for our [Designing Equitable Algorithms](https://5harad.com/papers/designing-equitable-algorithms.pdf) paper.

To reproduce the figures in our paper, run the `all_figures.R` script. R data objects for the analyses are located in the `data/` repository, so it is not necessary to run any data preprocessing scripts to generate the figures. 

For completeness, we do also include the data preprocessing scripts that produce the R data objects. These scripts are `make_diabetes_data.R` and `make_survey_data.R`. `make_diabetes_data.R` pulls the tables directly from the CDC website, and `make_survey_data.R` uses a .csv of the raw survey responses we collected (also included in `data/`).
