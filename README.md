## Hapiness dataset:

### utils.r

This file contains utility functions for implementing the LINGAM (Linear, Non-Gaussian, Acyclic Model) algorithm, which is used for discovering causal structures in multivariate data.

It includes the following to:
- read data from a CSV file
- calculate adjacency matrices
- implement the LINGAM algorithm
- perform Wald tests for pruning edges
- plot the resulting directed acyclic graphs (DAGs)
- verify if a given adjacency matrix represents a DAG.


### happiness_dataprep.r

This file preprocesses the World Happiness dataset, performing the following tasks:

- Loads necessary libraries (`ggplot2`, `reshape2`, `gplots`, `GGally`, `dplyr`).
- Reads the dataset from a CSV file.
- Checks for and reports missing values.
- Removes unnecessary columns and rows with missing data.
- Plots histograms to visualize variable distributions.
- Conducts Shapiro-Wilk tests for normality.
- Generates scatter plots for variable pairs.
- Normalizes the data and saves it to a CSV file.
- Computes and plots a Pearson correlation heatmap.

### happiness_dataset_main.r

This file executes the main analysis for discovering causal structures in the World Happiness dataset using the LINGAM algorithm. It performs the following tasks:

- Installs and loads necessary packages (`igraph`).
- Sources utility functions from `utils.R`.
- Sets a random seed for reproducibility.
- Loads and preprocesses the dataset.
- Runs the LINGAM algorithm to determine causal structures.
- Prints and analyzes results before and after pruning.
- Plots the directed acyclic graph (DAG) before and after pruning.


