# S4DS - LiNGAM Project 

## Authors
- Sara Hoxha
- Milica Radojicic

In this project, we have implemented in R the LiNGAM algorithm to analyze and determine causal order on the "TEH_World_Happiness_2019_Imputed" dataset.

## Project Structure

The project is organized into the following directories:

### 1. **Code**
   - **Purpose**: This folder contains all the R code used in the project.
   - **Files**:
     - `main.R`: The main script that calls the LINGAM algorithm and prints the primary results, as well as the causal graph.
     - `utils.R`: Utility functions and helper code used throughout the project. It includes the LINGAM algorithm, Wald Test, and plot graphing.
     - `dataprep.R`: Code for preprocessing the dataset, including normalization, removal of redudant features, and testing LiNGAM assumptions.

### 2. **Figures**
   - **Purpose**: This folder includes all the plots and figures generated from the project’s code.
   - **Files**:
     - `CausalGraph.png`: Visualization of the causal graph derived from applying LiNGAM.
     - `Happiness_Correlation_Heatmap.png`: Heatmap representing the Pearson correlation between features.
     - `Happiness_Scatter_Plot.png`: Scatter plots illustrating the relationships between different features.

### 3. **Dataset**
   - **Purpose**: This folder contains the data files used in the project.
   - **Files**:
     - `TEH_World_Happiness_2019_Imputed.csv`: The original dataset.
     - `TEH_World_Happiness_2019_Imputed_Normalized.csv`: The dataset after preprocessing which is used for the LiNGAM algorithm.
