# Global Mortality Patterns

Independent statistical analysis of worldwide mortality patterns using
Principal Component Analysis (PCA), hierarchical clustering, decision trees,
and regression modeling.

## Files included

- 3 Excel or CSV files containing raw data on global mortality causes, life expectancy, and GDP per capita, from Our World in Data.
- pdf file for the full report
- R script file used for the statistical analyses and data plots 

## Project goals

- Reduce 32 correlated mortality variables into interpretable latent dimensions.
- Identify clusters of countries with similar mortality profiles.
- Model life expectancy using principal component regression.

## Methods

- Data cleaning and integration
- Principal Component Analysis
- Hierarchical clustering (Ward's method)
- Decision tree interpretation
- Principal Component Regression
- Cross-validation
- LASSO comparison

## Results

- Reduced 32 correlated mortality variables to a small number of interpretable latent mortality dimensions using principal component analysis.
- Identified five distinct global mortality profiles corresponding to different stages of an epidemiological transition and regional development patterns.
- Developed an interpretable principal component regression model that predicts national life expectancy with approximately 91% cross-validated R^2 using only 9 principal components.
- Demonstrated that principal component regression achieves predictive performance comparable to much larger models with better stability by mitigating multicollinearity among mortality variables.
