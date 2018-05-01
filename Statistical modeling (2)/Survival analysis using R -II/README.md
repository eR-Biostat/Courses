# The >eR-Biostat initative
# Survival Analysis using R

This course in survival analysis (also known as the analysis of event-time data) will introduce the main ideas in non-parametric and semi-parametric regression for censored event-time data. Some background theory will be covered, but the emphasis will be on applications. The material are organized into both standard lectures and interactive lab sessions. All computing will be done using **R**. Lectures and labs will include both output and code; datasets will be in an **R** package.  

The main topics:

*Introduction and background*

  + Basic definitions of survival distributions and hazard functions
  + Types of censoring
  + Parametric survival distributions

*Non-parametric estimation of a survival distribution*

  + The Kaplan-Meier estimator
  + The cumulative hazard estimator
  + Estimating standard errors, including Greenwood's formula and the delta method for transformations
  + Confidence intervals and confidence bands for survival distributions
  + Alternatives to median survival: restricted mean survival

*Significance tests with censored data*

 + The log-rank test for two samples
  + The Tarone-Ware family of weighted log-rank tests for non-proportional hazards
  + Tests for more than two groups
  + Stratified tests

*Proportional hazards regression: basics*

  + The Cox proportional hazards model
  + Partial likelihood estimation and inference
  + The link between the Cox model and the log-rank test
  + Time-varying covariates

*Proportional hazards regression: special topics*
 
  + Graphical diagnostics for the Cox model
  + Regression with correlated event-time data

*Designing a surival study*

  + Power and sample size calculations for survival distributions and proportions in two groups
  + Adjusting for staggered arrival and loss to follow-up
  + Software in \textsf{R} for trial design
  + Fixed sample designs vs sequential designs

## Course materials

* Slides: https://github.com/eR-Biostat/Courses/tree/master/Statistical%20modeling%20(2)/Survival%20Analysis%20using%20R/Slides
* Examples in R: https://github.com/eR-Biostat/Courses/tree/master/Statistical%20modeling%20(2)/Survival%20Analysis%20using%20R/Practical%20survival%20analysis%20in%20R
* You Tube tutorials: https://github.com/eR-Biostat/Courses/tree/master/Statistical%20modeling%20(2)/Survival%20Analysis%20using%20R/YouTube%20tutorials

## Course and slides developer:   Dave Harrington 
 * Email: davidharrington@g.harvard.edu 
 * Website: https://statistics.fas.harvard.edu/people/david-p-harrington
