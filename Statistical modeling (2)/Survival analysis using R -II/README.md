# The >eR-Biostat initative
# Survival Analysis using R

This course in survival analysis (also known as the analysis of event-time data)  introduces the main ideas in non-parametric and semi-parametric regression for censored event-time data. Background theory is covered as well, but the emphasis is on applications. The course was developed by David Harrington and the material are organized into both standard lectures and interactive lab sessions.  All computing will be done using R. Lectures and labs will include both output and code; datasets will be in an R package. The course is an open source course, all course materials (slides, labs, R programs and Tex files) are available online.

The course cover the following topics:

**Introduction and background**

  + Basic definitions of survival distributions and hazard functions
  + Types of censoring
  + Parametric survival distributions

**Non-parametric estimation of a survival distribution**

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

**Proportional hazards regression: basics**

  + The Cox proportional hazards model
  + Partial likelihood estimation and inference
  + The link between the Cox model and the log-rank test
  + Time-varying covariates

**Proportional hazards regression: special topics**
 
  + Graphical diagnostics for the Cox model
  + Regression with correlated event-time data

**Designing a surival study**

  + Power and sample size calculations for survival distributions and proportions in two groups
  + Adjusting for staggered arrival and loss to follow-up
  + Software in \textsf{R} for trial design
  + Fixed sample designs vs sequential designs

## Course materials

* Course description: https://github.com/dave-harrington/survival_workshop/blob/master/course_description/survival_course_description.pdf
* Slides and Tex files for the lectures: https://github.com/dave-harrington/survival_workshop/tree/master/lectures
* Labs: https://github.com/dave-harrington/survival_workshop/tree/master/labs
* Notes (Survival_analysis in_R): https://github.com/dave-harrington/survival_workshop/blob/master/additional_resources/survival_analysis_in_R.pdf
* Notes (cox-regression in R and S-plus): https://github.com/dave-harrington/survival_workshop/blob/master/additional_resources/fox-appendix-cox-regression.pdf
* Papers: https://github.com/dave-harrington/survival_workshop/tree/master/clinical_papers and https://github.com/dave-harrington/survival_workshop/tree/master/methods_papers

## Course and slides developer:   Dave Harrington 
 * Email: davidharrington@g.harvard.edu 
 * Website: https://statistics.fas.harvard.edu/people/david-p-harrington
