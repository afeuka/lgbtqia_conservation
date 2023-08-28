# lgbtqia_conservation
Summary statistics, beta, and logistic regressions analyzing LGBTQIA+ perceptions of inclusion, safety, and belonging in the field of conservation.

## lgbtqia_summaries.R

Calculates summary statistics for survey data and creates comparison plots, primarily with tidyverse and ggplot2 packages.

## lgbtqia_analysis.R

Fits regression models for survey data. Beta regressions are conducted for proportion of the time respondents felt safe or a sense of belonging  with regard to sexual orientation or gender identity, in various locations, using the betareg package. Logistic regressions are conducted for whether or not respondents were out in a given location/to a group of people, using glm(). Pseduo R2 values are calculated for beta regression goodness of fit using the betareg package, and area under the receiver operating curve (AUC) scores were used as goodness of fit tests for logistic regressions, using the pROC pacakge.
