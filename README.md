## Stats Repo  
###### Note: data is not provided in the repository. Links are available in the README.

This repository houses an ongoing series of small statistics projects. There will typically be three files
for each R project; one will contain pure code (**.R**), another will be
markdown (**.Rmd**), and the last (**.pdf**) can be viewed directly in
github. The pdf file description includes the associated project
description. There will be one file (.ipynb) for python projects as notebooks can be viewed directly in github.


## Files

#### [OLS_ElasticNet_Ridge_Lasso_RandomForest.pdf](https://github.com/cordero-c-perez/Stats/blob/master/OLS_ElasticNet_Ridge_Lasso_RandomForest.pdf)

This project explores and applies four methods of linear regression, in addition to a
random forest model, to predict the sale price of homes based on 40
quantitative attributes (kaggle dataset link provided). The random
forest model is included to evaluate the trade-off in time vs. potential
increase in accuracy. The four linear regression methods applied within
are **Ordinary Least Squares** (OLS), **Ridge**, **Lasso**, and
**Elastic-Net**. Seeing as Ridge and Lasso are special cases of the
Elastic-Net, an arbitrary alpha (.5) is used in the Elastic-Net model.
This analysis also includes bootstrapped error bars of feature significance for
each model. Data link is in the report.

#### [one\_factor_anova.pdf](https://github.com/cordero-c-perez/Stats/blob/master/one_factor_anova.pdf)

Viewable output of one\_factor.rmd file. This project explores (i) the
assumptions for the one factor (one-way) ANOVA test using Citibike data, and
(ii) various tools available in R to verify these assumptions prior to
conducting the analysis. Some tools explored in this analysis are as
follows: Shapiro-Wilk’s Normality Test, Levene’s Test for Equality of
Variance, Tukey’s Honest Significant Difference Test, distribution
visualization (histograms, boxplots), and further EDA. Citibike data was pulled and aggregated from [here](https://www.citibikenyc.com/system-data).

