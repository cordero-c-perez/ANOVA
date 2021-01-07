
## General Note

There will typically be three files for each
mini stats project; one will contain the code (**.R**), another will be markdown
(**.Rmd**) that produces the last (**.pdf**) which can be viewed directly in github.
The (.pdf) file description includes the task description.



## File Descriptions

#### OLS_ElasticNet_Ridge_Lasso_RandomForest.r

A script file containing just the code used in the
OLS_ElasticNet_Ridge_Lasso_RandomForest.rmd file.

#### OLS_ElasticNet_Ridge_Lasso_RandomForest.rmd

A markdown file with an output converted to “.pdf” for browser
compatibility.

#### OLS_ElasticNet_Ridge_Lasso_RandomForest.pdf

Browser compatible output of OLS_ElasticNet_Ridge_Lasso_RandomForest.rmd file. This project
explores and applies four methods of linear regression, in addition to a
random forest model, to predict the sale price of homes based on 40
quantitative attributes (kaggle dataset link provided). The random
forest model is included to evaluate the trade-off in time vs. potential
increase in accuracy. The four linear regression methods applied within
are **Ordinary Least Squares** (OLS), **Ridge**, **Lasso**, and
**Elastic-Net**. Seeing as Ridge and Lasso are special cases of the
Elastic-Net, an arbitrary alpha (.5) is used in the Elastic-Net model.
This analysis also includes bootstrapped error bars of feature significance for
each model.


###### Note: data is not provided within this repository.
