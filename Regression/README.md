Regression Analysis Projects
================
C. Perez

## Repository Description

This repository houses projects exploring various applications of
regression. There will typically be three files for each project; one
will contain the code (**.R**), another will be markdown (**.Rmd**), and
the last (**.pdf**) can be viewed directly in github. The (.pdf) file
description includes the associated project description.

## Description of Files

#### reg\_analysis\_main.r

A script file containing just the code used in the
reg\_analysis\_main.rmd files.

#### reg\_analysis\_main.rmd

A markdown file with an output converted to “.pdf” for browser
compatibility.

#### reg\_analysis\_main.pdf

Browser compatible output of reg\_analysis\_main.rmd file. This project
explores and applies four methods of linear regression, in addition to a
random forest model, to predicting the sale price of homes based on 40
quantitative attributes for a kaggle dataset (link provided). The random
forest model is included to evaluate the trade-off in time vs. potential
increased accuracy. The four linear regression methods applied within
are **Ordinary Least Squares** (OLS), **Ridge**, **Lasso**, and
**Elastic-Net**. Seeing as Ridge and Lasso are special cases of the
Elastic-Net, an arbitrary alpha (.5) is used in the Elastic-Net model.
This analysis includes bootstrapped errors of parameter significance for
each model.

#### README.rmd

This file just creates a README.md file for the repository.

###### Note: data is not provided within this repository.
