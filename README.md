# Age-dependent-Reference-Intervals (AdRI)

![](https://img.shields.io/github/license/SandraKla/Age-dependent-Reference-Intervals.svg)
![](https://img.shields.io/github/last-commit/SandraKla/Age-dependent-Reference-Intervals.svg)
![](https://img.shields.io/github/languages/count/SandraKla/Age-dependent-Reference-Intervals.svg)
![](https://img.shields.io/github/languages/top/SandraKla/Age-dependent-Reference-Intervals.svg)

<img src="www/Logo.svg" width="225px" height="150px" align="right"/>

**Shiny App for calculating Age-dependent Reference Intervals!**

This Shiny App was developed to create **A**ge-**d**ependent **R**eference **I**ntervals (**AdRI**) using different methods: [**LMS**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Generalized-additive-models-for-location,-scale-and-shape-(GAMLSS)#lms), [**GAMLSS**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Generalized-additive-models-for-location,-scale-and-shape-(GAMLSS)), [**Window-Methods**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Window-Methods) and [**Regression**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Regression).

<img src="www/shiny_overview.png" align="center"/>

## Installation 

**Method 1:**
Use the function ```runGitHub()``` from the package [shiny](https://cran.r-project.org/web/packages/shiny/index.html):

```bash
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")
  library(shiny)}
runGitHub("Age-dependent-Reference-Intervals", "SandraKla")
```

**Method 2** (not recommended):
Download the Zip-File from this Shiny App. Unzip the file and set your working direction to the path of the folder. 
The package [shiny](https://cran.r-project.org/web/packages/shiny/index.html) (≥ 1.7.1) must be installed before using the Shiny App:

```bash
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")
  library(shiny)}
```
And then start the app with the following code:
```bash
runApp("app.R")
```

All required packages are downloaded when starting this app or imported if they already exist. For more information about the required packages use the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Versions).

## Contact

**Warning!** This Shiny application has not been enough validated for the basis of a medical diagnosis! There is no warranty for the app and/or the reference intervals!

You are welcome to:
- Submit suggestions and bugs at: https://github.com/SandraKla/Age-dependent-Reference-Intervals/issues
- Make a pull request on: https://github.com/SandraKla/Age-dependent-Reference-Intervals/pulls

For more information use the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki)! 
