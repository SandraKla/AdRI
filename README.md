# Age-dependent-Reference-Intervals (AdRI)
[![](https://img.shields.io/github/downloads/SandraKla/Age-dependent-Reference-Intervals/total.svg)]()
[![](https://img.shields.io/github/license/SandraKla/Age-dependent-Reference-Intervals.svg)]()
[![](https://img.shields.io/github/last-commit/SandraKla/Age-dependent-Reference-Intervals/master.svg)]()
[![](https://img.shields.io/github/languages/count/SandraKla/Age-dependent-Reference-Intervals.svg)]()
[![](https://img.shields.io/github/languages/top/SandraKla/Age-dependent-Reference-Intervals.svg)]()

<img src="www/Logo.svg" width="225px" height="150px" align="right"/>

**Shiny App for calculating Age-dependent Reference Intervals!**

This Shiny App was developed to create **A**ge-**d**ependent **R**eference **I**ntervals (**AdRI**) using different methods ([**LMS**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Generalized-additive-models-for-location,-scale-and-shape-(GAMLSS)#lms), [**GAMLSS**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Generalized-additive-models-for-location,-scale-and-shape-(GAMLSS)), [**Window-Methods**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Window-Methods) and [**Regression**](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Regression)) (see the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki)). 

<img src="www/shiny_overview.png" align="center"/>
<img src="www/shiny_tree.png" align="center"/>
<img src="www/shiny_gamlss.png" align="center"/>

## Installation

Download the Zip-File from this Shiny App, set your working direction to this path and run:

```bash
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")}
```

```bash
library(shiny)
runApp("app.R")
```
Or use the function ```runGitHub()``` from the package *shiny*:

```bash
library(shiny)
runGitHub("Age-dependent-Reference-Intervals", "SandraKla")
```

All required packages are downloaded when starting this app or imported if they already exist. For more information about the required packages use the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/Versions).
