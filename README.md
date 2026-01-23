# Age-dependent-Reference-Intervals (AdRI)

![](https://img.shields.io/github/license/SandraKla/AdRI.svg)
![](https://img.shields.io/github/last-commit/SandraKla/AdRI.svg)
![](https://img.shields.io/github/languages/count/SandraKla/AdRI.svg)
![](https://img.shields.io/github/languages/top/SandraKla/AdRI.svg)

<img src="www/Logo.png" width="300px" height="300px" align="right"/>

**Shiny App for calculating Age-dependent Reference Intervals!**

This **Shiny App** was developed to generate **A**ge-**d**ependent **R**eference **I**ntervals (**AdRI**) using various statistical methods. These include the **LMS method**, **GAMLSS (Generalized Additive Models for Location, Scale, and Shape)**, **Weighted Sliding Window Methods**, and **Regression Analysis**. These approaches allow for precise modeling and adaptation of reference intervals across different age groups, facilitating robust statistical analysis and visualization of age-dependent measurements.

<img src="docs/shiny_overview.png" align="center"/>

## Installation 

**Method 1:**
Use the function ```runGitHub()``` from the package [shiny](https://cran.r-project.org/web/packages/shiny/index.html):

```bash
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")
  library(shiny)}
runGitHub("AdRI", "SandraKla")
```

**Method 2:**
Download the Zip-File from this Shiny App. Unzip the file and set your working direction to the path of the folder. 
The package [shiny](https://cran.r-project.org/web/packages/shiny/index.html) and [shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/index.html) must be installed before using the Shiny App:

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

All required packages are downloaded when starting this app or imported if they already exist. For more information about the required packages use the [Homepage](https://sandrakla.github.io/AdRI/).

## Contact

You are welcome to:
- Submit suggestions and Bugs at: https://github.com/SandraKla/AdRI/issues
- Make a pull request on: https://github.com/SandraKla/AdRI/pulls
- Write an Email with any questions and problems to: s.klawitter@ostfalia.de

For more information use the [Homepage](https://sandrakla.github.io/AdRI/)!

Link to the publications:
- [A visualization tool for continuous reference intervals based on GAMLSS](https://doi.org/10.1515/labmed-2023-0033)
- [Automated sex and age partitioning for the estimation of reference intervals using a regression tree model](https://doi.org/10.1515/labmed-2024-0083)

## Disclaimer

Only anonymized data may be uploaded to this application. This application is provided “as is” and “as available”, without any express or implied warranties of any kind. No warranty is given regarding the accuracy, completeness, reliability, or timeliness of the results. The results are provided for informational and research purposes only and must not be used for diagnosis, treatment, prevention, or any form of clinical or medical decision-making. This application is not a medical device or medical product and does not replace professional medical advice. To the fullest extent permitted by law, the author disclaims all liability for any direct, indirect, incidental, consequential, or special damages arising from the use of this application or its results. Use of this application is entirely at your own risk.
