# Age-dependent-Reference-Intervals

<img src="Logo.svg" width="225px" height="150px" align="right"/>

**Shiny App for calculating Age-dependent Reference Intervals**

This Shiny App offers the possibility to model age-dependent laboratory analytes and to calculate their reference intervals, see the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki). 


<img src="shiny.png" width="1500px" height="500px" align="center"/>

## Installation

Download the Zip-File from this Shiny App and set the working direction to the order and run:

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

All required packages are downloaded when starting this app or read in if they already exist, see also the [Wiki](https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki/References) for the required packages.
