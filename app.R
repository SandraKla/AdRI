####################################### WELCOME TO THE SHINY APP AdRI #############################
####################################### from Sandra K. (2025) #####################################
###################################################################################################

####################################### Scripts ###################################################

source("R/analysis.R")
source("R/window.R")
source("R/gamlss.R")

####################################### Libraries #################################################

if ("boot" %in% rownames(installed.packages())) {
  library(boot)} else{
    install.packages("boot")
    library(boot)}

if ("dplyr" %in% rownames(installed.packages())) {
  library(dplyr)} else{
    install.packages("dplyr")
    library(dplyr)}

if ("DT" %in% rownames(installed.packages())) {
  library(DT)} else{
    install.packages("DT")
    library(DT)}

if ("gamlss" %in% rownames(installed.packages())) {
  library(gamlss)} else{
    install.packages("gamlss")
    library(gamlss)}

if ("gamlss.add" %in% rownames(installed.packages())) {
  library(gamlss.add)} else{
    install.packages("gamlss.add")
    library(gamlss.add)}

if ("ggplot2" %in% rownames(installed.packages())) {
  library(ggplot2)} else{
    install.packages("ggplot2")
    library(ggplot2)}

if ("Hmisc" %in% rownames(installed.packages())) {
  library(Hmisc)} else{
    install.packages("Hmisc")
    library(Hmisc)}

if ("plotly" %in% rownames(installed.packages())) {
  library(plotly)} else{
    install.packages("plotly")
    library(plotly)}

if ("reflimR" %in% rownames(installed.packages())) {
  library(reflimR)} else{
    install.packages("reflimR")
    library(reflimR)}

if ("reflimR.expand" %in% rownames(installed.packages())) {
  library(reflimR.expand)} else{

    if ("devtools" %in% rownames(installed.packages())) {
      library(reflimR)} else{
        install.packages("devtools")
        library(devtools)}

    devtools::install_github("SandraKla/reflimR.expand")
    library(reflimR.expand)}

if ("rlang" %in% rownames(installed.packages())) {
  library(rlang)} else{
    install.packages("rlang")
    library(rlang)}

if ("rpart" %in% rownames(installed.packages())) {
  library(rpart)} else{
    install.packages("rpart")
    library(rpart)}

if ("rpart.plot" %in% rownames(installed.packages())) {
  library(rpart.plot)} else{
    install.packages("rpart.plot")
    library(rpart.plot)}

if ("shinycssloaders" %in% rownames(installed.packages())) {
  library(shinycssloaders)} else{
    install.packages("shinycssloaders")
    library(shinycssloaders)}

if ("shinydashboard" %in% rownames(installed.packages())) {
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

if ("shinyjs" %in% rownames(installed.packages())) {
  library(shinyjs)} else{
    install.packages("shinyjs")
    library(shinyjs)}

if ("truncnorm" %in% rownames(installed.packages())) {
  library(truncnorm)} else{
    install.packages("truncnorm")
    library(truncnorm)}

if ("zoo" %in% rownames(installed.packages())) {
  library(zoo)} else{
    install.packages("zoo")
    library(zoo)}

####################################### Texts #####################################################

text <- HTML(paste0(strong("Shiny App for calculating Age-dependent Reference Intervals!"), br(), br(),
"This Shiny App was developed to create Age-dependent Reference Intervals (AdRI) using different methods: LMS, GAMLSS, weighted Sliding window method and Regression.", br(), 
"For further information visit our ", a("Homepage", href = "https://sandrakla.github.io/AdRI/"),"!"))

windowtext1 <- HTML(paste0("This is a regular Window-method, the window is make regular in the same size through the data, 
so it is only recommended for small changes through the age."))

windowtext2 <- HTML(paste0("Load a TXT file with the age groups from your Laboratory Information System (LIS)
and the Reference Intervals will be calculated."))

windowtext3 <- HTML(paste0("Decision Trees are used for machine learning. It can be used for classification (supervised learning), but also
for clustering (unsupervised learning). Here it used to cluster the data into subgroups with similar values.
The Decision Tree is from the package rpart and is visualized with rpart.plot. The subgroups according to the Decision Tree
are used to calculate the reference intervals. For futher informations read the ", a("publication", href = "https://doi.org/10.1515/labmed-2024-0083"),"!"))

windowtext4 <- HTML(paste0("Computes reference intervals based on a sliding window technique. There are two variants. 
A fixed window size and a fixed step width by which the window is shifted or windows with a guaranteed minimum number of values."))

windowtext5 <- HTML(paste0("The weighted sliding window method iterates through the data and calculates reference intervals for each window. 
It supports multiple distribution functions, including Gaussian, triangular, and trapezoidal distributions, with customizable parameters. 
These distributions are used to assign weights to the data, on which the program then computes and generates a reference interval chart. 
For the triangular distribution, the vertex position ranges from 0 to 1, forming an isosceles triangle when set at 0.5. 
In the trapezoidal distribution, the top left and right vertices also range from 0 to 1, creating an isosceles trapezoid when set to 0.3 and 0.6."))

regressiontext <- HTML(paste0("Regression can be used for normally distributed data to get the 95% prediction interval. The blue
line show the 95% Confindence interval from the regression in black and the red the prediction interval (2.5% and 97.5%)."))

gamlsstext1 <- HTML(paste0("Generalized additive models for location, scale and shape (GAMLSS) are distributional regression models, 
where all the parameters of the assumed distribution for the response can be modeled as additive functions of the explanatory variables. 
GAMLSS can be created with various distributions and different additive terms for smoothing. For futher informations read the ", 
a("publication", href = "https://doi.org/10.1515/labmed-2023-0033"),"!"))

gamlsstext2 <- HTML(paste0("Models can be compared visually or with the Akaike Information Criterion (AIC),
Generalized Information Criterion (GAIC), Bayesian Information Criterion (BIC),
or Pseudo R-Squared (R^2). The model with the smallest value for AIC, BIC and GAIC is the best model for the data.
The Pseudo R-Squared (R^2) should be as large as possible for a good model. These values are colored. For futher informations read the ", 
a("publication", href = "https://doi.org/10.1515/labmed-2023-0033"),"!"))

gamlsstext3 <- HTML(paste0("After fitting the models, the residuals can be calculated and high values can be removed to delete possible 
outliers from the model and refit the model. High residuals values are in red, low in blue", 
strong("This must not be equal to the real outliers of the data!", br())))

####################################### USER INTERFACE ############################################

ui <- dashboardPage(
  dashboardHeader(title = "AdRI", titleWidth = 175),
  dashboardSidebar(width = 175,
                   sidebarMenu(
                     menuItem("Analysis", tabName = "analysis", icon = icon("database")),
                     menuItem("Window-Methods", tabName = "window", icon = icon("align-center"), startExpanded = FALSE,
                              menuSubItem("Regular", tabName = "windowregular", icon = icon("chart-bar")),
                              menuSubItem("LIS", tabName = "windowlis", icon = icon("chart-bar")),
                              menuSubItem("Decision Tree", tabName = "windowtree", icon = icon("tree")),
                              menuSubItem("Sliding", tabName = "sliding", icon = icon("chart-line")),
                              menuSubItem("Weighted Sliding", tabName = "windowsliding", icon = icon("chart-line"))),
                     menuItem("Regression", tabName = "regression", icon = icon("chart-line"), startExpanded = FALSE,
                              menuSubItem("Regression", tabName = "regressionoverview", icon = icon("chart-line")),
                              menuSubItem("Comparison", tabName = "regressioncomparison", icon = icon("balance-scale"))),
                     menuItem("GAMLSS", tabName = "gamlss", icon = icon("chart-line"), startExpanded = FALSE,
                              menuSubItem("GAMLSS and LMS", tabName = "gamlsslms", icon = icon("chart-line")),
                              menuSubItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
                              menuSubItem("Percentiles", tabName = "percentiles", icon = icon("table")),
                              menuSubItem("Residuals", tabName = "residuals", icon = icon("eraser")))
                   )),
  dashboardBody(
    
    tags$style("html, body {overflow: visible !important;"),
    
    tabItems(
      tabItem(tabName = "analysis",
              
              fluidRow(
                ### Sidebar - Analysis ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  selectInput("dataset", "Select dataset:", choices = list.files(pattern = c(".csv"), recursive = TRUE)),
                  uiOutput("dataset_file"),
                  actionButton('reset', 'Reset Input', icon = icon("trash")), hr(),
                  
                  helpText("Data Preprocessing:"),
                  
                  radioButtons("days_or_years", "Unit for the selection of the age-range:", c("year" = "age", "day" = "age_days")),
                  
                  conditionalPanel(
                    condition = "input.days_or_years == 'age'",
                    sliderInput("age_end", "Select age-range in years:", min = 0, max = 100, value = c(0, 18))),
                  
                  conditionalPanel(
                    condition = "input.days_or_years == 'age_days'",
                    numericInput("age_input_min", "Select age-range in days from:", 0, min = 0, max = 100 * 365)),
                  
                  conditionalPanel(
                    condition = "input.days_or_years == 'age_days'",
                    numericInput("age_input", "to:", 100, min = 1, max = 100 * 365)),
                  
                  selectInput("sex", "Select the sex:", choices = list("Male + Female" = "t", "Male" = "m", "Female" = "f")),
                  
                  textInput("text_unit", "Unit of the analyte:", value = "Unit"),
                  checkboxInput("unique", "First unique values", value = TRUE),  hr(),

                  checkboxInput("checkboxtukey", "Use Outlierdetection with iboxplot()", value = FALSE)),
                
                ### MainPanel - Analysis ###
                column(
                  width = 9,
                  tabsetPanel(
                    tabPanel("Overview", icon = icon("home"),
                             
                             p(text), 
                             withSpinner(plotlyOutput("scatterplot_plotly", height = "700px"))),
                    
                    tabPanel("Dataset", icon = icon("table"),
                             
                             p(text),  
                             DT::dataTableOutput("datatable")),
                    
                    tabPanel("Barplots", icon = icon("chart-bar"),
                             
                             p(text),
                             
                             withSpinner(plotOutput("barplot_sex", height = "375px")),
                             withSpinner(plotOutput("barplot_value", height = "375px")))
                  )
                )
              )),

      ### Regular Window-Method ###
      tabItem(tabName = "windowregular",
              
              fluidRow(
                ### Sidebar - Regular Window-Method ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(windowtext1),
                  
                  sliderInput("window_age", "Regular Window-range for the age-subgroups in years:", 1, 10, 10),
                  conditionalPanel(condition = "input.window_age <= 1", 
                                   numericInput("window_agedays", "Regular Window-range for the age-subgroups in days:", 
                                                365, min = 1, max = 100*365)),
                  numericInput("window_age_nmin", "reflimR: n.min", 120, min = 40, max = 10000)
                ),
                
                ### MainPanel - Regular Window-Method ###
                column(width = 9,
                       fluidRow(
                         box(title = tagList(shiny::icon("chart-line"), "Plot"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",

                             withSpinner(plotOutput("window", height = "475px"))
                         ),
                         
                         box(title = tagList(shiny::icon("table"), "Table"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             class = "custom-scrollbox",
                             
                             DT::dataTableOutput("windowtable_t"),
                             downloadButton("Download_zlog_table_window_data_all_reflimR", "Table with Reference Intervals for Zlog_AdRI")
                         ))
                ))),
      
      ##### LIS Window-Method ##### 
      tabItem(tabName = "windowlis",
              
              fluidRow(
                ### Sidebar - LIS Window-Method ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(windowtext2),
                  
                  selectInput("lis_data", "Select preinstalled dataset:", choices = list.files(pattern = "txt", recursive = TRUE)),
                  uiOutput("lis_data_file"),
                  actionButton('reset_lis', 'Reset Input', icon = icon("trash")),
                  numericInput("lis_data_nmin", "reflimR: n.min", 120, min = 40, max = 10000)
                  ),
                
                ### MainPanel - LIS Window-Method ###
                column(width = 9,
                       fluidRow(
                         box(title = tagList(shiny::icon("chart-line"), "Plot"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("lis_window", height = "475px"))
                         ),
                         
                         box(title = tagList(shiny::icon("database"), "Laboratory Information System (LIS)"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             
                             withSpinner(plotOutput("lis_window_analysis", height = "475px"))
                         ),
                         
                         box(title = tagList(shiny::icon("table"), "Table"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             class = "custom-scrollbox",
                             
                             DT::dataTableOutput("lis_table_t"),
                             downloadButton("Download_zlog_lis_table_t", "Table with Reference Intervals for Zlog_AdRI")
                         ))
                ))),
      
      ##### Decision Tree Window-Method ##### 
      tabItem(tabName = "windowtree",
              
              fluidRow(
                ### Sidebar - Decision Tree Window-Method ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(windowtext3),
                 
                  numericInput("tree_window_minsplit", "rpart: minsplit", 20, min = 10, max = 100),
                  numericInput("tree_window_cp", "rpart: Complexity parameter", 0.01, min = 0, max = 10),
                  numericInput("tree_window_nmin", "reflimR: n.min", 120, min = 40, max = 10000)
                ),
                
                ### MainPanel - Decision Tree Window-Method ###
                column(width = 9,
                       fluidRow(
                         box(title = tagList(shiny::icon("chart-line"), "Plot"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("tree_window", height = "475px"))
                         ),
                         
                         box(title = tagList(shiny::icon("tree"), "Decision Tree"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             
                             withSpinner(plotOutput("tree_rpart", height = "475px")),
                             downloadButton("download_tree", "Decision Tree")
                         ),
                         
                         box(title = tagList(shiny::icon("table"), "Table"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             class = "custom-scrollbox",

                            downloadButton("Download_zlog_window_data_split_reflimR", "Table with Reference Intervals for Zlog_AdRI")
                         ))
                ))),
      
      ##### Sliding Window-Method ##### 
      
      tabItem(tabName = "sliding",
              
              fluidRow(
                ### Sidebar - Regular Sliding ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(windowtext4),
                  
                  checkboxInput("checkboxregularfixed", "Use fixed window size", value = FALSE),
                  
                  conditionalPanel(
                    condition = "input.checkboxregularfixed == true",
                    numericInput("regular_window_size_truncated", "Window Size", 1000, min = 100, max = 10000),
                    numericInput("regular_step_width_truncated", "Step Width", 100, min = 1, max = 10000)
                  ),
                  
                  conditionalPanel(
                    condition = "input.checkboxregularfixed == false",
                    numericInput("slding_window_nmin.window", "reflimR.expand: n.min.window", 200, min = 0, max = 10000)
                  )
                ),
                
                ### MainPanel - Regular Sliding ###
                column(width = 9,
                       fluidRow(
                         box(title = tagList(shiny::icon("chart-line"), "Plot"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("sliding_window", height = "475px"))
                         ))
                ))),

      tabItem(tabName = "windowsliding",
              fluidRow(

                ### Sidebar - Sliding Window-Method ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",

                  p(windowtext5),
                  
                  selectInput("distribution",
                              label = div(icon("wave-square"), "Choose weight function:"),
                              choices = c("Truncated gaussian" = "truncated_gaussian",
                                          "Triangular" = "triangular",
                                          "Trapezoidal" = "trapezoidal"),
                              selected = "truncated_gaussian"),
                  
                  sliderInput("segment",
                              "Choose Segment:",
                              min = 1,
                              max = 1, 
                              value = 1,
                              step = 1,
                              animate = animationOptions(interval = 1000, loop = TRUE)),
                  
                  # for truncated_gaussian
                  conditionalPanel(
                    condition = "input.distribution == 'truncated_gaussian'",
                    
                    checkboxInput("checkboxgaussianfixed", "Use fixed window size", value = FALSE),
                    
                    conditionalPanel(
                      condition = "input.checkboxgaussianfixed == true",
                      numericInput("window_size_truncated", "Window Size", 1000, min = 100, max = 10000),
                      numericInput("step_width_truncated", "Step Width", 100, min = 1, max = 10000)
                    ),
                    
                    numericInput("standard_deviation_truncated", "Standard deviation:", value = 50)
                  ), 
                  # for triangular
                  conditionalPanel(
                    condition = "input.distribution == 'triangular'",
                    
                    checkboxInput("checkboxtriangularfixed", "Use fixed window size", value = FALSE),
                    
                    conditionalPanel(
                      condition = "input.checkboxtriangularfixed == true",
                      numericInput("window_size_triangular", "Window Size", 1000, min = 100, max = 10000),
                      numericInput("step_width_triangular", "Step Width", 100, min = 1, max = 10000)
                    ),
                    
                    numericInput("vertex1", "Vertex:", value = 0.5)
                  ),
                  # for trapezoidal
                  conditionalPanel(
                    condition = "input.distribution == 'trapezoidal'",
                    
                    checkboxInput("checkboxtrapezoidalfixed", "Use fixed window size", value = FALSE),
                    
                    conditionalPanel(
                      condition = "input.checkboxtrapezoidalfixed == true",
                      numericInput("window_size_trapezoidal", "Window Size", 1000, min = 100, max = 10000),
                      numericInput("step_width_trapezoidal", "Step Width", 100, min = 1, max = 10000)
                    ),
                    
                    numericInput("vertex1_trap", "Top left vertex:", value = 0.3),
                    numericInput("vertex2_trap", "Top right vertex:", value = 0.6)
                  ),
                  
                  numericInput("weight_threshold", "Weight threshold:", value = NA, min = 1, max = 1000),
                ),

                ### MainPanel - Sliding Window-Method ###
                column(width = 9,
                       fluidRow(
                         
                         box(title = tagList(shiny::icon("chart-line"), "Plot"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("sliding_scatterPlot", height = "475px"))
                         ),
                         
                         box(title = tagList(shiny::icon("chart-line"), "Segments"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("sliding_scatterPlot2", height = "475px"))
                         ),
                         
                         box(title = tagList(shiny::icon("chart-line"), "Weight Distribution"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("weightDistPlot", height = "475px"))
                         ))
                ))),
      
      ### Regression ###
      tabItem(tabName = "regressionoverview",
              fluidRow(
                
                ### Sidebar - Regression ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(regressiontext)
                ),
                
                ### MainPanel - Regression ###
                column(width = 9,
                       fluidRow(
                         box(title = tagList(shiny::icon("chart-line"), "Plot"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             withSpinner(plotOutput("regression", height = "600px"))
                         ),
                         
                         box(title = tagList(shiny::icon("table"), "Table"),
                             width = 12,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             class = "custom-scrollbox",
                             
                             DT::dataTableOutput("regression_linear"), 
                             DT::dataTableOutput("regression_poly10"), 
                             DT::dataTableOutput("regression_poly2"), 
                             DT::dataTableOutput("regression_poly3")
                         ))
                ))),

      ### Regression - Comparison ###
      tabItem(tabName = "regressioncomparison",
              fluidRow(
                
                ### Sidebar - Regression - Comparison ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(regressiontext)
                ),
                
                ### MainPanel - Regression - Comparison ###
                column(width = 9,
                       fluidRow(
                         DT::dataTableOutput("regression_table"))
                ))),
      
      tabItem(tabName = "gamlsslms",
              fluidRow(
                
                ### Sidebar - GAMLSS ###
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(gamlsstext1),
                  
                  actionButton("button_lms", "Start LMS", icon("calculator"), onclick = "$(tab).removeClass('disabled')"),
                  htmlOutput("buttons_lms"),
                  hr(),
                  selectInput(
                    "distribtion_gamlss",
                    "Distribution for GAMLSS:",
                    choices = list(
                      "Log-Normal Distribution" = "LOGNO",
                      "Normal Distribution" = "NO",
                      "Box-Cox" = c(
                        #"Box-Cole Green Distribution" = "BCCG",
                        "Box-Cole Green Distribution (orginal)" = "BCCGo",
                        #"Box-Cole Green Exp. Distribution" = "BCPE",
                        "Box-Cole Green Exp. Distribution (orginal)" = "BCPEo",
                        #"Box-Cole Green T-Distribution" = "BCT",
                        "Box-Cole Green T-Distribution (orginal)" = "BCTo"
                      )
                    )
                  ),
                  checkboxInput("checkbox", "Distribution proposed by the LMS", value = FALSE),
                  actionButton("button_gamlss", "Start GAMLSS", icon("calculator"), onclick = "$(tabs).removeClass('disabled')"),
                  htmlOutput("buttons_gamlss")
                ),
                
                ### MainPanel - GAMLSS ###
                column(width = 9,
                       tabsetPanel(
                         tabPanel(
                           "LMS", 
                           icon = icon("chart-line"), 
                           value = "nav_lms",
                           
                           fluidRow(
                             box(title = tagList(shiny::icon("chart-line"), "Plot"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "primary",
                                 
                                 withSpinner(plotOutput("lms", height = "475px"))
                             ),
                             
                             box(title = tagList(shiny::icon("calculator"), "Statistics"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "info",
                                 class = "custom-scrollbox",
                                 
                                 verbatimTextOutput("lms_text"),
                                 withSpinner(plotOutput("lms_plot")),
                                 withSpinner(plotOutput("lms_fitted")),
                                 withSpinner(plotOutput("lms_wormplots")))
                           )),
                         
                         tabPanel(
                           "GAMLSS (Splines)",
                           icon = icon("chart-line"),
                           value = "nav_gamlss",
                           
                           fluidRow(
                             box(title = tagList(shiny::icon("chart-line"), "Plot"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "primary",
                                 
                                 withSpinner(plotOutput("gamlss_models_splines", height = "475px"))
                             ),
                             
                             box(title = tagList(shiny::icon("calculator"), "Statistics"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "info",
                                 class = "custom-scrollbox",
                                 
                                 p("GAMLSS with P-Splines:"),
                                 verbatimTextOutput("gamlss_text_psplines"),
                                 withSpinner(plotOutput("gamlss_term_pb")),
                                 withSpinner(plotOutput("gamlss_fitted_pb_")),
                                 p("GAMLSS with Cubic-Splines:"),
                                 verbatimTextOutput("gamlss_text_splines"),
                                 withSpinner(plotOutput("gamlss_term_cs")),
                                 withSpinner(plotOutput("gamlss_fitted_cs_")),
                                 p("Wormplots for GAMLSS with the P-Splines and Cubic Splines:"),
                                 withSpinner(plotOutput("wormplots_splines")))
                           )),
                         
                         tabPanel(
                           "GAMLSS (Polynomials)",
                           icon = icon("chart-line"),
                           value = "nav_gamlss",
                           
                           fluidRow(
                             box(title = tagList(shiny::icon("chart-line"), "Plot"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "primary",
                                 
                                 withSpinner(plotOutput("gamlss_models_poly", height = "475px"))
                             ),
                             
                             box(title = tagList(shiny::icon("calculator"), "Statistics"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "info",
                                 class = "custom-scrollbox",
                                 
                                 p("GAMLSS with Polynomial Degree 3:"),
                                 verbatimTextOutput("gamlss_text_poly"),
                                 withSpinner(plotOutput("gamlss_term_poly")),
                                 withSpinner(plotOutput("gamlss_fitted_poly_")),
                                 p("GAMLSS with Polynomial Degree 4:"),
                                 verbatimTextOutput("gamlss_text_poly4"),
                                 withSpinner(plotOutput("gamlss_term_poly4")),
                                 withSpinner(plotOutput("gamlss_fitted_poly4_")),
                                 p("Wormplots for GAMLSS with the Polynomial Degree 3 and 4:"),
                                 withSpinner(plotOutput("wormplots_poly")))
                           )),
                         
                         tabPanel(
                           "GAMLSS (Neural Network)",
                           icon = icon("brain"),
                           value = "nav_gamlss",
                           
                           fluidRow(
                             box(title = tagList(shiny::icon("chart-line"), "Plot"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "primary",
                                 
                                 withSpinner(plotOutput("gamlss_net", height = "475px"))),
                             
                             box(title = tagList(shiny::icon("calculator"), "Statistics"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "info",
                                 class = "custom-scrollbox",
                                 
                                 verbatimTextOutput("net_text"),
                                 # Plot neural network with term.plot(nn_)
                                 withSpinner(plotOutput("network_term")),
                                 withSpinner(plotOutput("network_fitted")),
                                 withSpinner(plotOutput("nn_wormplots")))
                           )),
                         
                         tabPanel(
                           "GAMLSS (Decision Tree)",
                           icon = icon("tree"),
                           value = "nav_gamlss",
                           
                           fluidRow(
                             box(title = tagList(shiny::icon("chart-line"), "Plot"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "primary",
                                 
                                 withSpinner(plotOutput("gamlss_tree", height = "475px"))
                             ),
                             
                             box(title = tagList(shiny::icon("calculator"), "Statistics"),
                                 width = 12,
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 status = "info",
                                 class = "custom-scrollbox",
                                 
                                 verbatimTextOutput("tree_text"),
                                 withSpinner(plotOutput("rpart_tree")),
                                 withSpinner(plotOutput("tree_term")),
                                 withSpinner(plotOutput("tree_fitted")),
                                 withSpinner(plotOutput("tr_wormplots")))
                           )
                         )
                       )
                ), tags$script(src = 'tabs_enabled.js'))), 
      
      ### GAMLSS - Comparison ###
      tabItem(tabName = "comparison",
              fluidRow(
                
                box(
                  title = tagList(shiny::icon("gear"), "Settings"),
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  p(gamlsstext2)
                ),
                
                box(
                  title = tagList(shiny::icon("balance-scale"), "Comparison"),
                  width = 9,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  DT::dataTableOutput("table_compare"),
                  withSpinner(plotOutput("metrics", height = "475px"))
                ))),
      
      ### GAMLSS - Prediction ###
      tabItem(tabName = "percentiles",
              
              fluidRow(column(
                width = 3,
                
                fluidRow(
                  box(
                    title = tagList(shiny::icon("gear"), "Settings"),
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    
                    selectInput(
                      "select_model",
                      "Select model:",
                      choices = list(
                        "Splines" = c("P-Splines" = "pb_ri",
                                      "Cubic Splines" = "cs_ri"),
                        "LMS" = c(LMS = "lms_ri"),
                        "Polynomial" = c(
                          "Polynomial (Degree 3)" = "poly_ri",
                          "Polynomial (Degree 4)" = "poly4_ri"
                        ),
                        "Machine Learning" = c("Neural Network" = "nn_ri",
                                               "Decision Tree" = "tr_ri")
                      )
                    ),
                    numericInput("deviation", "Deviation as max zlog value for the discrete Percentiles:", 5, min = 2, max = 10)
                
                  ))),
                
                column(width = 9,
                       tabsetPanel(
                         tabPanel(
                           "Percentiles", 
                           icon = icon("chart-line"), 
                           
                           fluidRow(
                       
                       box(title = tagList(shiny::icon("chart-line"), "Percentiles"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary",
                           
                           withSpinner(plotOutput("gamlss_prediction", height = "475px"))),
                       
                       box(title = tagList(shiny::icon("table"), "Table"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "info",
                           class = "custom-scrollbox",
                           
                           DT::dataTableOutput("gamlss_tables"))
                       )),
                       
                       tabPanel(
                         "Discrete Percentiles", 
                         icon = icon("chart-line"), 
                         
                         fluidRow(

                       box(title = tagList(shiny::icon("table"), "Discrete Table"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "info",
                           class = "custom-scrollbox",
                           
                           DT::dataTableOutput("gamlss_split"),
                           downloadButton("Download_deviation_gamlss", "Table with discrete Percentiles"),
                           downloadButton("Download_zlog_table_gamlss", "Table with discrete Percentiles for Zlog_AdRI")
                       ),
                       
                       box(title = tagList(shiny::icon("chart-line"), "Discrete Percentiles"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary",
                           
                           withSpinner(plotOutput("gamlss_plot", height = "475px")))
                       
                       ) )))
              )),
      
      ### GAMLSS - Residuals ###
      tabItem(tabName = "residuals",
              
              fluidRow(column(
                width = 3,
                
                fluidRow(
                  box(
                    title = tagList(shiny::icon("gear"), "Settings"),
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    
                    p(gamlsstext3),
                    
                    helpText("Improvement from the GAMLSS by deleting high residuals:"),
                    selectInput("select_model_resi", "Select Model:", choices = list("Splines" = c("P-Splines" = "pb_",
                                                                                                   "Cubic Splines" = "cs_"),
                                                                                     "Polynomial" = c("Polynomial (Degree 3)" = "poly_", 
                                                                                                      "Polynomial (Degree 4)" = "poly4_"),
                                                                                     "Machine Learning" = c("Neural Network" = "nn_", 
                                                                                                            "Decision Tree" = "tr_"))),                
                    numericInput("error", "Maximum value for residuals:", 1.5, min = 0.75, max = 10),
                    actionButton("button_residuals", "Refit the Models", icon("calculator"), onclick = "$(tab_resi).removeClass('disabled')")
                    
                  ))),
                
                column(width = 9,
                       
                       box(title = tagList(shiny::icon("chart-line"), "Plot"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary",
                           
                           withSpinner(plotOutput("outlier", height = "600px"))),
                       
                       box(title = tagList(shiny::icon("chart-line"), "Fitted Plot"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary",
                           
                           withSpinner(plotOutput("gamlss_outlier", height = "600px")),
                           withSpinner(plotOutput("outlier_term_gamlss", height = "475px"))),
                       
                       box(title = tagList(shiny::icon("table"), "Table"),
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "info",
                           class = "custom-scrollbox",
                           
                           p("Tables with the Prediction Tables, can be downloaded and be used like another dataset for this Shiny App!"),
                           DT::dataTableOutput("gamlss_residuals_pb"), 
                           downloadButton("Download_pb_residuals", "GAMLSS with P-Splines"),
                           DT::dataTableOutput("gamlss_residuals_cs"),
                           downloadButton("Download_cs_residuals", "GAMLSS with Cubic Splines"),
                           DT::dataTableOutput("gamlss_residuals_poly"),
                           downloadButton("Download_poly_residuals", "GAMLSS with Polynomial Regression with Degree 3"),
                           DT::dataTableOutput("gamlss_residuals_poly4"),
                           downloadButton("Download_poly4_residuals", "GAMLSS with Polynomial Regression with Degree 4"),
                           DT::dataTableOutput("gamlss_residuals_nn"),
                           downloadButton("Download_nn_residuals", "GAMLSS with Neural Network"),
                           DT::dataTableOutput("gamlss_residuals_tr"),
                           downloadButton("Download_tr_residuals", "GAMLSS with Decision Tree"),
                           tags$script(src = 'tabs_enabled_resi.js'))
              )))
    ))
)

####################################### SERVER ####################################################

server <- function(input, output, session) {

  options(shiny.sanitize.errors = TRUE)
  
  values <- reactiveValues(upload_state = NULL)
  show_more_info <- reactiveVal(FALSE)
  
  ##################################### Reactive Expressions ######################################

  ##################################### Reactive Dataset ##########################################
  ########### Data is subset with the function select_data() with the given age interval ##########
  #################################################################################################
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  dataset_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset_file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$dataset_file <- renderUI({
    input$reset ## Create a dependency with the reset button
    fileInput('dataset_file1', label = NULL)
  })
  
  values_lis <- reactiveValues(
    upload_state_lis = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values_lis$upload_state_lis <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values_lis$upload_state_lis <- 'reset'
  })
  
  # Reset inputs when switching the distribution
  observeEvent(input$distribution, {
    updateTextInput(session, "window_size_truncated", value = "")
    updateTextInput(session, "step_width_truncated", value = "")
    updateTextInput(session, "window_size_triangular", value = "")
    updateTextInput(session, "step_width_triangular", value = "")
    updateTextInput(session, "window_size_trapezoidal", value = "")
    updateTextInput(session, "step_width_trapezoidal", value = "")
    
    if (input$distribution == "truncated_gaussian") {
      updateNumericInput(session, "standard_deviation_truncated", value = 5)
    } else if (input$distribution == "gaussian") {
      updateNumericInput(session, "standard_deviation_gaussian", value = 5)
    } else if (input$distribution == "triangular") {
      updateNumericInput(session, "vertex1", value = 0.5)
    } else if (input$distribution == "trapezoidal") {
      updateNumericInput(session, "vertex1_trap", value = 0.3)
      updateNumericInput(session, "vertex2_trap", value = 0.6)}
  })
  
  dataset_input_lis <- reactive({
    if (is.null(values_lis$upload_state_lis)) {
      return(NULL)
    } else if (values_lis$upload_state_lis == 'uploaded') {
      return(input$dataset_file2)
    } else if (values_lis$upload_state_lis == 'reset') {
      return(NULL)
    }
  })
  
  output$lis_data_file <- renderUI({
    input$reset_lis ## Create a dependency with the reset button
    fileInput('dataset_file2', label = NULL)
  })
  
  data_analyte <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Load data...", detail = "", value = 2)
    
    req(input$dataset, input$age_end)
    
    ri_results <<- data.frame()
    lms_ready <<- FALSE        # To check if the lms method was used
    modelsprediction <<- FALSE # To check if the models are build 
    residuals_ready <<- FALSE  # Check if the residuals are calculated
    
    # Read the data
    file_path <- if (is.null(dataset_input())) {
      input$dataset
    } else {
      dataset_input()[["datapath"]]
    }
    
    data_data <- read.csv2(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "")
    
    ################################### Age is given by days ######################################
    if (input$days_or_years == "age_days") {
      
      # Preprocessing the data
      data_analyte <- select_data(data_data, input$age_input_min, input$age_input, input$sex, "days", input$unique)
    }
    else{
      ################################# Age in years (default) ####################################
      
      # Preprocessing the data
      data_analyte <- select_data(data_data, input$age_end[1] ,input$age_end[2], input$sex, "years", input$unique)
    }
    
    try(if (input$checkboxtukey == TRUE) {
      data_analyte <- outlier.detection(data_analyte)
    })
    
    cat("\n")
    
    data_analyte_short <<- data_analyte
    on.exit(progress$close())
    data_analyte
  })
  
  y.axis.name <- reactive({
    ylab <- paste0(data_analyte()[1,6]," [", input$text_unit,"]")
    ylab
  })
  
  ##################################### Build rpart Decision Tree #################################
  
  build_rpart <- reactive({
    
    input$tree_window_nmin
      
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate decision tree...", detail = "", value = 2)
    
    rpart_ready <- make_rpart(data_analyte(), as.numeric(input$tree_window_cp), as.numeric(input$tree_window_minsplit))
    on.exit(progress$close())
    rpart_ready
  })
  
  ##################################### Reactive Regular Window ###################################
  
  window_reactive <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate RI with regular windows...", detail = "", value = 2)
    
    days <- input$window_age*365
    if (input$window_age <= 1) {
      days <- input$window_agedays}
    
    window_data <- window_method(data_analyte(), days, "reflim", input$window_age_nmin)
    on.exit(progress$close())
    window_data
  })
  
  ##################################### Reactive LIS ##############################################
  
  read_lis_data <- reactive({
    
    if (is.null(dataset_input_lis())) {
      lis_data <- read.delim2(input$lis_data)}
    if (!is.null(dataset_input_lis())) {
      lis_data <- read.delim2(dataset_input_lis()[["datapath"]])}
    lis_data
  })
  
  window_lis <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate RI with the groups from the Laboratory Information System (LIS)...", detail = "", value = 2)
    
    lis <- data.frame(read_lis_data()[,2])
    splits <- data.frame(index = lis)
    lis_split <- round(c(0,sort(splits[,1])))
    
    window_lis <- window_method_lis(data_analyte(), lis_split, "reflim", input$lis_data_nmin)
    on.exit(progress$close())
    window_lis
  })
  
  ##################################### Reactive Sliding Window ###################################
  
  slidingwindowregular <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Sliding Window RI...", detail = "", value = 2)
    
    df <- data_analyte()
    
    on.exit(progress$close())
    
    return(df)
  })
  
  slidingwindow <- reactive({

    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Sliding Window RI...", detail = "", value = 2)
    
    df <- data_analyte()
    
    selected_x <- df$value
    selected_t <- df$age_days

    on.exit(progress$close())
    
    return(list(x = selected_x, t = selected_t))
  })
  
  sliding_data <- reactive({
    
    user_data <- slidingwindow()
    user_x <- user_data$x
    user_t <- user_data$t
    
    window_size <- switch(input$distribution,
                          "truncated_gaussian" = if (!is.null(input$checkboxgaussianfixed) && input$checkboxgaussianfixed) input$window_size_truncated else NULL,
                          "triangular"         = if (!is.null(input$checkboxtriangularfixed) && input$checkboxtriangularfixed) input$window_size_triangular else NULL,
                          "trapezoidal"        = if (!is.null(input$checkboxtrapezoidalfixed) && input$checkboxtrapezoidalfixed) input$window_size_trapezoidal else NULL,
                          NULL
    )
    
    step_width <- switch(input$distribution,
                         "truncated_gaussian" = if (!is.null(input$checkboxgaussianfixed) && input$checkboxgaussianfixed) input$step_width_truncated else NULL,
                         "triangular"         = if (!is.null(input$checkboxtriangularfixed) && input$checkboxtriangularfixed) input$step_width_triangular else NULL,
                         "trapezoidal"        = if (!is.null(input$checkboxtrapezoidalfixed) && input$checkboxtrapezoidalfixed) input$step_width_trapezoidal else NULL,
                         NULL
    )
    
    standard_deviation <- switch(input$distribution, "truncated_gaussian" = input$standard_deviation_truncated, NULL)
    
    vertex1 <- input$vertex1
    vertex1_trap <- input$vertex1_trap
    vertex2_trap <- input$vertex2_trap
    
    result <- tryCatch({
      
      if (input$distribution == "truncated_gaussian") {
        
        res <- reflimR.expand::w.sliding.reflim.plot(
          user_x,
          user_t,
          distribution = input$distribution,
          standard_deviation = standard_deviation,
          window.size = window_size,
          step.width = step_width,
          weight_threshold = input$weight_threshold
        )
      } else if (input$distribution == "triangular") {
        
        res <- reflimR.expand::w.sliding.reflim.plot(
          user_x,
          user_t,
          distribution = input$distribution,
          vertex1 = vertex1,
          window.size = window_size,
          step.width = step_width,
          weight_threshold = input$weight_threshold
        )
      } else if (input$distribution == "trapezoidal") {
        
        res <- reflimR.expand::w.sliding.reflim.plot(
          user_x,
          user_t,
          distribution = input$distribution,
          vertex1 = vertex1_trap,
          vertex2 = vertex2_trap,
          window.size = window_size,
          step.width = step_width,
          weight_threshold = input$weight_threshold
        )
      }
      
      # Compute segment index
      segment_indices <- which(res$x == "---") - 1
      
      if (length(segment_indices) == 0) {
        segment_indices <- nrow(res)
        segment_count <- 1
      } else {
        if (tail(segment_indices, 1) < nrow(res)) {
          segment_indices <- c(segment_indices, nrow(res))
        }
        segment_count <- length(segment_indices)
      }
      
      updateSliderInput(session, "segment", max = segment_count)
      
      list(
        res = res,
        segment_indices = segment_indices,
        segment_count = segment_count,
        error = NULL
      )
      
    }, error = function(e) {
      list(
        res = NULL,
        segment_indices = NULL,
        segment_count = 0,
        error = e$message
      )
    })
    
    return(result)
  })
  
  plot_sliding_data <- reactive({
    
    user_data <- slidingwindow()
    user_x <- user_data$x
    user_t <- user_data$t
    
    window_size <- switch(input$distribution,
                          "truncated_gaussian" = if (!is.null(input$checkboxgaussianfixed) && input$checkboxgaussianfixed) input$window_size_truncated else NULL,
                          "triangular"         = if (!is.null(input$checkboxtriangularfixed) && input$checkboxtriangularfixed) input$window_size_triangular else NULL,
                          "trapezoidal"        = if (!is.null(input$checkboxtrapezoidalfixed) && input$checkboxtrapezoidalfixed) input$window_size_trapezoidal else NULL,
                          NULL
    )
    
    step_width <- switch(input$distribution,
                         "truncated_gaussian" = if (!is.null(input$checkboxgaussianfixed) && input$checkboxgaussianfixed) input$step_width_truncated else NULL,
                         "triangular"         = if (!is.null(input$checkboxtriangularfixed) && input$checkboxtriangularfixed) input$step_width_triangular else NULL,
                         "trapezoidal"        = if (!is.null(input$checkboxtrapezoidalfixed) && input$checkboxtrapezoidalfixed) input$step_width_trapezoidal else NULL,
                         NULL
    )
    
    standard_deviation <- switch(input$distribution, "truncated_gaussian" = input$standard_deviation_truncated, NULL)
    
    vertex1 <- input$vertex1
    vertex1_trap <- input$vertex1_trap
    vertex2_trap <- input$vertex2_trap
    
    result <- tryCatch({
      if (input$distribution == "truncated_gaussian") {
        reflimR.expand::sliding.w.reflim(user_x, user_t, distribution = input$distribution, log.scale = FALSE,
                        standard_deviation = standard_deviation,
                        window.size = window_size,
                        step.width = step_width,
                        weight_threshold = input$weight_threshold)
      } else if (input$distribution == "triangular") {
        reflimR.expand::sliding.w.reflim(user_x, user_t, distribution = input$distribution, log.scale = FALSE,
                        vertex1 = vertex1,
                        window.size = window_size,
                        step.width = step_width,
                        weight_threshold = input$weight_threshold)
      } else if (input$distribution == "trapezoidal") {
        reflimR.expand::sliding.w.reflim(user_x, user_t, distribution = input$distribution, log.scale = FALSE,
                        vertex1 = vertex1_trap, vertex2 = vertex2_trap,
                        window.size = window_size,
                        step.width = step_width,
                        weight_threshold = input$weight_threshold)
      }
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    return(result)
  })
  
  ##################################### Reactive GAMLSS ###########################################
  # make_gamlss() is used with six different smooth additive terms for the GAMLLSS models #########
  # with different distributions. #################################################################
  
  build_gamlss_model <- eventReactive(input$button_gamlss, {
    
    req(input$dataset, input$age_end, input$distribtion_gamlss, 50, "RS")
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Percentiles with GAMLSS-Models...", detail = "", value = 2)

    # Error message
    if (input$checkbox == TRUE) {
      validate(need(lms_ready == TRUE, 
      "Please make first the LMS-Method to get the proposed distribution!"))
      gamlss_model_read <- make_gamlss(data_analyte(), input$age_end[2], lms_$family[1], 100, "RS")} 

    else{gamlss_model_read <- make_gamlss(data_analyte(), input$age_end[2], input$distribtion_gamlss, 100, "RS")}
    
    # Save global value to check later if the models are already calculated
    modelsprediction <<- TRUE
    
    on.exit(progress$close())
  })
  
  ##################################### Reactive LMS ##############################################
  
  lms_reactive <- eventReactive(input$button_lms, {
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Percentiles with LMS...", detail = "", value = 2)
    
    lms_model <- make_lms(data_analyte())
    
    lms_ready <<- TRUE # Value to check if lms is accomplished
    on.exit(progress$close())
  })
  
  ##################################### Reactive GAMLSS - Residuals ###############################
  # Use the outliers_residuals() to delete very high and low residuals and refit the models #######
  
  build_outlier <- eventReactive(input$button_residuals, {
    
    progress <- shiny::Progress$new()
    progress$set(message = "Refit the GAMLSS...", detail = "", value = 2)
    
    outliers_residuals(data_analyte(), input$distribtion_gamlss, 50, "RS", input$error)

    residuals_ready <<- TRUE
    on.exit(progress$close())
  })
  
  ##################################### Information (Current selected dataset) ####################
  
  # helptext <- reactive({
  #   text <- HTML(paste("Selected dataset:", input$dataset))
  #   text
  # })
  
  ##################################### Obeserve Event ############################################
  # Update Slider by Window method with the selected age range from the dataset ###################
  
  observeEvent(input$age_end, {
    if (input$days_or_years == "age_days") { 
      updateSliderInput(session = session, inputId = "window_age", max = round(input$age_end[2]/365 - input$age_end[1]/365))}
    else{updateSliderInput(session = session, inputId = "window_age", max = input$age_end[2] - input$age_end[1])}
  })
  
  ##################################### Output ####################################################
  
  ##################################### Help-Text with the selected dataset #######################
  # output$helptext_gamlss <- renderUI({
  #   helptext()
  # })
  # 
  # output$helptext_window <- renderUI({
  #   helptext()
  # })
  # 
  # output$helptext_prediction <- renderUI({
  #   helptext()
  # })
  # 
  # output$helptext_residuals <- renderUI({
  #   helptext()
  # })

  ##################################### Overview ##################################################
  
  # Scatterplot from the data_analyte() with plotly
  output$scatterplot_plotly <- renderPlotly({
    
      fig <- plot_ly(data_analyte(), x = ~age_days, y = ~value,
                   text = ~ paste('</br>Patient: ', patient,
                                  '</br>Age [Years]: ', age,
                                  '</br>Age [Days]: ', age_days,
                                  '</br>Value: ', value),
                   type = "scatter",
                   symbol = ~sex,
                   symbols = c('circle', 17),
                   color = ~sex,
                   colors = c("indianred", "cornflowerblue"),
                   mode = "markers",
                   marker = list(size = 10)) %>%
          layout(xaxis = list(title = "Age [Days]", titlefont = list(size = 20), tickfont = list(size = 15)),
                 yaxis = list(title = y.axis.name(), titlefont = list(size = 20), tickfont = list(size = 15)))
  })
  
  # Barplot with the distribution of the sex
  output$barplot_sex <- renderPlot({

    if (!(nrow(data_analyte())) == 0) {
      hist_data_w <- subset(data_analyte(), sex == "F", select = age)
      hist_data_m <- subset(data_analyte(), sex == "M", select = age)
  
      hist_w <- hist(hist_data_w$age, breaks = seq(min(data_analyte()[,3]) - 1,max(data_analyte()[,3]),by = 1))$counts
      hist_m <- hist(hist_data_m$age, breaks = seq(min(data_analyte()[,3]) - 1,max(data_analyte()[,3]),by = 1))$counts
  
      barplot(rbind(hist_m,hist_w), col = c("cornflowerblue","indianred"),
            names.arg = seq(min(data_analyte()[,3]), max(data_analyte()[,3]), by = 1), xlab = "Age", las = 1, beside = TRUE, ylab = "Number of data")
      abline(h = 0)
      legend("topright", legend = c(paste0("Men: ", nrow(hist_data_m)), paste0("Female: ", nrow(hist_data_w))), col = c("cornflowerblue","indianred"), pch = c(17, 20))
    
      par(new = TRUE)
      boxplot(data_analyte()[,3], horizontal = TRUE, axes = FALSE, col = rgb(0, 0, 0, alpha = 0.15))
    }
  })
  
  output$barplot_value <- renderPlot({
    
    if (!(nrow(data_analyte())) == 0) {
      
      if (input$sex == "m") {
        boxplot(data_analyte()[,5]~interaction(data_analyte()[,2], data_analyte()[,3]), xlab = "Age", 
                ylab = y.axis.name(), col = "cornflowerblue", las = 2)
      }
      else if (input$sex == "f") {
        boxplot(data_analyte()[,5]~interaction(data_analyte()[,2], data_analyte()[,3]), xlab = "Age", 
                ylab = y.axis.name(), col = "indianred", las = 2)
      } else{
        boxplot(data_analyte()[,5]~interaction(data_analyte()[,2], data_analyte()[,3]), xlab = "Age", 
                ylab = y.axis.name(), col = c("indianred", "cornflowerblue"), las = 2)
      }
    }
  })

  # Data-Table
  output$datatable <- DT::renderDataTable({
    
    data_table <- data_analyte()
    colnames(data_table) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "ANALYTE")
    
    DT::datatable(data_table, extensions = 'Buttons', rownames = FALSE, 
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')),
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Overview from Dataset'))
  })
  
  ##################################### Window-Methods ############################################
  
  # Regular Window method
  output$window <- renderPlot({
    
    result <- window_reactive()
    window_data <- result$window_data
    
    plot(value~age_days, data = data_analyte(), pch = 20, cex = 1, col = "grey", xlab = "Age [Days]",
           ylab = y.axis.name(), cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
    x_lower <- window_data$age_days
    y_lower <- window_data$quantile1
      
    segments(x_lower[-length(x_lower)],y_lower[-length(x_lower)],x_lower[-1],y_lower[-length(x_lower)])
    lowerlimit <- data.frame(x = x_lower, y = y_lower)

    x_upper <- window_data$age_days
    y_upper <- window_data$quantile2
      
    segments(x_upper[-length(x_upper)],y_upper[-length(x_upper)],x_upper[-1],y_upper[-length(x_upper)])
    upperlimit <- data.frame(x = x_upper, y = y_upper)
      
    for (i in 1:(nrow(lowerlimit) - 1)) {
        
      age <- c(lowerlimit$x[i + 1], lowerlimit$x[i], lowerlimit$x[i], lowerlimit$x[i + 1])
        
      lowerlimit_polygon <- c(lowerlimit$y[i], lowerlimit$y[i])
      upperlimit_polygon <- c(upperlimit$y[i], upperlimit$y[i])
      if (length(lowerlimit_polygon > 1)) {
        polygon(age, c(upperlimit_polygon[2], upperlimit_polygon[1], lowerlimit_polygon[1], lowerlimit_polygon[2]), 
            col = rgb(red = 0 , green = 0, blue = 0, alpha = 0.25), border = NA)
      }
    }
  })
  
  # Tables to the regular windowmethod - with reflimR
  output$windowtable_t <- DT::renderDataTable({
    
    result <- window_reactive()
    window_data_all <- result$window_data_all
    
    DT::datatable(window_data_all, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Regular Window-Method with reflimR')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "seagreen") %>% 
      DT::formatRound(c(3:length(window_data_all)), 2)
  })
  
  
  # LIS-Method
  output$lis_window <- renderPlot({
    
    result <- window_lis()
    window_data <- result$window_data
    
    plot(value~age_days, data = data_analyte(), pch = 20, cex = 1, col = "grey", 
         xlab = "Age [Days]", ylab = y.axis.name(), cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
    
    x_lower <- window_data$age_days
    y_lower <- window_data$quantile1
    
    segments(x_lower[-length(x_lower)],y_lower[-length(x_lower)],x_lower[-1],y_lower[-length(x_lower)])
    lowerlimit <- data.frame(x = x_lower, y = y_lower)
    
    x_upper <- window_data$age_days
    y_upper <- window_data$quantile2
    
    segments(x_upper[-length(x_upper)],y_upper[-length(x_upper)],x_upper[-1],y_upper[-length(x_upper)])
    upperlimit <- data.frame(x = x_upper, y = y_upper)
    
    for (i in 1:(nrow(lowerlimit) - 1)) {
      
      age <- c(lowerlimit$x[i + 1], lowerlimit$x[i], lowerlimit$x[i], lowerlimit$x[i + 1])
      
      lowerlimit_polygon <- c(lowerlimit$y[i], lowerlimit$y[i])
      upperlimit_polygon <- c(upperlimit$y[i], upperlimit$y[i])
      if (length(lowerlimit_polygon > 1)) {
        polygon(age, c(upperlimit_polygon[2], upperlimit_polygon[1], 
                       lowerlimit_polygon[1], lowerlimit_polygon[2]), 
                col = rgb(red = 0 , green = 0, blue = 0, alpha = 0.25), border = NA)
      }
    }
  })
  
  # Window-Method coupled to the LIS-Method - Analysis for Normal- and Lognormaldistribution
  output$lis_window_analysis <- renderPlot({
    
    lis <- data.frame(read_lis_data()[,2])
    splits <- data.frame(index = lis)
    lis_split <- round(c(0,sort(splits[,1])))
 
    par(mfrow = c(1,length(lis_split) - 1))
    
    window_method_lis(data_analyte(), lis_split, "reflim", input$lis_data_nmin, TRUE)
  })
  
  # Tables to the Window-method coupled to the LIS-Method - with reflimR
  output$lis_table_t <- DT::renderDataTable({
    
    result <- window_lis()
    window_data_all <- result$window_data_all
    
    DT::datatable(window_data_all, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Laboratory Information System (LIS)-Method with reflim()')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "seagreen") %>% 
      DT::formatRound(c(3:length(window_data_all)), 2)
  })
  
  # Window-Method coupled to a Decision Tree
  output$tree_window <- renderPlot({
    
    # Build Decision Tree
    df <- data_analyte()
    sex_age_rpart <- build_rpart()
    df$leaf <- sex_age_rpart$where
    
    gr <- ggplot(df, aes(x = age_days, y = value, color = sex)) + geom_point() + theme_bw() + 
      labs(x = 'Age (days)', y = y.axis.name()) + scale_color_manual(values = c("F" = "indianred", "M" = "cornflowerblue"))

    gr <- draw_ri_rpart(df, gr, sex = "M", is_unisex = TRUE, reflim_n.min = input$tree_window_nmin)
    gr <- draw_ri_rpart(df, gr, sex = "M", is_unisex = FALSE, reflim_n.min = input$tree_window_nmin)
    gr <- draw_ri_rpart(df, gr, sex = "F", is_unisex = FALSE, reflim_n.min = input$tree_window_nmin)

    print(gr)    
  })
  
  # Plot used Decision Tree
  output$tree_rpart <- renderPlot({
    
    rpart.plot(build_rpart(), box.palette = "RdBu", roundint = FALSE)
    #rsq.rpart(build_rpart())
  })
  
  output$sliding_window <- renderPlot({
    
    df <- slidingwindowregular()
    x <- df$value
    covariate <- df$age_days
    
    if (input$checkboxregularfixed) {
      
      result <- reflimR.expand::sliding.reflim(x, covariate, window.size = input$regular_window_size_truncated,
        step.width = input$regular_step_width_truncated)
      
      reflimR.expand::draw.sliding.reflims(result, xlim = c(0, max(covariate) + max(covariate)*0.1), 
                                           ylim = c(0, max(x) + max(x)*0.1), xlab = "Age [Days]", ylab = y.axis.name(),
                                           col.low = c(0.392,0.620,0.929), col.upp = c(0.804,0.361,0.361))
    } else {
      
      result <- reflimR.expand::sliding.reflim(x, covariate, n.min.window = input$slding_window_nmin.window)
      
      reflimR.expand::draw.sliding.reflims(result, xlim = c(0, max(covariate) + max(covariate)*0.1), 
                                           ylim = c(0, max(x) + max(x)*0.1), xlab = "Age [Days]", ylab = y.axis.name(),
                                           col.low = c(0.392,0.620,0.929), col.upp = c(0.804,0.361,0.361))
      
    }
  })
  
  output$sliding_scatterPlot <- renderPlot({
    alistplot <- plot_sliding_data()
    
    if (is.null(alistplot)) {
      return(NULL)
    }
    
    alistplot + ggtitle("Reference Limits") + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$sliding_scatterPlot2 <- renderPlot({
    data_info <- sliding_data()
    
    if (!is.null(data_info$error)) {
      return(NULL)
    }
    
    res <- data_info$res
    
    segment_indices <- data_info$segment_indices
    segment_count <- data_info$segment_count
    
    segment_index <- input$segment
    
    if (segment_index == 1) {
      start_row <- 1
    } else {
      start_row <- segment_indices[segment_index - 1] + 2
    }
    end_row <- segment_indices[segment_index]
    
    segment_data <- res[start_row:end_row, ]
    
    calculated_window_size <- max(as.numeric(segment_data$t)) - min(as.numeric(segment_data$t))
    calculated_step_width <- if (segment_count > 1 && segment_index > 1) {
      current_start_time <- min(as.numeric(segment_data$t))
      prev_start_time <- min(as.numeric(res[if (segment_index == 2) 1 else segment_indices[segment_index - 2] + 2, "t"]))
      current_start_time - prev_start_time
    } else 0
    
    w_values <- as.numeric(segment_data$w)
    total_w <- sum(w_values, na.rm = TRUE)
    
    # Creating a continuous color gradient with colorRampPalette()
    color_palette <- colorRampPalette(c("cornflowerblue", "indianred"))(100)
    w_colors <- color_palette[cut(w_values, breaks = 100, labels = FALSE)]
    
    plot(as.numeric(segment_data$t), as.numeric(segment_data$x), xlab = "Age [Days]", ylab = y.axis.name(),
         main = paste("Scatter Plot for Segment", segment_index, "with Distribution:", input$distribution), pch = 16, col = w_colors)
    
    legend("topright", legend = c("w = 1", "w = 0.8", "w = 0.5"), col = color_palette[c(100, 80, 50)],
           pch = 16, title = "Weighting", xpd = TRUE, bty = "n")
  })
  
  output$weightDistPlot <- renderPlot({
    
    data_info <- sliding_data()
    
    if (!is.null(data_info$error)) {
      return(NULL)
    }
    
    res <- data_info$res
    segment_indices <- data_info$segment_indices    # the end of each segment
    segment_count <- data_info$segment_count        # the number of segments
    
    segment_index <- input$segment
    
    if (segment_index == 1) {
      start_row <- 1
    } else {
      start_row <- segment_indices[segment_index - 1] + 2
    }
    end_row <- segment_indices[segment_index]
    
    segment_data <- res[start_row:end_row, ]
    interval_cov <- as.numeric(segment_data$t)
    www <- as.numeric(segment_data$w)
    www_sum <- sum(www, na.rm = TRUE)
    
    plot(interval_cov, www, type = "l", col = "cornflowerblue",
         lwd = 2,  main = paste("Weight Distribution - Segment", segment_index), xlab = "Interval Covariate", ylab = "Weight")
    points(interval_cov, www, col = "indianred")
    
    legend("topright", legend = paste("Total weight:", round(www_sum, 2)), col = "black", xpd = TRUE, bty = "n")
  })
 
  ################################ LMS #############################################
  
  # LMS-Percentile Plot
  output$lms <- renderPlot({
    
    lms_reactive()
    centiles(lms_, cent = c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1, 
             col.cent = c("indianred","black","cornflowerblue"), lty.centiles = c(3,1,3),lwd.centiles = 2, 
             legend = FALSE, col = "lightgrey")
  })

  # Analysis LMS
  output$lms_plot <- renderPlot({
    
    lms_reactive()
    plot(lms_, parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
  
  # Analysis LMS
  output$lms_fitted <- renderPlot({
    
    lms_reactive()
    new_lms_data <- data.frame(value_lms = data_analyte()[[5]], age_lms = data_analyte()[[4]])
    fittedPlot(lms_ ,x = new_lms_data$age_lms, xlab = "Age [Days]")
  })
  
  # Analysis LMS-Text
  output$lms_text <- renderPrint({
    
    lms_reactive()
    print(lms_) 
    cat("Power:")
    print(lms_$power) 
    cat("\n")
    centiles(lms_, cent = c(2.5,50,97.5), plot=FALSE)
  })
  
  # Wormplots LMS
  output$lms_wormplots <- renderPlot({
    lms_reactive()
    try(wp(lms_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
  })

  ##################################### GAMLSS ####################################################
  
  output$all_gamlss <- renderPlot({
    centiles.com(pb_, cs_, poly_, poly4_, nn_, tr_, cent = c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), 
                  legend = TRUE, main = "GAMLSS")
  })
  
  output$buttons_gamlss <- renderUI({
    build_gamlss_model()
    print("*** Your GAMLSS are ready! ***")
  })
  
  output$buttons_lms <- renderUI({
    lms_reactive()
    print("*** Your LMS model is ready! ***")
  })
  
  # Centiles Plot with gamlss (P-Splines, Cubic Splines) ######
  output$gamlss_models_splines <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(1,2))
    
    centiles(pb_, main = "GAMLSS with P-Splines", cent = c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = y.axis.name(), pch = 20, cex = 1, col.cent = c("indianred","black","cornflowerblue"), 
             lty.centiles = c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
    centiles(cs_, main = "GAMLSS with Cubic Splines", cent = c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = y.axis.name(), pch = 20, cex = 1, col.cent = c("indianred","black","cornflowerblue"), 
             lty.centiles = c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })
  
  # Centiles Plot with gamlss (Polynomials Degree 3 and 4) ######
  output$gamlss_models_poly <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(1,2))
 
    centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent = c(2.5,50,97.5), xlab = "Age [Days]",
             ylab = y.axis.name(), pch = 20, cex = 1, col.cent = c("indianred","black","cornflowerblue"), 
             lty.centiles = c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
    centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent = c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = y.axis.name(), pch = 20, cex = 1, col.cent = c("indianred","black","cornflowerblue"), 
             lty.centiles = c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })
  
  # Plot fitted models for P-Splines 
  output$gamlss_fitted_pb_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(pb_, x = data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Cubic Splines
  output$gamlss_fitted_cs_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(cs_, x = data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Polynomials Degree 3
  output$gamlss_fitted_poly_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(poly_, x = data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Polynomials Degree 4
  output$gamlss_fitted_poly4_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(poly4_, x = data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_psplines <- renderPrint({
     
    build_gamlss_model()
    suppressWarnings({summary(pb_)})
    cat("\n")
    centiles(pb_, cent = c(2.5,50,97.5), plot = FALSE)
    
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_splines <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(cs_)})
    cat("\n")
    centiles(cs_, cent = c(2.5,50,97.5), plot = FALSE)
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_poly <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(poly_)})
    cat("\n")
    centiles(poly_,cent = c(2.5,50,97.5), plot = FALSE)
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_poly4 <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(poly4_)})
    cat("\n")
    centiles(poly4_, cent = c(2.5,50,97.5), plot = FALSE)
  })
  
  # Plot the changed terms for P-Splines
  output$gamlss_term_pb <- renderPlot({

    build_gamlss_model()
    try(plot(pb_, parameters = par(mfrow = c(2,2), mar = par("mar") +
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })
  
  # Plot the changed terms for Cubic Splines
  output$gamlss_term_cs <- renderPlot({
    
    build_gamlss_model()
    try(plot(cs_, parameters = par(mfrow = c(2,2), mar = par("mar") +
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })
  
  # Plot the changed terms for Polynomial Degree 3
  output$gamlss_term_poly <- renderPlot({
    
    build_gamlss_model()
    try(plot(poly_, parameters = par(mfrow = c(2,2), mar = par("mar") +
                                 c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                 col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))) 
  })
  
  # Plot the changed terms for Polynomial Degree 4
  output$gamlss_term_poly4 <- renderPlot({
    
    build_gamlss_model()
     try(plot(poly4_, parameters = par(mfrow = c(2,2), mar = par("mar") +
                                  c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                  col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))) 
  })
  
  # Wormplots from P-Splines and Cubic Splines
  output$wormplots_splines <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(1,2))
    
    try(wp(pb_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
    try(wp(cs_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
  })
  
  # Wormplots from Polynomials Degree 3 and 4
  output$wormplots_ploy <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(1,2))
    
    try(wp(poly_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
    try(wp(poly4_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
  })
  
  # Neural Network (machine learning) #############################################################
  
  # Centiles Plot with the Neural Network 
  output$gamlss_net <- renderPlot({
    
    build_gamlss_model()
    centiles(nn_, main = "GAMLSS with Neural Network", cent = c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
             col.cent = c("indianred","black","cornflowerblue"), lty.centiles = c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })

  # Neural Network - Analysis
  output$network_term <- renderPlot({
    
    build_gamlss_model()
    try(plot(nn_, parameters = par(mfrow = c(2,2), mar = par("mar") +
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  }) 

  # Neural Network - Analysis
  output$network_fitted <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(nn_, x = data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Neural Network - Analysis
  output$net_text <- renderPrint({
    
    build_gamlss_model()
    suppressWarnings({summary(nn_)})
    centiles(nn_,cent = c(2.5,50,97.5), plot = FALSE)
  })
  
  #Wormplots from the Neural Network
  output$nn_wormplots <- renderPlot({
    
    build_gamlss_model()
    try(wp(nn_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
  })
  
  # Decision Tree #################################################################################
  
  # Centiles Plot with the Decision Tree
  output$gamlss_tree <- renderPlot({
    
    build_gamlss_model()
    
    centiles(tr_, main = "GAMLSS with Decision Tree", cent = c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
             col.cent = c("indianred","black","cornflowerblue"), lty.centiles = c(3,1,3), lwd.centiles = 2, legend = FALSE, col = "lightgrey")
  })
  
  # Decision Tree - Analysis
  output$tree_term <- renderPlot({
    
    build_gamlss_model()
    try(plot(tr_, parameters = par(mfrow = c(2,2), mar = par("mar") +
                              c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })

  # Decision Tree - Analysis
  output$tree_fitted <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(nn_, x = data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Decision Tree - Analysis
  output$tree_text <- renderPrint({
    
    build_gamlss_model()
    print(getSmo(tr_))
    suppressWarnings({summary(tr_)})
    centiles(tr_, cent = c(2.5,50,97.5), plot = FALSE)
  })

  # Plotted Decision Tree
  output$rpart_tree <- renderPlot({
    
    build_gamlss_model()
    rpart.plot(getSmo(tr_), roundint = FALSE, box.palette = "RdBu")
  })
  
  # Wormplots from Decision Tree
  output$tr_wormplots <- renderPlot({
    
    build_gamlss_model()
    try(wp(tr_, ylim.all = 3, col = "cornflowerblue", n.inter = 9))
  })
  
  # Comparism #####################################################################################
  
  # Comparison Table for all GAMLSS and LMS
  output$table_compare <- DT::renderDataTable({
  
    if (lms_ready == TRUE) { 
      build_gamlss_model()
      lms_reactive()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(pb_,cs_,poly_, poly4_, nn_,tr_, lms_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(pb_,cs_,poly_, poly4_, nn_,tr_,lms_, k = 3))
      GAIC_$model <- rownames(GAIC_)
      colnames(GAIC_) <- c("GAIC.df","GAIC","model")
      rownames(GAIC_) <- c()
      
      # Bayesian Information Criterion (BIC)
      BIC_ <- data.frame(BIC(pb_,cs_,poly_, poly4_, nn_,tr_, lms_))
      BIC_$model <- rownames(BIC_)
      colnames(BIC_) <- c("BIC.df","BIC","model")
      rownames(BIC_) <- c()
      
      # Pseudo R-squared (R^2)
      R_2 <- data.frame(model = c("pb_","cs_","poly_","poly4_","nn_","tr_","lms_"), 
                        R2 = c(Rsq(pb_), Rsq(cs_),Rsq(poly_), Rsq(poly4_), Rsq(nn_), Rsq(tr_), Rsq(lms_)))
      
      # Merge the Metrics
      compare_models <- merge(AIC_,GAIC_,by = c("model"))
      compare_models <- merge(compare_models,BIC_,by = c("model"))
      compare_models <- merge(compare_models,R_2,by = c("model"))
      
      compare_models["df"] <- c()
      compare_models["GAIC.df"] <- c()
      compare_models["BIC.df"] <- c()
      
      compare_models$model[compare_models$model == "nn_"] <- "Neural Network"
      compare_models$model[compare_models$model == "cs_"] <- "Cubic Splines"
      compare_models$model[compare_models$model == "poly_"] <- " Polynomials (Degree 3) "
      compare_models$model[compare_models$model == "pb_"] <- "P-Splines"
      compare_models$model[compare_models$model == "tr_"] <- "Decision Tree"
      compare_models$model[compare_models$model == "poly4_"] <- " Polynomials (Degree 4)"
      compare_models$model[compare_models$model == "lms_"] <- "LMS"}
    
    if (lms_ready == FALSE) {
      
      build_gamlss_model()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(pb_,cs_,poly_, poly4_, nn_,tr_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(pb_,cs_,poly_, poly4_, nn_,tr_, k = 3))
      GAIC_$model <- rownames(GAIC_)
      colnames(GAIC_) <- c("GAIC.df","GAIC","model")
      rownames(GAIC_) <- c()
      
      # Bayesian Information Criterion (BIC)
      BIC_ <- data.frame(BIC(pb_,cs_,poly_, poly4_, nn_,tr_)) 
      BIC_$model <- rownames(BIC_)
      colnames(BIC_) <- c("BIC.df","BIC","model")
      rownames(BIC_) <- c()
      
      # Pseudo R-squared (R^2)
      R_2 <- data.frame(model = c("pb_","cs_","poly_","poly4_","nn_","tr_"), 
                        R2 = c(Rsq(pb_), Rsq(cs_),Rsq(poly_), Rsq(poly4_), Rsq(nn_), Rsq(tr_)))
      # Error in solve.default(oout$hessian) : 
      #   Lapack routine dgesv: system is exactly singular: U[4,4] = 0
      
      # Merge the Metrics
      compare_models <- merge(AIC_,GAIC_,by = c("model"))
      compare_models <- merge(compare_models,BIC_,by = c("model"))
      compare_models <- merge(compare_models,R_2,by = c("model"))
      compare_models["df"] <- c()
      compare_models["GAIC.df"] <- c()
      compare_models["BIC.df"] <- c()
      
      compare_models$model[compare_models$model == "nn_"] <- "Neural Network"
      compare_models$model[compare_models$model == "cs_"] <- "Cubic Splines"
      compare_models$model[compare_models$model == "poly_"] <- " Polynomials (Degree 3)"
      compare_models$model[compare_models$model == "pb_"] <- "P-Splines"
      compare_models$model[compare_models$model == "tr_"] <- "Decision Tree"
      compare_models$model[compare_models$model == "poly4_"] <- " Polynomials (Degree 4)"}
    
    compare_models <- compare_models
    
    if (residuals_ready == TRUE) {
     
      build_gamlss_model()
      build_outlier()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(opb_,ocs_,opoly_, opoly4_, onn_,otr_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(opb_,ocs_,opoly_, opoly4_, onn_,otr_, k = 3))
      GAIC_$model <- rownames(GAIC_)
      colnames(GAIC_) <- c("GAIC.df","GAIC","model")
      rownames(GAIC_) <- c()
    
      # Bayesian Information Criterion (BIC)
      BIC_ <- data.frame(model = c("opb_","ocs_","opoly_","opoly4_","onn_","otr_"),
                        BIC = c(BIC(opb_), BIC(ocs_),BIC(opoly_), BIC(opoly4_), BIC(onn_), BIC(otr_)))
      
    
      # Pseudo R-squared (R^2)
      R_2 <- data.frame(model = c("opb_","ocs_","opoly_","opoly4_","onn_","otr_"),
                        R2 = c(Rsq(opb_), Rsq(ocs_),Rsq(opoly_), Rsq(opoly4_), Rsq(onn_), Rsq(otr_)))
      
      # Merge the Metrics
      compare_models_residuals <- merge(AIC_,GAIC_,by = c("model"))
      compare_models_residuals <- merge(compare_models_residuals,BIC_,by = c("model"))
      compare_models_residuals <- merge(compare_models_residuals,R_2,by = c("model"))
      
      compare_models_residuals["df"] <- c()
      compare_models_residuals["GAIC.df"] <- c()
      compare_models["BIC.df"] <- c()
      
      compare_models_residuals$model[compare_models_residuals$model == "onn_"] <- "Neural Network (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "ocs_"] <- "Cubic Splines (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "opoly_"] <- " Polynomials (Degree 3) (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "opb_"] <- "P-Splines (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "otr_"] <- "Decision Tree (refit)"
      compare_models_residuals$model[compare_models_residuals$model == "opoly4_"] <- " Polynomials (Degree 4) (refit)"
      
      compare_models <- rbind(compare_models, compare_models_residuals)
    }
    
    # Round the data
    compare_models <- round_df(compare_models, 3)
    
    row_smallest_aic <- compare_models[which.min(compare_models$AIC),]$AIC
    row_smallest_gaic <- compare_models[which.min(compare_models$GAIC),]$GAIC
    row_smallest_bic <- compare_models[which.min(compare_models$BIC),]$BIC
    biggest_r2 <- compare_models[which.max(compare_models$R2),]$R2
    
    compare_models <<- compare_models
  
    DT::datatable(compare_models, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print'))) %>%
      DT::formatStyle(columns = "AIC", background = styleEqual(row_smallest_aic, "cornflowerblue")) %>%
      DT::formatStyle(columns = "GAIC", background = styleEqual(row_smallest_gaic, "indianred")) %>%
      DT::formatStyle(columns = "BIC", background = styleEqual(row_smallest_bic, "seagreen")) %>%
      DT::formatStyle(columns = "R2", background = styleEqual(biggest_r2, "lavender")) 
  })
  
  # Plot with the Metrics (AIC, GAIC, BIC and R^2)
  output$metrics <- renderPlot({
    
    build_gamlss_model()
    if (lms_ready == TRUE) {lms_reactive()}
    
    par(mar = c(12, 3, 3, 3))
    par(mfrow = c(1,2))

    barplot(rbind(compare_models[,2],compare_models[,3],compare_models[,4]), ylab = "Value", 
            ylim = c(min(rbind(compare_models[,2],compare_models[,3],compare_models[,4])) - min(rbind(compare_models[,2],compare_models[,3],compare_models[,4])/100), 
                     max(rbind(compare_models[,2],compare_models[,3],compare_models[,4])) + max(rbind(compare_models[,2],compare_models[,3],compare_models[,4])/100)),
            xpd = FALSE, beside = TRUE, las = 2, names.arg = c(compare_models[,1]), col = c("cornflowerblue","indianred","seagreen3"))
    legend("topright", legend = c("AIC","GAIC","BIC"),
           col = c("cornflowerblue","indianred","seagreen3"), pch = 20)
    
    barplot(compare_models[,5], ylab = "Value",  ylim = c(0, 1), las = 2,
         names.arg = c(compare_models[,1]), col = c("lavender"))
    legend("topright", legend = c("Pseudo R^2"),col = c("lavender"), pch = 20)
  })
  
  ##################################### Prediction ################################################
  
  # Predict new values with the fitted models
  output$gamlss_prediction <- renderPlot({
    
    build_gamlss_model()
    
    if (input$select_model == "lms_ri") {
      if (lms_ready == TRUE) {
        lms_reactive()
        
        # Create new x_values with all possible days in the age range
        data_subset <- data_analyte()
        subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
        x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by = 1)
        
        lms_ri <<- centiles.pred(lms_, xname = "age_lms", xvalues = x_values, cent = c(2.5,50,97.5))}
      else{validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
        stop()}}

    # Create new x_values with all possible days in the age range
    data_subset <- data_analyte()
    subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
    x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by = 1)
    
    pb_ri <<- centiles.pred(pb_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    cs_ri <<- centiles.pred(cs_, xname="age_days",  xvalues=x_values, cent = c(2.5,50,97.5))
    poly_ri <<- centiles.pred(poly_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    poly4_ri <<- centiles.pred(poly4_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    nn_ri <<- centiles.pred(nn_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    tr_ri <<- centiles.pred(tr_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    
    model <- eval(parse(text = input$select_model))
    
    plot(model$x, model$`2.5`, xlab = "Age [Days]", ylab = y.axis.name(), cex = 0.5, lty = 3, type = "l", col = "indianred", ylim = c(0,max(model$`97.5`)))
    lines(model$x, model$`50`, cex = 0.5, lty = 1, col = "black")
    lines(model$x, model$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
  })

  # Tables of the predicted values ################################################################
  
  # Table for prediction
  output$gamlss_tables <- DT::renderDataTable({
    
    build_gamlss_model()
    
    if (input$select_model == "lms_ri") {
      if (lms_ready == TRUE) {
        lms_reactive()
        
        # Create new x_values with all possible days in the age range
        data_subset <- data_analyte()
        subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
        x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by = 1)
        
        lms_ri <<- centiles.pred(lms_, xname = "age_lms", xvalues = x_values, cent = c(2.5,50,97.5))}
      else{validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
        stop()}}
    
    model <- eval(parse(text = input$select_model))
    
    if (exists(input$select_model)) {
      table_prediction <- data.frame(model)
      colnames(table_prediction) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
      DT::datatable(table_prediction, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the Reference Intervals')) %>%
      DT::formatRound(c(2:4), 2)}
  })
  
  # # Tool to predict reference intervals for the different gamlss
  # output$prediction_gamlss <- renderText({
  #   
  #   if(input$select_model == "lms_ri"){text_model <- "LMS"}
  #   if(input$select_model == "pb_ri"){text_model <- "P-Splines"}
  #   if(input$select_model == "cs_ri"){text_model <- "Cubic Splines"}
  #   if(input$select_model == "poly_ri"){text_model <- " Polynomials (Degree 3)"}
  #   if(input$select_model == "poly4_ri"){text_model <- "Polynomials (Degree 4)"}
  #   if(input$select_model == "tr_ri"){text_model <- "Decision Tree"}
  #   if(input$select_model == "nn_ri"){text_model <- "Neural Network"}
  #  
  #   if(modelsprediction == TRUE){
  #     if(input$select_model == "lms_ri" && lms_ready == FALSE){
  #       prediction_text <- "First you need to make LMS Models, than you can predict with these!"
  #     }
  #   
  #     else{
  #       value <- eval(parse(text = input$select_model))[input$prediction_age+1,]
  #       colnames(value) <- c("Age [Days]", "2.5% Percentile", "50% Prcentile", "97.5% Percentile")
  #       prediction_text <- paste("The Reference Intervals for", input$prediction_age, "days for the dataset", 
  #                                input$dataset, "is for the GAMLSS Model", text_model,":", round(value[1,2], digits = 2),
  #                                "(2.5% Percentil) to", round(value[1,4], digits = 2),
  #                                "(97.5% Percentil) with the Median (50% Percentil):", round(value[1,3], digits = 2))}}
  #   else{prediction_text <- "First you need to make GAMLSS, than you can predict with these!"}
  # 
  #   prediction_text
  # })

  # Tables with the discrete values from the predicted GAMLSS #####################################
  output$gamlss_split <- DT::renderDataTable({
    
    if (input$select_model == "pb_ri") {text_model <- "P-Splines"}
    if (input$select_model == "cs_ri") {text_model <- "Cubic Splines"}
    if (input$select_model == "poly_ri") {text_model <- " Polynomials (Degree 3)"}
    if (input$select_model == "poly4_ri") {text_model <- "Polynomials (Degree 4)"}
    if (input$select_model == "tr_ri") {text_model <- "Decision Tree"}
    if (input$select_model == "nn_ri") {text_model <- "Neural Network"}
    
    build_gamlss_model()
    
    deviation_gamlss <- split_gamlss(eval(parse(text = input$select_model)), input$deviation)
    deviation_gamlss <- rbind(0, deviation_gamlss, max(eval(parse(text = input$select_model))))
    
    mean_gamlss <- data.frame()
    gamlss_2_5 <- data.frame()
    gamlss_97_5 <- data.frame()
    model <- eval(parse(text = input$select_model))
      
    for (i in seq(2,nrow(deviation_gamlss))) {
      
      # The data subset 
      age_data <- subset(model, model$x <= deviation_gamlss[i,])  
      age_data_ready <- subset(age_data, age_data$x > deviation_gamlss[i - 1,])  # Below the lowest condition
    
      mean_gamlss_ <- mean(age_data_ready$`50`) 
      mean_gamlss <- rbind(mean_gamlss,mean_gamlss_) 
      
      gamlss_2_5_ <- mean(age_data_ready$`2.5`) 
      gamlss_2_5 <- rbind(gamlss_2_5,gamlss_2_5_) 
      
      gamlss_97_5_ <- mean(age_data_ready$`97.5`) 
      gamlss_97_5 <- rbind(gamlss_97_5,gamlss_97_5_) 
    }
    
    if (lms_ready == TRUE) {
      
      mean_gamlss <- data.frame()
      gamlss_2_5 <- data.frame()
      gamlss_97_5 <- data.frame()
      
      lms_reactive()
      
      # Create new x_values with all possible days in the age range
      data_subset <- data_analyte()
      subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
      x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by = 1)
      
      lms_ri <<- centiles.pred(lms_, xname = "age_lms", xvalues = x_values, cent = c(2.5,50,97.5))
      
      model <- eval(parse(text = "lms_ri"))
      
      for (i in seq(2, nrow(deviation_gamlss))) {
        
        # The data subset 
        age_data <- subset(model, model$x <= deviation_gamlss[i,])  
        age_data_ready <- subset(age_data, age_data$x > deviation_gamlss[i - 1,])  # Below the lowest condition
        
        mean_gamlss_ <- mean(age_data_ready$`50`) 
        mean_gamlss <- rbind(mean_gamlss,mean_gamlss_) 
        
        gamlss_2_5_ <- mean(age_data_ready$`2.5`) 
        gamlss_2_5 <- rbind(gamlss_2_5,gamlss_2_5_) 
        
        gamlss_97_5_ <- mean(age_data_ready$`97.5`) 
        gamlss_97_5 <- rbind(gamlss_97_5,gamlss_97_5_) 
      }
    }
    
    deviation_gamlss <- cbind(head(deviation_gamlss,-1), tail(deviation_gamlss,-1), head(deviation_gamlss,-1)/365, tail(deviation_gamlss,-1)/365,
                              gamlss_2_5, gamlss_97_5)
    colnames(deviation_gamlss) <- c("Age-range from", "to [Days]","Age from", "to [Years]",
                                    "2.5% Percentil","97.5% Percentil")
    deviation_gamlss <<- deviation_gamlss
    
    if (input$select_model == "lms_ri" && lms_ready == FALSE) {
      validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
    }
    
    DT::datatable(deviation_gamlss, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the GAMLSS')) %>%
    DT::formatRound(c(3:length(deviation_gamlss)), 2)
  })
  
  # Plot for the discrete percentiles from the GAMLSS-models
  output$gamlss_plot <- renderPlot({
    
    input$deviation
    input$select_model
    
    build_gamlss_model()
   
    if (input$select_model == "lms_ri") {
      if (lms_ready == TRUE) {
        lms_reactive()
        
        # Create new x_values with all possible days in the age range
        data_subset <- data_analyte()
        subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
        x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by = 1)
        
        lms_ri <<- centiles.pred(lms_, xname="age_lms", xvalues=x_values, cent = c(2.5,50,97.5))}
      else{validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
        stop()}}
    
    plot(value~age_days, data = data_analyte(), pch = 20, cex = 1, col = "grey", xlab = "Age [Days]",
         ylab = y.axis.name(), cex.lab = 1.25, cex.axis = 1.25, xaxs = "i",  
         ylim = c(min(deviation_gamlss$`2.5% Percentil`, na.rm = TRUE), max(deviation_gamlss$`97.5% Percentil`, na.rm = TRUE)))
    
    x_lower <- c(deviation_gamlss$`Age-range from`, tail(deviation_gamlss$`to [Days]`, 1))
    y_lower <- c(deviation_gamlss$`2.5% Percentil`, tail(deviation_gamlss$`2.5% Percentil`, 1))
    
    segments(x_lower[-length(x_lower)],y_lower[-length(x_lower)],x_lower[-1],y_lower[-length(x_lower)])
    lowerlimit <- data.frame(x = x_lower, y = y_lower)
    
    x_upper <- c(deviation_gamlss$`Age-range from`, tail(deviation_gamlss$`to [Days]`, 1))
    y_upper <- c(deviation_gamlss$`97.5% Percentil`, tail(deviation_gamlss$`97.5% Percentil`, 1))
    
    segments(x_upper[-length(x_upper)],y_upper[-length(x_upper)],x_upper[-1],y_upper[-length(x_upper)])
    upperlimit <- data.frame(x = x_upper, y = y_upper)
    
    for (i in 1:(nrow(lowerlimit) - 1)) {
      
      age <- c(lowerlimit$x[i + 1], lowerlimit$x[i], lowerlimit$x[i], lowerlimit$x[i + 1])
      
      lowerlimit_polygon <- c(lowerlimit$y[i], lowerlimit$y[i])
      upperlimit_polygon <- c(upperlimit$y[i], upperlimit$y[i])
      if (length(lowerlimit_polygon > 1)) {
        polygon(age, c(upperlimit_polygon[2], upperlimit_polygon[1], lowerlimit_polygon[1], lowerlimit_polygon[2]), 
                col = rgb(red = 0 , green = 0, blue = 0, alpha = 0.25), border = NA)
      }
    }
  })
  
  ################################ Residuals ######################################################
  
  # Plot the residuals from the GAMLSS
  output$outlier <- renderPlot({
    
    build_gamlss_model()
    data_analyte <- data_analyte()
    
    model <- eval(parse(text = input$select_model_resi))
    
    # Make a dataframe with the residuals 
    residuals_gamlss <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                                   resid = model$residuals, patient = data_analyte$patient)
    
    # Delete possible very high values
    residuals_gamlss <- residuals_gamlss[!is.infinite(residuals_gamlss$resid),]
    
    value_max <- max(residuals_gamlss$resid)
    value_min <- min(residuals_gamlss$resid)
    
    cat(paste("The maximal values for the Residuals is for all models:", round_df(value_max,3), round_df(value_min, 3), "\n"))
    
    palette <- colorRampPalette(c("indianred","seagreen3","cornflowerblue","seagreen3","indianred"))(50)
    
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_gamlss$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = y.axis.name(), main = "Residuals")
  })
  
  # Plot the centiles from the cutted dataset with a small residual
  output$gamlss_outlier <- renderPlot({
    
    build_gamlss_model()
    build_outlier()

    model <- eval(parse(text = paste0("o", input$select_model_resi)))
    
    centiles(model, cent = c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
             col.cent = c("indianred","black","cornflowerblue"), lty.centiles = c(3,1,3), legend = FALSE, lwd.centiles = 2)
  })
  
  # Plot the changed terms from refitted model with P-Splines  
  output$outlier_term_gamlss <- renderPlot({
    
    build_gamlss_model()
    build_outlier()
    
    model <- eval(parse(text = paste0("o", input$select_model_resi)))
    
    plot(model, parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
  
  # Tables with the cutted datatsets ##############################################################
  
  # Table for prediction for the P-Splines
  output$gamlss_residuals_pb <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    colnames(outlierfree_pb) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "RESIDUALS", "ANALYTE")
    
    DT::datatable(outlierfree_pb, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Dataset without the Residuals of the P-Splines')) %>%
    DT::formatRound(6, 2)
  })
  
  # Table for prediction for the Cubic Splines
  output$gamlss_residuals_cs <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    colnames(outlierfree_cs) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "RESIDUALS", "ANALYTE")
    
    DT::datatable(outlierfree_cs, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Cubic Splines')) %>%
    DT::formatRound(6, 2)
  })
  
  # Table for prediction for the Polynomials (Degree 3)
  output$gamlss_residuals_poly <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    colnames(outlierfree_poly) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "RESIDUALS", "ANALYTE")
    
    DT::datatable(outlierfree_poly, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Polynomial Degree 3')) %>%
    DT::formatRound(6, 2)
  })
  
  # Table for prediction for the Polynomials (Degree 4)
  output$gamlss_residuals_poly4 <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    colnames(outlierfree_poly4) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "RESIDUALS", "ANALYTE")
    
    DT::datatable(outlierfree_poly4, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Polynomial Degree 4')) %>%
    DT::formatRound(6, 2)
  })
  
  # Table for prediction for the Neural Network
  output$gamlss_residuals_nn <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    colnames(outlierfree_nn) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "RESIDUALS", "ANALYTE")
    
    DT::datatable(outlierfree_nn, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Neural Network')) %>%
    DT::formatRound(6, 2)
  })
  
  # Table for prediction for the Decision Tree
  output$gamlss_residuals_tr <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    colnames(outlierfree_tr) <- c("ID", "SEX", "AGE_YEARS", "AGE_DAYS", "VALUE", "RESIDUALS", "ANALYTE")
    
    DT::datatable(outlierfree_tr, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Decision Tree')) %>%
    DT::formatRound(6, 2)
  })

  ##################################### Regressionen ##############################################
  
  # Regression (Linear, Polynomials (Degree 10, 3 and 4))
  output$regression <- renderPlot({
    
    par(mfrow = c(2,2))
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    data_regression <- data_analyte()

    ################################### Linear regression #########################################
    
    linear_regression <<- lm(value~age_days, data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 1, col = "grey", xlab = "Age [Days]",
         ylab = y.axis.name(), main = paste("Linear Regression of the data from", data_analyte()[1,7]), xaxs = "i")
    abline(linear_regression)
    
    reg_p <- predict(linear_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    reg_c <- predict(linear_regression, newdata = data.frame(age_days = regression_pred), interval = "confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty = 2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty = 2) 
    
    ###################################  Polynomials (Degree 10) ##################################
    
    poly_2_regression <<- lm(value ~ poly(age_days,10), data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 1, col = "grey", xlab = "Age [Days]",
         ylab = y.axis.name(), main = paste(" Polynomials (10) of the data from", data_analyte()[1,7]), xaxs = "i")
    lines(sort(data_analyte()[,4]), fitted(poly_2_regression)[order(data_analyte()[,4])])
    
    reg_p <- predict(poly_2_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    reg_c <- predict(poly_2_regression, newdata = data.frame(age_days = regression_pred), interval = "confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty = 2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty = 2)
    
    ###################################  Polynomials (Degree 3) ###################################
    
    poly_3_regression <<- lm(value ~ poly(age_days,3), data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 1, col = "grey", xlab = "Age [Days]",
         ylab = y.axis.name(), main = paste(" Polynomials (3) of the data from", data_analyte()[1,7]), xaxs = "i")
    lines(sort(data_analyte()[,4]), fitted(poly_3_regression)[order(data_analyte()[,4])])
    
    reg_p <- predict(poly_3_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    reg_c <- predict(poly_3_regression, newdata = data.frame(age_days = regression_pred), interval = "confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty = 2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty = 2)
  
    ###################################  Polynomials (Degree 4) ###################################
    
    poly_4_regression <<- lm(value ~ poly(age_days,4), data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 1, col = "grey", xlab = "Age [Days]",
         ylab = y.axis.name(), main = paste(" Polynomials (4) of the data from", data_analyte()[1,7]), xaxs = "i")
    lines(sort(data_analyte()[,4]), fitted(poly_4_regression)[order(data_analyte()[,4])])
    
    reg_p <- predict(poly_4_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    reg_c <- predict(poly_4_regression, newdata = data.frame(age_days = regression_pred), interval = "confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty = 2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty = 2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty = 2)
  })
  
  # Metrics from the regressions
  output$regression_table <- DT::renderDataTable({
    
    data_regression <- data_analyte()
    
    # Predict data 
    prediction_linear <- as.data.frame(predict(linear_regression, newdata = data.frame(age_days = data_regression$age_days)))
    prediction_poly_2 <- as.data.frame(predict(poly_2_regression, newdata = data.frame(age_days = data_regression$age_days)))
    prediction_poly_3 <- as.data.frame(predict(poly_3_regression, newdata = data.frame(age_days = data_regression$age_days)))
    prediction_poly_4 <- as.data.frame(predict(poly_4_regression, newdata = data.frame(age_days = data_regression$age_days)))
    
    # Mean Absolute Error (MAE)
    mae_linear_regression <- mae(data_regression$value, prediction_linear[,1])
    mae_poly_2_regression <- mae(data_regression$value, prediction_poly_2[,1])
    mae_poly_3_regression <- mae(data_regression$value, prediction_poly_3[,1])
    mae_poly_4_regression <- mae(data_regression$value, prediction_poly_4[,1])
    
    # Mean Squared Error (MSE)
    mse_linear_regression <- mse(data_regression$value, prediction_linear[,1])
    mse_poly_2_regression <- mse(data_regression$value, prediction_poly_2[,1])
    mse_poly_3_regression <- mse(data_regression$value, prediction_poly_3[,1])
    mse_poly_4_regression <- mse(data_regression$value, prediction_poly_4[,1])
    
    # Root Mean Squared Error (RMSE)
    rmse_linear_regression <- rmse(data_regression$value, prediction_linear[,1])
    rmse_poly_2_regression <- rmse(data_regression$value, prediction_poly_2[,1])
    rmse_poly_3_regression <- rmse(data_regression$value, prediction_poly_3[,1])
    rmse_poly_4_regression <- rmse(data_regression$value, prediction_poly_4[,1])
    
    regression_table <<- data.frame("Regression" = c("Linear Regression"," Polynomials (10)", 
                                                    " Polynomials (3)", " Polynomials (4)"),
                                    R2 = c(summary(linear_regression)$r.squared, summary(poly_2_regression)$r.squared,
                                              summary(poly_3_regression)$r.squared, summary(poly_4_regression)$r.squared), 
                                    AIC = c(AIC(linear_regression), AIC(poly_2_regression), 
                                            AIC(poly_3_regression), AIC(poly_4_regression)), 
                                    BIC = c(BIC(linear_regression), BIC(poly_2_regression), 
                                            BIC(poly_3_regression), BIC(poly_4_regression)), 
                                    MAE = c(mae_linear_regression, mae_poly_2_regression, 
                                            mae_poly_3_regression, mae_poly_4_regression),
                                    MSE = c(mse_linear_regression, mse_poly_2_regression, 
                                            mse_poly_3_regression, mse_poly_4_regression),
                                    RMSE = c(rmse_linear_regression, rmse_poly_2_regression, 
                                             rmse_poly_3_regression, rmse_poly_4_regression),
                                    check.names = FALSE)
    
    regression_table[2:7] <- round_df(regression_table[2:7], 3)
    
    smallest_aic <- regression_table[which.min(regression_table$AIC),]$AIC
    smallest_bic <- regression_table[which.min(regression_table$BIC),]$BIC
    biggest_r2 <- regression_table[which.max(regression_table$R2),]$R2
    smallest_mae <- regression_table[which.min(regression_table$MAE),]$MAE
    smallest_mse <- regression_table[which.min(regression_table$MSE),]$MSE
    smallest_rmse <- regression_table[which.min(regression_table$RMSE),]$RMSE
    
    DT::datatable(regression_table, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), ) %>%
      DT::formatStyle(columns = "AIC", background = styleEqual(smallest_aic, "cornflowerblue")) %>%
      DT::formatStyle(columns = "BIC", background = styleEqual(smallest_bic, "seagreen")) %>%
      DT::formatStyle(columns = "R2", background = styleEqual(biggest_r2, "lavender")) %>%
      DT::formatStyle(columns = "MAE", background = styleEqual(smallest_mae, "lavender")) %>%
      DT::formatStyle(columns = "MSE", background = styleEqual(smallest_mse, "lavender")) %>%
      DT::formatStyle(columns = "RMSE", background = styleEqual(smallest_rmse, "lavender"))
  }) 
  
  # # Analysis from the linear regression
  # output$regression_stats_linear <- renderPlot({
  #   par(mfrow=c(2,2))
  #   plot(linear_regression)
  # })
  # 
  # # Analysis from the  Polynomials (Degree 2)
  # output$regression_stats_poly10 <- renderPlot({
  #   par(mfrow=c(2,2))
  #   plot(poly_2_regression)
  # })
  # 
  # # Analysis from the  Polynomials (Degree 3)
  # output$regression_stats_poly3 <- renderPlot({
  #   par(mfrow=c(2,2))
  #   plot(poly_3_regression)
  # })
  # 
  # # Analysis from the  Polynomials (Degree 4)
  # output$regression_stats_poly4 <- renderPlot({
  #   par(mfrow=c(2,2))
  #   plot(poly_4_regression)
  # })
   
  # Table for the linear Regression
  output$regression_linear <- DT::renderDataTable({

    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(linear_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    regression_ <- data.frame(regression_pred, round(reg_p, 2))
    regression_ <- regression_[,-2]
    
    colnames(regression_) <- c("Age [Days]", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Linear Regression'))
  })
  
  output$regression_poly10 <- DT::renderDataTable({
    
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(poly_4_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    regression_ <- data.frame(regression_pred, round(reg_p, 2))
    regression_ <- regression_[,-2]
    
    colnames(regression_) <- c("Age [Days]", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Polynomial Regression (10)'))
  })
  
  output$regression_poly2 <- DT::renderDataTable({
    
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(poly_2_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    regression_ <- data.frame(regression_pred, round(reg_p, 2))
    regression_ <- regression_[,-2]
    
    colnames(regression_) <- c("Age [Days]", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Polynomial Regression (2)'))
  })
  
  output$regression_poly3 <- DT::renderDataTable({
    
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(poly_3_regression, newdata = data.frame(age_days = regression_pred), interval = "prediction")
    regression_ <- data.frame(regression_pred, round(reg_p, 2))
    regression_ <- regression_[,-2]
    
    colnames(regression_) <- c("Age [Days]", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Polynomial Regression (3)'))
  })

  ################################ Predictions #####################################

  output$Download_pb_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_P_Splines.csv")},
    content = function(file) {
      write.csv2(pb_ri, file, row.names = FALSE)})

  output$Download_cs_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Cubic_Splines.csv")},
    content = function(file) {
      write.csv2(cs_ri, file, row.names = FALSE)})

  output$Download_poly_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Poly3.csv")},
    content = function(file) {
      write.csv2(poly_ri, file, row.names = FALSE)})

  output$Download_poly4_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Poly4.csv")},
    content = function(file) {
      write.csv2(poly4_ri, file, row.names = FALSE)})

  output$Download_tr_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Tree.csv")},
    content = function(file) {
      write.csv2(tr_ri, file, row.names = FALSE)})

  output$Download_nn_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_Neural_Net.csv")},
    content = function(file) {
      write.csv2(nn_ri, file, row.names = FALSE)})

  output$Download_lms_ri <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Prediction_LMS.csv")},
    content = function(file) {
      write.csv2(lms_ri, file, row.names = FALSE)})
  
  ################################ Interface to Zlog_AdrI ##########################
  
  # Regular Window with reflim()
  output$Download_zlog_table_window_data_all_reflimR <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Zlog_AdRI_Regular_Window.csv")},
    content = function(file) {
      
      if (input$sex == "t")
        sex_zlog <- "MF"
      if (input$sex == "m")
        sex_zlog <- "M"
      if (input$sex == "f")
        sex_zlog <- "F"
      
      result <- window_reactive()
      window_data_all <- result$window_data_all
      
      zlog_adri <- data.frame(CODE = data_analyte_short$name[1], LABUNIT = input$text_unit, SEX =  sex_zlog, UNIT = "day",
                              AgeFrom = window_data_all$`Age-range from [days]`, AgeUntil = window_data_all$`to [days]`,
                              LowerLimit = window_data_all$`2.5% RI`, UpperLimit = window_data_all$`97.5% RI`, 
                              check.names = FALSE)
      
      write.csv(zlog_adri, file, row.names = FALSE)})
  
  # Window-Method coupled Decision Tree with reflim()
  output$Download_zlog_window_data_split_reflimR <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Zlog_AdRI_Windowtree.csv")},
    content = function(file) {
      
      colnames(ri_results) <- c("Age_L", "Age_U", "SEX", "LL", "UL")
      
      zlog_adri <- data.frame(CODE = as.factor(data_analyte_short$name[1]), LABUNIT = input$text_unit,
        SEX =  as.character(ri_results$SEX), UNIT = "day", AgeFrom = as.numeric(ri_results$Age_L), AgeUntil = as.numeric(ri_results$Age_U),
        LowerLimit = as.numeric(ri_results$LL), UpperLimit = as.numeric(ri_results$UL), check.names = FALSE)
      
      write.csv(zlog_adri, file, row.names = FALSE, quote = FALSE)
    })
  
  # LIS Window-Method with reflim()
  output$Download_zlog_lis_table_t <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Zlog_AdRI_LIS.csv")},
    content = function(file) {
      
      if (input$sex == "t")
        sex_zlog <- "MF"
      if (input$sex == "m")
        sex_zlog <- "M"
      if (input$sex == "f")
        sex_zlog <- "F"
      
      result <- window_lis()
      window_data_all <- result$window_data_all
      
      zlog_adri <- data.frame(CODE = data_analyte_short$name[1], LABUNIT = input$text_unit, SEX =  sex_zlog, UNIT = "day",
                              AgeFrom = window_data_all$`Age-range from [days]`, AgeUntil = window_data_all$`to [days]`,
                              LowerLimit = window_data_all$`2.5% RI`, UpperLimit = window_data_all$`97.5% RI`, 
                              check.names = FALSE)
      
      write.csv(zlog_adri, file, row.names = FALSE)})
  
  output$Download_zlog_table_gamlss <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Zlog_AdRI_GAMLSS.csv")},
    content = function(file) {

      if (input$sex == "t")
        sex_zlog <- "MF"
      if (input$sex == "m")
        sex_zlog <- "M"
      if (input$sex == "f")
        sex_zlog <- "F"
      
      zlog_adri <- data.frame(CODE = data_analyte_short$name[1], LABUNIT = input$text_unit, SEX =  sex_zlog, UNIT = "day",
                              AgeFrom = deviation_gamlss$`Age-range from`, AgeUntil = deviation_gamlss$`to [Days]`,
                              LowerLimit = deviation_gamlss$`2.5% Percentil`, UpperLimit = deviation_gamlss$`97.5% Percentil`, 
                              check.names = FALSE)

      write.csv(zlog_adri, file, row.names = FALSE)})

  ################################ Residuals #######################################
  
  output$Download_pb_residuals <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Resdiuals_P_Splines.csv")},
    content = function(file) {
      colnames(outlierfree_pb) <- c("ID","SEX","AGE_YEARS","AGE_DAYS","VALUE","RESIDUALS","ANALYTE")
      write.csv2(outlierfree_pb, file, row.names = FALSE)})
  
  output$Download_cs_residuals <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Residuals_Cubic_Splines.csv")},
    content = function(file) {
      colnames(outlierfree_cs) <- c("ID","SEX","AGE_YEARS","AGE_DAYS","VALUE","RESIDUALS","ANALYTE")
      write.csv2(outlierfree_cs, file, row.names = FALSE)})
  
  output$Download_poly_residuals <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Resdiuals_Poly3.csv")},
    content = function(file) {
      colnames(outlierfree_poly) <- c("ID","SEX","AGE_YEARS","AGE_DAYS","VALUE","RESIDUALS","ANALYTE")
      write.csv2(outlierfree_poly, file, row.names = FALSE)})
  
  output$Download_poly4_residuals <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Residuals_Poly4.csv")},
    content = function(file) {
      colnames(outlierfree_poly4) <- c("ID","SEX","AGE_YEARS","AGE_DAYS","VALUE","RESIDUALS","ANALYTE")
      write.csv2(outlierfree_poly4, file, row.names = FALSE)})
  
  output$Download_tr_residuals <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Residuals_Tree.csv")},
    content = function(file) {
      colnames(outlierfree_tr) <- c("ID","SEX","AGE_YEARS","AGE_DAYS","VALUE","RESIDUALS","ANALYTE")
      write.csv2(outlierfree_tr, file, row.names = FALSE)})
  
  output$Download_nn_residuals <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Residuals_Neural_Net.csv")},
    content = function(file) {
      colnames(outlierfree_nn) <- c("ID","SEX","AGE_YEARS","AGE_DAYS","VALUE","RESIDUALS","ANALYTE")
      write.csv2(outlierfree_nn, file, row.names = FALSE)})
  
  ##################################### Comparison ################################################

  # output$Download_rsquared_table <- downloadHandler(
  #   filename = function(){
  #     paste0(Sys.Date(),"_Window_Comparison.csv")},
  #   content = function(file) {
  #     write.csv2(rsquared_table, file, row.names = FALSE)})

  # output$downloadData_comparison <- downloadHandler(
  #   filename = function(){
  #     paste0(Sys.Date(),"_GAMLSS_Comparison.csv")},
  #   content = function(file) {
  #     write.csv2(compare_models, file, row.names = FALSE)})

  output$DownloadData_regression_table <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Regression_Comparison.csv")},
    content = function(file) {
      write.csv2(regression_table, file, row.names = FALSE)})

  ##################################### Plots #####################################################

  # output$download_lms <- downloadHandler(
  #   filename =  function(){
  #     "LMS.eps"},
  #   content = function(file){
  #     setEPS()
  #     postscript(file)
  #     centiles(lms_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     dev.off()})
  
  # output$download_lms_jpeg <- downloadHandler(
  #   filename =  function(){
  #     "LMS.jpeg"},
  #   content = function(file){
  #     jpeg(file)
  #     centiles(lms_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     dev.off()})
  
  # output$download_gamlss <- downloadHandler(
  #   filename =  function(){
  #     "GAMLSS.eps"},
  #   content = function(file){
  #     setEPS()
  #     postscript(file)
  #     
  #     par(mfrow=c(3,2))
  #     
  #     centiles(pb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1, 
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(cs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]",
  #              ylab = y.axis.name(), pch = 20, cex = 1, 
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent=c(2.5,50,97.5), xlab = "Age [Days]", 
  #              ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(nn_, main = "GAMLSS with Neural Network", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(tr_, main = "GAMLSS with Decision Tree", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     dev.off()})
  
  # output$download_gamlss_jpeg <- downloadHandler(
  #   filename =  function(){
  #     "GAMLSS.jpeg"},
  #   content = function(file){
  #     jpeg(file)
  #     
  #     par(mfrow=c(3,2))
  #     
  #     centiles(pb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1, 
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(cs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]",
  #              ylab = y.axis.name(), pch = 20, cex = 1, 
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent=c(2.5,50,97.5), xlab = "Age [Days]", 
  #              ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(nn_, main = "GAMLSS with Neural Network", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     centiles(tr_, main = "GAMLSS with Decision Tree", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = y.axis.name(), pch = 20, cex = 1,
  #              col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 2, 
  #              legend = FALSE, col = "lightgrey")
  #     dev.off()})

  output$download_tree <- downloadHandler(
    filename =  function() {
      "Decision_Tree.eps"},
    content = function(file) {
      setEPS()
      postscript(file)
      rpart.plot(build_rpart(), box.palette = "RdBu", roundint = FALSE)
      dev.off()})
}  

####################################### Run the application #######################################
shinyApp(ui = ui, server = server)