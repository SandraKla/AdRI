####################################### WELCOME TO THE SHINY APP AdRI #############################
####################################### from Sandra K. (2023) #####################################
###################################################################################################

####################################### Scripts ###################################################

source("R/analysis.R")
source("R/reflim.R")
source("R/window.R")

####################################### Libraries #################################################

if("boot" %in% rownames(installed.packages())){
  library(boot)} else{
    install.packages("boot")}

if("dplyr" %in% rownames(installed.packages())){
  library(dplyr)} else{
    install.packages("dplyr")}

if("DT" %in% rownames(installed.packages())){
  library(DT)} else{
    install.packages("DT")}

if("gamlss" %in% rownames(installed.packages())){
  library(gamlss)} else{
    install.packages("gamlss")}

if("gamlss.add" %in% rownames(installed.packages())){
  library(gamlss.add)} else{
    install.packages("gamlss.add")}

#if("hexbin" %in% rownames(installed.packages())){
#  library(hexbin)} else{
#    install.packages("hexbin")}

#if("MASS" %in% rownames(installed.packages())){
#  library(MASS)} else{
#    install.packages("MASS")}

if("plotly" %in% rownames(installed.packages())){
  library(plotly)} else{
    install.packages("plotly")}

if("rpart" %in% rownames(installed.packages())){
  library(rpart)} else{
    install.packages("rpart")}

if("rpart.plot" %in% rownames(installed.packages())){
  library(rpart.plot)} else{
    install.packages("rpart.plot")}

if("zoo" %in% rownames(installed.packages())){
  library(zoo)} else{
    install.packages("zoo")}

####################################### USER INTERFACE ############################################

ui <- fluidPage(
 
  theme = "style.css",  
  navbarPage("Age-dependent Reference Intervals (AdRI)", 
  
  ################################### Overview ##################################################
    
  tabPanel("Analysis", icon = icon("database"),
    sidebarLayout( 
    ### Sidebar - Analysis ###
      sidebarPanel(width = 3,
                   
        selectInput("dataset", "Select preinstalled dataset:", choice = list.files(pattern = c(".csv"), recursive = TRUE)),
          
        #fileInput("dataset_file", "Upload own dataset:", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), hr(),
        uiOutput("dataset_file"),
        
        actionButton('reset', 'Reset Input', icon = icon("trash")), hr(),
          
        selectInput("days_or_years", "Unit for the Age:", choices = list("Year" = "age", "Day "= "age_days")), 
          
        conditionalPanel(condition = "input.days_or_years == 'age'", 
                         sliderInput("age_end", "Select age-range:", min = 0 , max = 100, value = c(0,18))),
          
        conditionalPanel(condition = "input.days_or_years == 'age_days'", 
                         numericInput("age_input_min", "Select age-range from:", 0, min = 0, max = 100*365)), 
      
        conditionalPanel(condition = "input.days_or_years == 'age_days'", 
                         numericInput("age_input", "to:", 100, min = 1, max = 100*365)), hr(),
          
        selectInput("sex", "Select the Sex:", choices = list("Male + Female" = "t", "Male" = "m", "Female" = "f")), 
          
        textInput("text_unit", "Unit of the Analyte:", value = "Unit"), hr(),
          
        helpText("Outlierdetection:"), 
      
        checkboxInput("unique", "First Unique values", value = TRUE),
      
        checkboxInput("checkboxtukey", "RefLim coupled with a Decision Tree", value = FALSE),
      
        helpText("Hyperparameter for the Decision Tree (minbucket): Minimum number of observations in a leaf node (age group).
                 According to CLSI 120 patients must be available for Reference Intervals (RI)!"),
      
        selectInput("tree_minsplit", "Setting for the Decision Tree:", 
                    choices = list("Each group with > 120 patients (for RI according to CLSI)" = 360,
                                   "> 40 patients" = 120, 
                                   "> 20 patients" = 60))),
      
    ### MainPanel - Analysis ###              
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Overview", icon = icon("home"),
                 
          p(strong("Shiny App for calculating Age-dependent Reference Intervals!"), br(), br(),
          "This Shiny App was developed to create Age-dependent Reference Intervals (AdRI) using different methods: 
          LMS, GAMLSS, Window-Methods and Regression.", br(), "For further information visit our", 
          a("Wiki", href="https://github.com/SandraKla/Age-dependent-Reference-Intervals/wiki"),"!"), 
          plotlyOutput("scatterplot_plotly", height="700px")),
            
        tabPanel("Dataset", 
                 
          DT::dataTableOutput("datatable"), verbatimTextOutput("summary")),
            
        tabPanel("Barplots", 
                 
          p("Distribution of the", strong("SEX"),"across the ages and", strong("STATION"),":"),
          plotOutput("barplot_sex", height="500px"), 
          plotOutput("barplot_station", height="300px")),
          
        #tabPanel("2D Density Plot", plotlyOutput("hexbinplotly", height="600px")),
            
        tabPanel("Statistics", 
          
          p("QQ-Plots (Quantile-Quantile plot)"), 
          plotOutput("qqplot", height="400px"),
          p("Density plot to check if the data is normally or log-normally distributed with the help of the Bowley Skewness 
            (see Frank Klawonn et al. (2020)):"),
          plotOutput("lognorm", height="375px"))
        )
    ))),
  
  ################################### Window-Methods ############################################

  navbarMenu("Window-method", icon = icon("align-center"),
      
    ##### Regular Window-Method #####        
    tabPanel("Regular Window-Method",
      sidebarLayout(
        ### Sidebar - Regular Window ###
        sidebarPanel(width = 3,
                     
          selectInput("window_select_regular", "Show outlierdetection in plot:", choices = list("All" = "all",
                                                                                                "None" = "none",
                                                                                                "RefLim" = "tukey")),
          selectInput("method_window_regular", "Calculation-method for the Reference Intervals:", 
                      choices = list("Nonparametric" = "nonpara", "Parametric" = "para", "Hoffmann-Method" = "qqplot")),
          sliderInput("window_age", "Regular Window for the Subgroups in years:", 1, 10, 10),
          conditionalPanel(condition = "input.window_age <= 1", 
                           numericInput("window_agedays", "Regular Window for the Subgroups in days:", 365, min = 1, max = 100*365))),
              
        ### MainPanel - Regular Window ###
        mainPanel(width = 9,
          tabsetPanel(
            tabPanel("Plot",
                           
              p(strong("All Window-Methods differentiate between Normal- and Lognormal-distribution by the RefLim! But the for the calculation
                       use the nonparametric if it is a Lognormal-distribution."), br(),  br(),
                       "This is a regular Window-method, the window is make regular in the same size through the data (given by the user on the left), 
                       so it is only recommended for small changes through the age. Available for the calculation for the reference intervals a 
                       nonparametric, parametric and the Hoffmann-Method (Without visual recognition of the linear range, it is important to know whether 
                       there is a mixed distribution and to use the RefLim before!) for the calculation from the reference intervals and 
                       the RefLim for Outlierdetection."),
              plotOutput("window", height="600px")),

            tabPanel("Table", icon = icon("table"), 
                     
                     downloadButton("Download_window_data_all_outlier", "Table with Reference intervals without Outlierdetection"),
                     DT::dataTableOutput("windowtable_o"), 
                     downloadButton("Download_window_data_all_tukey", "Table with Reference intervals with RefLim"),
                     DT::dataTableOutput("windowtable_t"))
          )
        )
      )
    ),
    
    ##### LIS Window-Method #####      
    tabPanel("LIS Window-method",
      sidebarLayout(
        ### Sidebar - LIS Window-Method ###
        sidebarPanel(width = 3,

          selectInput("window_select_lis", "Show outlierdetection in plot:", choices = list("All" = "all",
                                                                                            "None" = "none",
                                                                                            "RefLim" = "tukey")),
          selectInput("method_window_lis", "Calculation-method for the Reference Intervals:", 
                      choices = list("Nonparametric" = "nonpara", "Parametric" = "para", "Hoffmann-Method" = "qqplot")),
          selectInput("lis_data", "Select preinstalled dataset:", choice = list.files(pattern = "txt", recursive = TRUE)),
          #fileInput("lis_data_file", "Upload TXT File:", accept = ".txt")),
          uiOutput("lis_data_file"),
          actionButton('reset_lis', 'Reset Input', icon = icon("trash"))), 
            
        ### MainPanel - LIS Window-Method ###   
        mainPanel(width = 9,
          tabsetPanel(
            tabPanel("Plot",
                         
              p(strong("All Window-Methods differentiate between Normal- and Lognormal-distribution by the RefLim! But the for the calculation
              use the nonparametric if it is a Lognormal-distribution"), br(),br(), "Load a TXT file on the left with the age groups from your laboratory information system (LIS)
              and the Reference Intervals will be calculated."),
              
              plotOutput("lis_window", height = "600px")),
                           
            tabPanel("Table", icon = icon("table"), 
                     
              downloadButton("Download_lis_table_o","Table with Reference Intervals without Outlierdetection"),
              DT::dataTableOutput("lis_table_o"),
              downloadButton("Download_lis_table_t","Table with Reference Intervals without Outlierdetection"),
              DT::dataTableOutput("lis_table_t")))
        )
      )   
    ),
    
    ##### Decision Tree Method #####                       
    tabPanel("Decision Tree", icon = icon("tree"),
      sidebarLayout(
        ### Sidebar - Decision Tree Method ###
        sidebarPanel(width = 3,
            
          selectInput("window_select_tree", "Show outlierdetection in plot:", choices = list("All" = "all",
                                                                                             "None" = "none",
                                                                                             "RefLim" = "tukey")),
          selectInput("method_window_tree", "Calculation-method for the Reference Intervals:", 
                      choices = list("Nonparametric" = "nonpara", "Parametric" = "para", "Hoffmann-Method" = "qqplot"))),
      
        ### MainPanel - Decision Tree Method ###
        mainPanel(width = 9,
          tabsetPanel(
            tabPanel("Plot",
                     
              p(strong("All Window-Methods differentiate between Normal- and Lognormal-distribution by the RefLim! But the for the calculation
                use the nonparametric if it is a Lognormal-distribution"), br(),br(), "Decision Trees are used for machine learning. It can be used for classification (supervised learning), but also
                for clustering (unsupervised learning). Here it used to cluster the data into subgroups with similar values.
                The Decision is from the package rpart and is visualized with rpart.plot. The subgroups according to the Decision Tree
                are used to calculate the reference intervals (nonparametric, parametric or with the Hoffmann-Method)."),
              plotOutput("tree_window", height = "600px")),
              
            tabPanel("Decision Tree", icon = icon("tree"),
                     
              p("Used Decision Tree and Analysis for each subgroup for Normal- or Lognormaldistribution with the 
                Bowley-Skewness (see Frank Klawonn et al. (2020)):"),
                downloadButton("download_tree", "Decision Tree"),
              plotOutput("tree_rpart", height = "500px"),
              plotOutput("tree_window_analysis")),
                           
            tabPanel("Table", icon = icon("table"), 
                     
              downloadButton("Download_window_data_split_outlier","Table with Reference Intervals without Outlierdetection"),
              DT::dataTableOutput("tree_windowtable_o"),
              downloadButton("Download_window_data_split_tukey", "Table with Reference Intervals with RefLim"), 
              DT::dataTableOutput("tree_windowtable_t")))
        )
      )
    ),
    
    tabPanel("Sliding Window",
      sidebarLayout(
        ### Sidebar - Sliding Window
          sidebarPanel(width = 3,
                
            selectInput("window_select_sliding", "Show outlierdetection in plot:", choices = list("All" = "all",
                                                                                                  "None" = "none",
                                                                                                  "RefLim" = "tukey")),
            selectInput("method_window_sliding", "Calculation-method for the Reference Intervals:", 
                        choices = list("Nonparametric" = "nonpara", "Parametric" = "para", "Hoffmann-Method" = "qqplot")),
                            
            numericInput("sliding_width", "Sliding Window-Method:", 500, min = 10, max = 10000),
            numericInput("sliding_by", "Steps for the Sliding Window-Method:", 100, min = 10, max = 500)),
             
        ### MainPanel - Sliding Window
        mainPanel(width = 9,
          tabsetPanel(
            tabPanel("Plot",
                  
              p(strong("All Window-Methods differentiate between Normal- and Lognormal-distribution by the RefLim! But the for the calculation
              use the nonparametric if it is a Lognormal-distribution"), br(),br(), "Sliding Window-method goes through the data with a window calculates the mean and reference intervals and goes then
              with a window-steps further through the data to the end. Only nonparametric method available for the reference intervals
              This method is not yet validated, caution when using it for meaningful reference intervals."), 
              plotOutput("slidingwindow", height = "600px")),
                       
            tabPanel("Table", icon = icon("table"), 
                     
              downloadButton("Download_sliding", "Table with Reference Intervals without Outlierdetection"),
              DT::dataTableOutput("sliding"),
              downloadButton("Download_sliding_tukey", "Table with Reference Intervals with RefLim"), 
              DT::dataTableOutput("sliding_tukey"))
          )
        )
      )
    ),

    ##### Comparison #####   
    tabPanel("Comparison", icon = icon("balance-scale"),
             
      p("Compare the Regular Window-Method and the Window-Method coupled to
      the Decision Tree with R-Squared (R2), Mean Absolute Error (MAE), 
      Mean squared error (MSE), Root mean squared error (RMSE). Best models to each each metric are marked."),
           
      downloadButton("Download_rsquared_table", "Table with Metrics"), DT::dataTableOutput("rsquared_table"))),
  
    ############################## Quant sheets ####################################
    
    tabPanel("Quant Sheets", icon = icon("table"), plotOutput("quantsheets", height = "600px")),
    
    ############################## Regression ######################################
    
    navbarMenu("Regression", icon = icon("chart-line"),
               
      tabPanel("Overview",icon = icon("home"),
        
        p("Regression can be used for normally distributed data to get the 95% prediction interval. The blue
        line show the 95% Confindence interval from the regression in black and the red the prediction interval (2.5% and 97.5%).
        This method is not yet validated, caution when using it for meaningful reference intervals."), 
        plotOutput("regression", height="800px")),
              
      tabPanel("Tables", icon = icon("table"),
               
        DT::dataTableOutput("regression_linear"), 
        DT::dataTableOutput("regression_poly10"), 
        DT::dataTableOutput("regression_poly2"), 
        DT::dataTableOutput("regression_poly3")),
        
      tabPanel("Comparison", icon = icon("balance-scale"),
               
        p("Compare the Regressions with Akaike Information Criterion (AIC), 
        Bayesian Information Criterion (BIC) / Schwatz Bayesian Criterion (SBC),
        R-Squared (R2), Mean absolute Error (MAE), Mean squared error (MSE), 
        Root mean squared error (RMSE). Best model to each metric is marked"),
        downloadButton("downloadData_regression_table", "Table with Metrics"),
        DT::dataTableOutput("regression_table")),
         
      tabPanel("Analysis",
               
        p("Linear Regression:"), 
        plotOutput("regression_stats_linear", height = "500px"),
        p("Polynomials (Degree 10):"),
        plotOutput("regression_stats_poly10", height = "500px"),
        p("Polynomials (Degree 3):"), 
        plotOutput("regression_stats_poly3", height = "500px"),
        p("Polynomials (Degree 4):"), 
        plotOutput("regression_stats_poly4", height = "500px")
      )
    ),
    
    ############################################# GAMLSS ##########################################
  
    navbarMenu("GAMLSS", icon = icon("chart-line"),
    
      tabPanel("GAMLSS- and LMS-Models",
        sidebarLayout( 
          ### Sidebar - GAMLSS ###
          sidebarPanel(width = 3,
            
            # selectInput("method", "GAMLSS Algorithm:", choices = list("Rigby and Stasinopoulos algorithm (RS)" = "RS",
            #                                                           "Cole and Green algorithm (CG)" = "CG")),
            sliderInput("epochs", "Number of Epochs:", 5 , 250, 30),
            selectInput("distribtion_gamlss", "GAMLSS-Distribution:", choices = list("Normal distribution" = "NO", 
                                                                                     "Log-Normal distribution" = "LOGNO",
                                                                "Box-Cox" = c("Box-Cole Green Distrubtion" = "BCCG", 
                                                                              "Box-Cole Green Distrubtion (orginal)" = "BCCGo",
                                                                              "Box-Cole Green Exp. Distrubtion" = "BCPE",
                                                                              "Box-Cole Green Exp. Distrubtion (orginal)" = "BCPEo", 
                                                                              "Box-Cole Green T-Distribution" = "BCT", 
                                                                              "Box-Cole Green T-Distribution (orginal)" = "BCTo"))),
            checkboxInput("checkbox", "Distribution proposed by the LMS", value = FALSE),
            actionButton("button_gamlss", "GAMLSS-Models", icon("calculator"), onclick = "$(tabs).removeClass('disabled')"),
            actionButton("button_lms", "LMS-Model", icon("calculator"), onclick = "$(tab).removeClass('disabled')"), 
            htmlOutput("buttons_gamlss"), htmlOutput("buttons_lms")#, 
            #hr(), htmlOutput("helptext_gamlss")
          ),
        
        ### MainPanel - GAMLSS ###        
        mainPanel(width = 9, 
                  
          tabsetPanel( 
            tabPanel("Overview", icon = icon("home"), includeHTML("www/gamlss.html"), 
                     
              downloadButton("download_lms", "LMS-Plot in .EPS"),
              downloadButton("download_lms_jpeg", "LMS-Plot in .JPEG"),
              downloadButton("download_gamlss", "GAMLSS-Plot in .EPS"),
              downloadButton("download_gamlss_jpeg", "GAMLSS-Plot in .JPEG")),
            
            tabPanel("LMS", value = "nav_lms",
                     
              plotOutput("lms", height="500px"), p("LMS - Analysis:"), verbatimTextOutput("lms_text"), 
              plotOutput("lms_plot"), plotOutput("lms_fitted", height = "500px"), plotOutput("lms_wormplots")),
            
            tabPanel("GAMLSS with Splines", value = "nav_gamlss",
              
              plotOutput("gamlss_models_splines", height="500px"), verbatimTextOutput("gamlss_text_splines")), 
            
            tabPanel("GAMLSS with Splines - Analysis", value = "nav_gamlss", 
                     
              p("GAMLSS with P-Splines:"), plotOutput("gamlss_term_pb"), plotOutput("gamlss_fitted_pb_"),
              p("GAMLSS with Cubic-Splines:"),plotOutput("gamlss_term_cs"), plotOutput("gamlss_fitted_cs_"),
              p("Wormplots for GAMLSS with the P-Splines and Cubic Splines:"), 
              plotOutput("wormplots_splines", height="500px")),
            
            tabPanel("GAMLSS with Polynomials", value = "nav_gamlss",
                     
              plotOutput("gamlss_models_poly", height="500px"), verbatimTextOutput("gamlss_text_poly")), 
            
            tabPanel("GAMLSS with Polynomial - Analysis", value = "nav_gamlss", 
                    
              p("GAMLSS with Polynomial Degree 3:"), plotOutput("gamlss_term_poly"), plotOutput("gamlss_fitted_poly_"),
              p("GAMLSS with Polynomial Degree 4:"), plotOutput("gamlss_term_poly4"), plotOutput("gamlss_fitted_poly4_"),
              p("Wormplots for GAMLSS with the Polynomial Degree 3 and 4:"), 
              plotOutput("wormplots_poly", height="500px")),
            
            tabPanel("GAMLSS with Neural Network", value = "nav_gamlss",
                   
              plotOutput("gamlss_net", height="500px"), verbatimTextOutput("net_text"), p("Neural Network - Analysis:"), 
              # Plot neural network with term.plot(nn_)
              plotOutput("network_term"), plotOutput("network_fitted", height="500px"), plotOutput("nn_wormplots")),
          
            tabPanel("GAMLSS with Decision Tree", value = "nav_gamlss",
                   
              plotOutput("gamlss_tree", height="500px"), verbatimTextOutput("tree_text"), p("Decision Tree - Analysis:"), 
              plotOutput("rpart_tree"),  plotOutput("tree_term"), plotOutput("tree_fitted", height="500px"), 
              plotOutput("tr_wormplots"))
          ),
          tags$script( src = 'tabs_enabled.js')),
        )
      ),
      
    ##################################### GAMLSS - Comparison #####################################
      
      tabPanel("2. Comparison", icon = icon("balance-scale"),
               
        p("Models from the package gamlss can be compared visually or with the Akaike Information Criterion (AIC), 
        Bayesian Information Criterion (BIC), Generalized Information Criterion (GAIC) 
        or Pseudo R-Squared (R^2). The model with the smallest value for AIC, BIC and GAIC is the best model for the data.
        The Pseudo R-Squared (R^2) should be as large as possible for a good model. These values are colored."), 
               
        downloadButton("downloadData_comparison", "Comparison"),
        DT::dataTableOutput("table_compare"),
        plotOutput("metrics", height = "400px")),  
      
    ################################### GAMLSS - Prediction #######################################
    
    tabPanel("3. Predict the Reference Intervals", 
      sidebarLayout(    
        ### Sidebar - GAMLSS ###
        sidebarPanel(width = 3,
                     
          numericInput("prediction_age", "Prediction of the Reference Intervals for age [Days]:", 0, min = 0, max = 12*365),
          
          selectInput("select_model", "Select Model:", choices = list("Splines" = c("P-Splines" = "pb_ri",
                                                                                                  "Cubic Splines" = "cs_ri"),
                                                                                    "LMS" = c(LMS = "lms_ri"),
                                                                                    "Polynomial" = c("Polynomial (Degree 3)" = "poly_ri", 
                                                                                                   "Polynomial (Degree 4)" = "poly4_ri"),
                                                                                    "Machine Learning" = c("Neural Network" = "nn_ri", 
                                                                                                           "Decision Tree" = "tr_ri"))),
          textOutput("prediction_gamlss"), hr(),
          helpText("To make discrete Reference Intervals from the continuous models the upper or the lower percentiles are compared 
          and split in two age groups when a specific change over the threshold (deviation) occurs:"),
          numericInput("deviation", " Deviation in %:", 10, min = 1, max = 100) #, hr(),
          #htmlOutput("helptext_prediction")
          ),
               
        ### MainPanel - GAMLSS ###
        mainPanel(width = 9,
          tabsetPanel( 
            tabPanel("Predicted Reference Intervals",
                     
              plotOutput("gamlss_prediction", height = "1200px"), 
              plotOutput("gamlss_prediction_lms_plot", height = "400px")),
           
            tabPanel("", icon = icon("table"), 
                     
              downloadButton("Download_pb_ri", "GAMLSS with P-Splines"),
              DT::dataTableOutput("gamlss_table_pb"), 
              downloadButton("Download_cs_ri", "GAMLSS with Cubic Splines"),
              DT::dataTableOutput("gamlss_table_cs"),
              downloadButton("Download_poly_ri", "GAMLSS with Polynomial Regerssion with Degree 3"),
              DT::dataTableOutput("gamlss_table_poly"),
              downloadButton("Download_poly4_ri", "GAMLSS with Polynomial Regerssion with Degree 4"),
              DT::dataTableOutput("gamlss_table_poly4"),
              downloadButton("Download_nn_ri", "GAMLSS with Neural Network"),
              DT::dataTableOutput("gamlss_table_nn"),
              downloadButton("Download_tr_ri", "GAMLSS with Decision Tree"),
              DT::dataTableOutput("gamlss_table_tr"), 
              downloadButton("Download_lms_ri", "LMS Model"),
              DT::dataTableOutput("gamlss_table_lms")),
            
            tabPanel("Discrete Reference Intervals", plotOutput("gamlss_plot", height = "500px"),
                     
              downloadButton("Download_deviation_gamlss", "Table with discrete Reference Intervals"),
              DT::dataTableOutput("gamlss_split"))
          )
        )
      )
    ),
    
    ############################### GAMLSS - Residuals ##############################
    
    tabPanel("4. Residuals", 
      sidebarLayout(  
        ### Sidebar - GAMLSS ###
        sidebarPanel(width = 3,
                            
          helpText("Improvement from the GAMLSS models by deleting high residuals:"),
          numericInput("error", "Value for the maximal Residuals:", 1.5, min = 0.75, max = 10),
          actionButton("button_residuals", "Delete high Residuals and refit the GAMLSS", icon("calculator")) #, hr(), 
          #htmlOutput("helptext_residuals")
          ),
          
        ### MainPanel - GAMLSS ###      
        mainPanel(width = 9,
          tabsetPanel(               
            tabPanel("Overview", 
                     
              p("After fitting the models, the residuals can be calculated and high values can be removed to delete possible 
              outliers from the model and refit the model. High residuals values are in red, low in blue", 
              strong("This must not be equal to the real outliers of the data!"),"."),
              plotOutput("outlier", height="900px")),
                             
            tabPanel("Refit GAMLSS models", 
                     
              plotOutput("gamlss_outlier", height="900px"), 
                                    
              p("GAMLSS with P-Splines"), plotOutput("outlier_term_pb"),
              p("GAMLSS with Cubic Splines"), plotOutput("outlier_term_cs"),
              p("GAMLSS with Polynomial (Degree 3)"), plotOutput("outlier_term_poly"),
              p("GAMLSS with Polynomial (Degree 4)"), plotOutput("outlier_term_poly4"),
              p("GAMLSS with Neural Network"), plotOutput("outlier_term_nn"), 
              p("GAMLSS with Decision Tree"), plotOutput("outlier_term_tr")),
      
            tabPanel("", icon = icon("table"), 
                                    
              p("Tables with the Prediction Tables, can be downloaded and be used like another dataset for this Shiny App!"),
              downloadButton("Download_pb_residuals", "GAMLSS with P-Splines"),
              DT::dataTableOutput("gamlss_residuals_pb"), 
              downloadButton("Download_cs_residuals", "GAMLSS with Cubic Splines"),
              DT::dataTableOutput("gamlss_residuals_cs"),
              downloadButton("Download_poly_residuals", "GAMLSS with Polynomial Regression with Degree 3"),
              DT::dataTableOutput("gamlss_residuals_poly"),
              downloadButton("Download_poly4_residuals", "GAMLSS with Polynomial Regression with Degree 4"),
              DT::dataTableOutput("gamlss_residuals_poly4"),
              downloadButton("Download_nn_residuals", "GAMLSS with Neural Network"),
              DT::dataTableOutput("gamlss_residuals_nn"),
              downloadButton("Download_tr_residuals", "GAMLSS with Decision Tree"),
              DT::dataTableOutput("gamlss_residuals_tr"))
          )
        )
      )
    )
  )
  )
)

####################################### SERVER ####################################################

server <- function(input, output, session) {

  options(shiny.sanitize.errors = TRUE)
  
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
    
    lms_ready <<- FALSE        # To check if the lms method was used
    modelsprediction <<- FALSE # To check if the models are build 
    residuals_ready <<- FALSE  # Check if the residuals are calculated
    
    # Read the data (from the CALIPER study or from the generator)
    if(is.null(dataset_input())){
    data_data <- read.csv2(input$dataset, header = TRUE, 
                           stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "")}
    if(!is.null(dataset_input()))
    {data_data <- read.csv2(dataset_input()[["datapath"]], header = TRUE, 
                            stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "")}
    ################################### Age is given by days ######################################
    if(input$days_or_years == "age_days"){
      
      # Preprocessing the data
      data_analyte <- select_data_days(data_data, input$age_input_min, input$age_input, input$sex)
      
      ################################# First samples #############################################
      
      rows_table_ <- nrow(data_analyte) 
      
      # Take only the first and unique samples from the data if ID is given
      if(input$unique == TRUE){
        
        data_analyte <- 
          data_analyte %>% 
          group_by(patient) %>% 
          filter(row_number()==1)
        # Convert into tibble so as.data.frame()
        data_analyte <- as.data.frame(data_analyte)
        
        if(!(rows_table_ == nrow(data_analyte))){
          cat(paste("Information!", rows_table_ - nrow(data_analyte), "values of the patients were present several times and were deleted. \n"))}
      }
      
      try(if(input$checkboxtukey == TRUE){
        
        rows_table_ <- nrow(data_analyte) 
        
        data_analyte_tukey <- data.frame()
        # Make Decision Tree to group the data in groups
        rpart_ready <- make_rpart(data_analyte, as.numeric(input$tree_minsplit))
        # Read the splits from the Decision Tree
        splits <- data.frame(rpart_$splits)
        
        split <- round(c(0,sort(splits$index), max(data_analyte$age_days))) 
        
        # Select each range of the splits and delete outliers with the iBoxplot95()
        for (i in 2:length(split)){
          
          data_analyte_split <- subset(data_analyte, age_days <= split[i], select = c(patient, sex, age, age_days, value, code, name)) 
          data_analyte_subset <- subset(data_analyte_split, data_analyte_split$age_days > split[i-1])
          
          if(split[i-1] == 0){
            data_analyte_subset <- subset(data_analyte_split, data_analyte_split$age_days >= split[i-1])}
          
          normal_log <- FALSE
          try(normal_log <- def.distribution(data_analyte_subset$value, plot.it = FALSE)$lognorm)
          
          if(normal_log == TRUE){modi <- iBoxplot95(data_analyte_subset$value, plot.it = FALSE, lognorm = TRUE)}
          else{modi <- iBoxplot95(data_analyte_subset$value, plot.it = FALSE)}
          
          data_analyte_save <- data_analyte_subset[data_analyte_subset$value %in% modi,]
          data_analyte_tukey <- rbind(data_analyte_tukey, data_analyte_save)}
        
        data_analyte <- data_analyte_tukey
        
        if(!(rows_table_ == nrow(data_analyte))){
          cat(paste("Information!", rows_table_ - nrow(data_analyte), "values were deleted because of the RefLim. \n"))}
      })
    } 
    
    else{
      
      ################################# Age in years (default) ####################################
      
      # Preprocessing the data
      data_analyte <- select_data(data_data,input$age_end[1] ,input$age_end[2], input$sex)
      
      ################################# First samples #############################################
      
      rows_table_ <- nrow(data_analyte) 
      
      if(input$unique == TRUE){
        
        data_analyte <- 
          data_analyte %>% 
          group_by(patient) %>% 
          filter(row_number()==1)
        # Convert into tibble so as.data.frame()
        data_analyte <- as.data.frame(data_analyte)
        
        if(!(rows_table_ == nrow(data_analyte))){
          cat(paste("Information!", rows_table_ - nrow(data_analyte), "values of the patients were present several times and were deleted. \n"))}
      }
      
      try(if(input$checkboxtukey == TRUE){
        
        rows_table_ <- nrow(data_analyte) 
        
        data_analyte_tukey <- data.frame()
        # Make Decision Tree to group the data in groups
        rpart_ready <- make_rpart(data_analyte, as.numeric(input$tree_minsplit))
        
        # Read the splits from the Decision Tree
        splits <- data.frame(rpart_$splits) 
        
        split <- round(c(0,sort(splits$index), max(data_analyte$age_days)))

        # Select each range of the splits and delete outliers with the iBoxplot95()
        for (i in 2:length(split)){
          
          data_analyte_split <- subset(data_analyte, age_days <= split[i], select = c(patient, sex, age, age_days, value, code, name)) 
          data_analyte_subset <- subset(data_analyte_split, data_analyte_split$age_days > split[i-1])
          
          if(split[i-1] == 0){
            data_analyte_subset <- subset(data_analyte_split, data_analyte_split$age_days >= split[i-1])}
          
          normal_log <- FALSE
          try(normal_log <- def.distribution(data_analyte_subset$value, plot.it = FALSE)$lognorm)
          
          if(normal_log == TRUE){modi <- iBoxplot95(data_analyte_subset$value, plot.it = FALSE, lognorm = TRUE)}
          else{modi <- iBoxplot95(data_analyte_subset$value, plot.it = FALSE)}
          
          data_analyte_save <- data_analyte_subset[data_analyte_subset$value %in% modi,] 
          data_analyte_tukey <- rbind(data_analyte_tukey, data_analyte_save)}
        
        data_analyte <- data_analyte_tukey
        
        if(!(rows_table_ == nrow(data_analyte))){
          cat(paste("Information!", rows_table_ - nrow(data_analyte), "values were deleted because of the RefLim. \n"))}
      })
    }
    
    cat("\n")

    data_analyte_short <<- data_analyte
    on.exit(progress$close())
    data_analyte
  })
  
  ##################################### Build rpart Decision Tree #################################
  
  build_rpart <- reactive({
    rpart_ready <- make_rpart(data_analyte(), as.numeric(input$tree_minsplit))
  })
  
  ##################################### Reactive Regular Window ###################################
  
  window_reactive <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate RI with regular windows...", detail = "", value = 2)
    
    days <- input$window_age*365
    if(input$window_age <= 1){
      days <- input$window_agedays}
    
    window_method(data_analyte(), days, input$method_window_regular)
    
    on.exit(progress$close())
  })
  
  ##################################### Reactive Window with Decision Tree ########################
  
  windowtree <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Make groups with the Decision Tree and calculate RI...", detail = "", value = 2)
    
    build_rpart()
    splits <- data.frame(rpart_$splits)
  
    split <- round(c(0,sort(splits$index), max(data_analyte()$age_days)))
    window_method_split(data_analyte(), split, input$method_window_tree, FALSE)
    
    on.exit(progress$close())
  })
  
  ##################################### Reactive Sliding Window ###################################
  
  slidingwindow <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Sliding Window RI...", detail = "", value = 2)
    
    req(input$sliding_width, input$sliding_by)
    slide <- sliding_window(data_analyte(), input$sliding_width, input$sliding_by, outliers = input$window_select_window)

    on.exit(progress$close())
    slide
  })
  
  ##################################### Reactive GAMLSS ###########################################
  # make_gamlss() is used with six different smooth additive terms for the GAMLLSS models #########
  # with different distributions. #################################################################
  
  build_gamlss_model <- eventReactive(input$button_gamlss, {
    
    req(input$dataset, input$age_end, input$distribtion_gamlss, input$epochs, "RS")
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Percentiles with GAMLSS-Models...", detail = "", value = 2)

    # Error message
    if(input$checkbox == TRUE){
      validate(need(lms_ready == TRUE, 
      "Please make first the LMS-Method to get the proposed distribution!"))
      gamlss_model_read <- make_gamlss(data_analyte(), input$age_end[2], lms_$family[1], input$epochs, "RS")} 

    else{gamlss_model_read <- make_gamlss(data_analyte(), input$age_end[2], input$distribtion_gamlss, input$epochs, 
                                          "RS")}
    
    # Save global value to check later if the models are already calculated
    modelsprediction <<- TRUE
    
    on.exit(progress$close())
  })
  
  ##################################### Reactive LMS ##############################################
  
  lms_reactive <- eventReactive(input$button_lms, {
    
    progress <- shiny::Progress$new()
    progress$set(message = "Calculate Percentiles with LMS...", detail = "", value = 2)
    
    new_lms_data <<- data.frame(value_lms = data_analyte()[[5]], age_lms = data_analyte()[[4]])
    lms_ <<- lms(value_lms, age_lms, k=2, data = new_lms_data, cent=c(2.5,50,97.5), trans.x = TRUE)
    
    lms_ready <<- TRUE # Value to check if lms is accomplished
    on.exit(progress$close())
  })
  
  ##################################### Reactive GAMLSS - Residuals ###############################
  # Use the outliers_residuals() to delete very high and low residuals and refit the models #######
  
  build_outlier <- eventReactive(input$button_residuals, {
    
    progress <- shiny::Progress$new()
    progress$set(message = "Refit the GAMLSS Models...", detail = "", value = 2)
    
    outliers_residuals(data_analyte(), input$distribtion_gamlss, input$epochs, "RS", input$error)

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
    if(input$days_or_years == "age_days"){ 
      updateSliderInput(session = session, inputId = "window_age", max = round(input$age_end[2]/365-input$age_end[1]/365))}
    else{updateSliderInput(session = session, inputId = "window_age", max = input$age_end[2]-input$age_end[1])}
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
  
  # Scatterplot from the data_analyte()
  # output$scatterplot <- renderPlot({
  #   
  #   scatterplot_data <- data_analyte()
  #   plot(scatterplot_data$value ~ scatterplot_data$age_days , pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
  #        ylab = ylab_)
  # })

  # Scatterplot from the data_analyte() with plotly
  output$scatterplot_plotly <- renderPlotly({

    ylab_ <<- paste0(data_analyte()[1,7]," [", input$text_unit,"]") 
    
    #if(input$fast == FALSE){
      fig <- plot_ly(data_analyte(), x = ~age_days, y = ~value, 
                   text = ~ paste('</br>Patient: ', patient,
                                  '</br>Station: ', code,
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
          layout(xaxis = list(title="Age [Days]", titlefont=list(size=20), tickfont = list(size = 15)), 
                 yaxis = list(title=ylab_, titlefont=list(size=20), tickfont = list(size = 15)))#}
    # else{
    #   fig <- plot_ly(data_analyte(), x = ~age_days, y = ~value, color = ~sex, colors = c("cornflowerblue", "indianred"),
    #                  text = ~ paste('</br>Patient: ', patient,
    #                                 '</br>Station: ', code,
    #                                 '</br>Age [Years]: ', age,
    #                                 '</br>Age [Days]: ', age_days,
    #                                 '</br>Value: ', value),
    #                  type = "scattergl",
    #                  mode = "markers",
    #                  marker = list(size = 5)) %>%
    #     layout(xaxis = list(title="Age [Days]", titlefont=list(size=20), tickfont = list(size = 15)), 
    #            yaxis = list(title=ylab_, titlefont=list(size=20), tickfont = list(size = 15)))
    #   }
  })
  
  # Barplot with the distribution of the sex
  output$barplot_sex <- renderPlot({

    hist_data_w <- subset(data_analyte(), sex == "F", select = age)
    hist_data_m <- subset(data_analyte(), sex == "M", select = age)
  
    hist_w <- hist(hist_data_w$age, breaks=seq(min(data_analyte()[,3])-1,max(data_analyte()[,3]),by=1))$counts
    hist_m <- hist(hist_data_m$age, breaks=seq(min(data_analyte()[,3])-1,max(data_analyte()[,3]),by=1))$counts
  
    barplot(rbind(hist_m,hist_w), col = c("cornflowerblue","indianred"),
          names.arg=seq(min(data_analyte()[,3]), max(data_analyte()[,3]), by=1), xlab = "Age [Years]", las = 1)
    abline(h=0)
    legend("topright", legend = c("Men","Women"), col = c("cornflowerblue","indianred"), pch = 20)
  })

  # Barplot with the distribution of the stations
  output$barplot_station <- renderPlot({
    barplot(table(data_analyte()[,6]), las=2, col = "grey")
    abline(h=0)
  })
  
  # QQ-Plot for the complete dataset  
  output$qqplot <- renderPlot({
    
    qqnorm(data_analyte()[,5], pch = 20, col = "grey")
    qqline(data_analyte()[,5])
  })
  
  # Bowley and Lognormfunction
  output$lognorm <- renderPlot({
    try(def.distribution(data_analyte()[,5]))
  })
  
  #Hexbin for the Data with the package hexbin
  # output$hexbin <- renderPlot({
  # 
  #   hexbin_data <- data_analyte()
  #   try(plot(hexbin(hexbin_data$value ~ hexbin_data$age_days, ylab = ylab_, xlab = "Age [Days]", shape = 0.5, xbins = 100)))
  # })
  
  output$hexbinplotly <- renderPlotly({
    
    hexbin_data <- data_analyte()
    s <- subplot(
      plot_ly(data_analyte(), x = ~age_days, type = "histogram", nbinsx = 100),
      plotly_empty(type = "scatter", mode = "markers"),
      plot_ly(data_analyte(), x = ~age_days, y = ~value, type = "histogram2dcontour", nbinsx = 100) %>%
        layout(xaxis = list(title="Age [Days]", titlefont=list(size=20), tickfont = list(size = 15)), 
               yaxis = list(title=ylab_, titlefont=list(size=20), tickfont = list(size = 15))),
      plot_ly(data_analyte(), y = ~value, type = "histogram", nbinsx = 100),
      nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
      shareX = TRUE, shareY = TRUE)
    fig <- layout(s, showlegend = FALSE)
  })
  
  # Summary of the data for a short overview
  output$summary <- renderPrint({
    print(summary(data_analyte()))
  })
  
  # Data-Table
  output$datatable <- DT::renderDataTable({
    DT::datatable(data_analyte(), extensions = 'Buttons', rownames= FALSE, 
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')),
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset'))
  })
  
  ##################################### Window-Methods ############################################
  
  # Regular Window method
  output$window <- renderPlot({
    
    window_reactive()
    
    # Show plot with outliers and deleted outlier with the iBoxplot95()
    if(input$window_select_regular == "all"){
   
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
           ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data$age_days, window_data$mean, type="s", col= "indianred", lty=3, lwd = 1.5)
      points(window_data$age_days, window_data$quantile1, type="s", col= "indianred", lty=6, lwd = 1.5)
      points(window_data$age_days, window_data$quantile2, type="s", col= "indianred", lty=6, lwd = 1.5)

      points(window_data_tukey$age_days, window_data_tukey$mean, type="s", col= "seagreen3", lty=3, lwd = 1.5)
      points(window_data_tukey$age_days, window_data_tukey$quantile1, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(window_data_tukey$age_days, window_data_tukey$quantile2, type="s", col= "seagreen3", lty=6, lwd = 1.5)
    
      legend("topright", legend = c("Without Outlierdetection", "Outlierdetection with the RefLim"), 
             col = c("Indianred", "seagreen3"), pch = 20, cex = 1.25)}
    
    # No Outlierdetection
    if(input$window_select_regular == "none"){
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
           ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data$age_days, window_data$mean, type="s", col= "black", lty=3, lwd = 1.5)
      points(window_data$age_days, window_data$quantile1, type="s", col= "indianred", lty=6, lwd = 1.5)
      points(window_data$age_days, window_data$quantile2, type="s", col= "indianred", lty=6, lwd = 1.5)}
     
    # RefLim
    if(input$window_select_regular == "tukey"){ 
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
           ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data_tukey$age_days, window_data_tukey$mean, type="s", col= "black", lty=3, lwd = 1.5)
      points(window_data_tukey$age_days, window_data_tukey$quantile1, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(window_data_tukey$age_days, window_data_tukey$quantile2, type="s", col= "seagreen3", lty=6, lwd = 1.5)}
  })
  
  # Tables to the regular windowmethod - Without Outlierdetction
  output$windowtable_o <- DT::renderDataTable({
    
    window_reactive()

    DT::datatable(window_data_all_outlier, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Regular Window-Method without Outlierdetection')) %>%
    DT::formatStyle(columns = c(1,2), backgroundColor = "indianred") %>% 
    DT::formatRound(c(3:length(window_data_all_outlier)), 2)
  })
  
  # Tables to the regular windowmethod - With RefLim
  output$windowtable_t <- DT::renderDataTable({
    
    window_reactive()
    
    DT::datatable(window_data_all_tukey, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Regular Window-Method With RefLim')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "seagreen") %>% 
      DT::formatRound(c(3:length(window_data_all_tukey)), 2)
  })
  
  # Window-Method coupled to a Decision Tree
  output$tree_window <- renderPlot({
    
    # Build Decision Tree
    build_rpart()
    windowtree()

    if(input$window_select_tree == "all"){
    
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
    
      points(window_data_rpart$age_days, window_data_rpart$mean, type="s", col= "red", lty=3, lwd = 1.5)
      points(window_data_rpart$age_days, window_data_rpart$quantile1, type="s", col= "indianred", lty=6, lwd = 1.5)
      points(window_data_rpart$age_days, window_data_rpart$quantile2, type="s", col= "indianred", lty=6, lwd = 1.5)
    
      points(window_data_tukey_rpart$age_days, window_data_tukey_rpart$mean, type="s", col= "green", lty=3, lwd = 1.5)
      points(window_data_tukey_rpart$age_days, window_data_tukey_rpart$quantile1, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(window_data_tukey_rpart$age_days, window_data_tukey_rpart$quantile2, type="s", col= "seagreen3", lty=6, lwd = 1.5)
    
      legend("topright", legend = c("Without Outlierdetection", "Outlierdetection with the RefLim"), 
             col = c("Indianred", "seagreen3"), pch = 20, cex = 1.25)}
    
    if(input$window_select_tree == "none"){
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data_rpart$age_days, window_data_rpart$mean, type="s", col= "black", lty=3, lwd = 1.5)
      points(window_data_rpart$age_days, window_data_rpart$quantile1, type="s", col= "indianred", lty=6, lwd = 1.5)
      points(window_data_rpart$age_days, window_data_rpart$quantile2, type="s", col= "indianred", lty=6, lwd = 1.5) }
    
    if(input$window_select_tree == "tukey"){    
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data_tukey_rpart$age_days, window_data_tukey_rpart$mean, type="s", col= "black", lty=3, lwd = 1.5)
      points(window_data_tukey_rpart$age_days, window_data_tukey_rpart$quantile1, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(window_data_tukey_rpart$age_days, window_data_tukey_rpart$quantile2, type="s", col= "seagreen3", lty=6, lwd = 1.5)}
  })
  
  # Plot used Decision Tree
  output$tree_rpart <- renderPlot({
    
    build_rpart()
    rpart.plot(rpart_, box.palette = "RdBu", roundint = FALSE)
    #rsq.rpart(rpart_)
  })
  
  # Window-Method coupled to a Decision Tree - Analysis for Normal- and Lognormaldistribution
  output$tree_window_analysis <- renderPlot({
    
    # Build Decision Tree
    build_rpart()
    
    windowtree()
    
    splits <- data.frame(rpart_$splits)
    split <- round(c(0,sort(splits$index), max(data_analyte()$age_days)))
    par(mfrow=c(1,length(split)-1))
    
    window_method_split(data_analyte(), split, input$method_window_tree, TRUE)
  })
  
  # Tables to the Window-method with Decision Tree - Without Outlierdetection
  output$tree_windowtable_o <- DT::renderDataTable({
    
    build_rpart()  
    windowtree()
             
    DT::datatable(window_data_split_outlier, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Window-Method with Decision Tree without Outlierdetection')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "indianred") %>% 
      DT::formatRound(c(3:length(window_data_split_outlier)), 2)
  })
  
  # Tables to the Window-method with Decision Tree - With RefLim
  output$tree_windowtable_t <- DT::renderDataTable({
  
    build_rpart()  
    windowtree()
    
    DT::datatable(window_data_split_tukey, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style ='caption-side: bottom; text-align: center;','Table: Window Method with Decision Tree with RefLim')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "seagreen") %>% 
      DT::formatRound(c(3:length(window_data_split_tukey)), 2)
  })
  
  # Sliding Window-Method 
  output$slidingwindow <- renderPlot({
  
    slidingwindow()
    
    if(input$window_select_sliding == "all"){
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(slidingwindow()[,2], slidingwindow()[,3], type="s", col= "red", lty=3, lwd = 1.5)
      points(slidingwindow()[,2], slidingwindow()[,4], type="s", col= "indianred", lty=6, lwd = 1.5)
      points(slidingwindow()[,2], slidingwindow()[,5], type="s", col= "indianred", lty=6, lwd = 1.5)
      
      points(slide_tukey[,2], slide_tukey[,3], type="s", col= "green", lty=3, lwd = 1.5)
      points(slide_tukey[,2], slide_tukey[,4], type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(slide_tukey[,2], slide_tukey[,5], type="s", col= "seagreen3", lty=6, lwd = 1.5)
      
      legend("topright", legend = c("Without Outlierdetection", "Outlierdetection with RefLim"), 
             col = c("Indianred", "seagreen3"), pch = 20, cex = 1.25)}
    
    if(input$window_select_sliding == "none"){
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(slidingwindow()[,2], slidingwindow()[,3], type="s", col= "black", lty=3, lwd = 1.5)
      points(slidingwindow()[,2], slidingwindow()[,4], type="s", col= "indianred", lty=6, lwd = 1.5)
      points(slidingwindow()[,2], slidingwindow()[,5], type="s", col= "indianred", lty=6, lwd = 1.5)}
    
    if(input$window_select_sliding == "tukey"){    
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(slide_tukey[,2], slide_tukey[,3], type="s", col= "green", lty=3, lwd = 1.5)
      points(slide_tukey[,2], slide_tukey[,4], type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(slide_tukey[,2], slide_tukey[,5], type="s", col= "seagreen3", lty=6, lwd = 1.5)}
    })
  
  # Sliding Window-Method - Table
  output$sliding <- DT::renderDataTable({
    
    slidingwindow()

    DT::datatable(slide, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Slding Window-Method')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "indianred") %>% 
      DT::formatRound(c(3:length(slide)), 2)
  })
  
  output$sliding_tukey <- DT::renderDataTable({
    
    slidingwindow()
    
    DT::datatable(slide_tukey, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Slding Window-Method with RefLim')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "seagreen") %>% 
      DT::formatRound(c(3:length(slide_tukey)), 2)
  })
  
  # Comparison Regular Window and Sliding Window
  output$rsquared_table <- DT::renderDataTable({
  
    data_analyte()
    window_reactive()
    build_rpart()
    windowtree()
    
    mae_window <- mae_window()
    mse_window <- mse_window()
    rmse_window <- rmse_window()
    r_test <- r_window()

    rsquared_table <<- data.frame("Method" = c("Regular Window (No Outlierdetction)","Regular Window (RefLim)", 
                                                "Decision Tree Window (No Outlierdetction)", "Decision Tree Window (RefLim)"),
                                  MAE = c(mae_window),
                                  MSE = c(mse_window),
                                  RMSE = c(rmse_window),
                                  R2 = c(r_test), check.names = FALSE)
    # Round the data
    rsquared_table[2] <- round_df(rsquared_table[2], 3)
    rsquared_table[3] <- round_df(rsquared_table[3], 3)
    rsquared_table[4] <- round_df(rsquared_table[4], 3)
    rsquared_table[5] <- round_df(rsquared_table[5], 3)
    
    biggest_r2 <- rsquared_table[which.max(rsquared_table$R2),]$R2
    smallest_mae <- rsquared_table[which.min(rsquared_table$MAE),]$MAE
    smallest_mse <- rsquared_table[which.min(rsquared_table$MSE),]$MSE
    smallest_rmse <- rsquared_table[which.min(rsquared_table$RMSE),]$RMSE
    
    DT::datatable(rsquared_table, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print'))) %>%
      DT:: formatStyle(columns = "R2", background = styleEqual(biggest_r2, "lavender")) %>%
      DT:: formatStyle(columns = "MAE", background = styleEqual(smallest_mae, "lavender")) %>%
      DT:: formatStyle(columns = "MSE", background = styleEqual(smallest_mse, "lavender")) %>%
      DT:: formatStyle(columns = "RMSE", background = styleEqual(smallest_rmse, "lavender"))
  })
  
  # LIS-Method
  output$lis_window <- renderPlot({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Make groups with the given intervals from the LIS...", detail = "", value = 2)
    
    if(is.null(dataset_input_lis())){
      lis_data <<- read.delim2(input$lis_data)}
    if(!is.null(dataset_input_lis()))
    {lis_data <<- read.delim2(dataset_input_lis()[["datapath"]])}
    
    lis <- subset(lis_data, AGE_TO <= input$age_input)

    lis <- data.frame(lis_data[,2])
    splits <- data.frame(index = lis)
    
    split <- round(c(0,sort(splits[,1])))
    window_method_lis(data_analyte(), split, input$method_window_lis, FALSE)
    
    on.exit(progress$close())
    
    if(input$window_select_lis == "all"){
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data_lis$age_days, window_data_lis$mean, type="s", col= "red", lty=3, lwd = 1.5)
      points(window_data_lis$age_days, window_data_lis$quantile1, type="s", col= "indianred", lty=6, lwd = 1.5)
      points(window_data_lis$age_days, window_data_lis$quantile2, type="s", col= "indianred", lty=6, lwd = 1.5)
      
      points(window_data_tukey_lis$age_days, window_data_tukey_lis$mean, type="s", col= "green", lty=3, lwd = 1.5)
      points(window_data_tukey_lis$age_days, window_data_tukey_lis$quantile1, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(window_data_tukey_lis$age_days, window_data_tukey_lis$quantile2, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      
      legend("topright", legend = c("Without Outlierdetection", "Outlierdetection with the RefLim"), 
             col = c("Indianred", "seagreen3"), pch = 20, cex = 1.25)}
    
    if(input$window_select_lis == "none"){
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data_lis$age_days, window_data_lis$mean, type="s", col= "black", lty=3, lwd = 1.5)
      points(window_data_lis$age_days, window_data_lis$quantile1, type="s", col= "indianred", lty=6, lwd = 1.5)
      points(window_data_lis$age_days, window_data_lis$quantile2, type="s", col= "indianred", lty=6, lwd = 1.5) }
    
    if(input$window_select_lis == "tukey"){    
      
      plot(value~age_days, data=data_analyte(), pch = 20, cex = 0.75, col = "grey", 
           xlab = "Age [Days]", ylab = ylab_, cex.lab = 1.25, cex.axis = 1.25, xaxs = "i")
      
      points(window_data_tukey_lis$age_days, window_data_tukey_lis$mean, type="s", col= "black", lty=3, lwd = 1.5)
      points(window_data_tukey_lis$age_days, window_data_tukey_lis$quantile1, type="s", col= "seagreen3", lty=6, lwd = 1.5)
      points(window_data_tukey_lis$age_days, window_data_tukey_lis$quantile2, type="s", col= "seagreen3", lty=6, lwd = 1.5)}
    })
  
  
  # Tables to the Window-method with Decision Tree - Without Outlierdetection
  output$lis_table_o <- DT::renderDataTable({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Make groups with the given intervals from the LIS...", detail = "", value = 2)
    
    lis_data <- read.delim2(input$lis_data)
    lis <- subset(lis_data, AGE_TO <= input$age_input)
    
    lis <- data.frame(lis_data[,2])
    splits <- data.frame(index = lis)
    
    split <- round(c(0,sort(splits[,1])))
    window_method_lis(data_analyte(), split, input$method_window_lis, FALSE)
    
    on.exit(progress$close())
    
    DT::datatable(window_data_lis_outlier, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: LIS Window-Method without Outlierdetection')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "indianred") %>% 
      DT::formatRound(c(3:length(window_data_lis_outlier)), 2)
  })
  
  # Tables to the Window-method with Decision Tree - With RefLim
  output$lis_table_t <- DT::renderDataTable({
    
    progress <- shiny::Progress$new()
    progress$set(message = "Make groups with the given intervals from the LIS...", detail = "", value = 2)
    
    lis_data <- read.delim2(input$lis_data)
    lis <- subset(lis_data, AGE_TO <= input$age_input)
    
    lis <- data.frame(lis_data[,2])
    splits <- data.frame(index = lis*365)
    
    split <- round(c(0,sort(splits[,1])))
    window_method_lis(data_analyte(), split, input$method_window_lis, FALSE)
    
    on.exit(progress$close())
    
    DT::datatable(window_data_lis_tukey, rownames= FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style ='caption-side: bottom; text-align: center;','Table: LIS Window-Method with RefLim')) %>%
      DT::formatStyle(columns = c(1,2), backgroundColor = "seagreen") %>% 
      DT::formatRound(c(3:length(window_data_lis_tukey)), 2)
  })
  
  ################################ Quant Sheets ####################################
  
  output$quantsheets <- renderPlot({

    data_analyte()

    # Check transformation for x
    data_power <- findPower(data_analyte_short[,3],data_analyte_short[,4])
    # Make Quant sheets with the power of the transformation
    quantsheets <<- quantSheets(value, age_days, data = data_analyte_short,
                       cent = c(2.5, 50, 97.5), power = data_power)
  })
  
  ################################ LMS #############################################
  
  # LMS-Percentile Plot
  output$lms <- renderPlot({
    
    lms_reactive()
    centiles(lms_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75, 
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3),lwd.centiles = 1.5, 
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
    centiles(lms_, cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  # Wormplots LMS
  output$lms_wormplots <- renderPlot({
    lms_reactive()
    wp(lms_, ylim.all = 3, col = "cornflowerblue")
  })
  
  ##################################### GAMLSS ####################################################
  
  output$buttons_gamlss <- renderUI({
    build_gamlss_model()
    print("Your GAMLSS models are ready!")
  })
  
  output$buttons_lms <- renderUI({
    lms_reactive()
    print("Your LMS model is ready!")
  })
  
  # Centiles Plot with gamlss models (P-Splines, Cubic Splines) ######
  output$gamlss_models_splines <- renderPlot({
    
    build_gamlss_model()
    par(mfrow=c(1,2))
    
    centiles(pb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = ylab_, pch = 20, cex = 0.75, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 1.5 , legend = FALSE, col = "lightgrey")
    centiles(cs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = ylab_, pch = 20, cex = 0.75, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 1.5, legend = FALSE, col = "lightgrey")
  })
  
  # Centiles Plot with gamlss models (Polynomials Degree 3 and 4) ######
  output$gamlss_models_poly <- renderPlot({
    
    build_gamlss_model()
    par(mfrow=c(1,2))
 
    centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]",
             ylab = ylab_, pch = 20, cex = 0.75, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 1.5, legend = FALSE, col = "lightgrey")
    centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent=c(2.5,50,97.5), xlab = "Age [Days]", 
             ylab = ylab_, pch = 20, cex = 0.75, col.cent=c("indianred","black","cornflowerblue"), 
             lty.centiles=c(3,1,3), lwd.centiles = 1.5, legend = FALSE, col = "lightgrey")
  })
  
  # Plot fitted models for P-Splines 
  output$gamlss_fitted_pb_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(pb_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Cubic Splines
  output$gamlss_fitted_cs_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(cs_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Polynomials Degree 3
  output$gamlss_fitted_poly_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(poly_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Plot fitted models for Polynomials Degree 4
  output$gamlss_fitted_poly4_ <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(poly4_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_splines <- renderPrint({
     
    build_gamlss_model()
    
    print("GAMLSS with P-Splines")
    centiles(pb_, cent=c(2.5,50,97.5), plot=FALSE)
    #summary(pb_)
    print("GAMLSS with Cubic splines")
    centiles(cs_, cent=c(2.5,50,97.5), plot=FALSE) 
    #summary(cs_)
  })
  
  # GAMLSS - Analysis Text
  output$gamlss_text_poly <- renderPrint({
    
    build_gamlss_model()
    
    print("GAMLSS with Polynomials (Degree 3)")
    centiles(poly_,cent=c(2.5,50,97.5), plot=FALSE)
    #summary(poly_)
    print("GAMLSS with Polynomials (Degree 4)")
    centiles(poly4_, cent=c(2.5,50,97.5), plot=FALSE)
    #summary(poly4_)
  })
  
  # Plot the changed terms for P-Splines
  output$gamlss_term_pb <- renderPlot({

    build_gamlss_model()
    try(plot(pb_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })
  
  # Plot the changed terms for Cubic Splines
  output$gamlss_term_cs <- renderPlot({
    
    build_gamlss_model()
    try(plot(cs_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })
  
  # Plot the changed terms for Polynomial Degree 3
  output$gamlss_term_poly <- renderPlot({
    
    build_gamlss_model()
    try(plot(poly_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                                 c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                 col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))) 
  })
  
  # Plot the changed terms for Polynomial Degree 4
  output$gamlss_term_poly4 <- renderPlot({
    
    build_gamlss_model()
     try(plot(poly4_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                                  c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                  col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))) 
  })
  
  # Wormplots from P-Splines and Cubic Splines
  output$wormplots_splines <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(2,2))
    
    try(wp(pb_, ylim.all  = 3, col = "cornflowerblue"))
    try(wp(cs_, ylim.all = 3, col = "cornflowerblue"))
  })
  
  # Wormplots from Polynomials Degree 3 and 4
  output$wormplots_ploy <- renderPlot({
    
    build_gamlss_model()
    par(mfrow = c(2,2))
    
    try(wp(poly_, ylim.all = 3, col = "cornflowerblue"))
    try(wp(poly4_, ylim.all = 3, col = "cornflowerblue"))
  })
  
  # Neural Network (machine learning) ##############################################
  
  # Centiles Plot with the Neural Network 
  output$gamlss_net <- renderPlot({
    
    build_gamlss_model()
    centiles(nn_, main = "GAMLSS with Neural Network (3 Hidden-Units, decay=0.1)", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, legend = FALSE, col = "lightgrey")
  })

  # Neural Network - Analysis
  output$network_term <- renderPlot({
    
    build_gamlss_model()
    try(plot(nn_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                               c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                               col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  }) 

  # Neural Network - Analysis
  output$network_fitted <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(nn_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Neural Network - Analysis
  output$net_text<- renderPrint({
    
    build_gamlss_model()
    centiles(nn_,cent=c(2.5,50,97.5), plot=FALSE)
  })
  
  #Wormplots from the Neural Network
  output$nn_wormplots <- renderPlot({
    
    build_gamlss_model()
    try(wp(nn_, ylim.all = 3, col = "cornflowerblue"))
  })
  
  # Decision Tree #################################################################################
  
  # Centiles Plot with the Decision Tree
  output$gamlss_tree <- renderPlot({
    
    build_gamlss_model()
    
    centiles(tr_, main = "GAMLSS with Decision Tree only for mu (minsplit = 360, cp = 0.01)", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, legend = FALSE, col = "lightgrey")
  })
  
  # Decision Tree - Analysis
  output$tree_term <- renderPlot({
    
    build_gamlss_model()
    try(plot(tr_, parameters = par(mfrow = c(2,2), mar = par("mar")+
                              c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5)))
  })

  # Decision Tree - Analysis
  output$tree_fitted <- renderPlot({
    
    build_gamlss_model()
    fittedPlot(nn_, x=data_analyte_short$age_days, xlab = "Age [Days]")
  })
  
  # Decision Tree - Analysis
  output$tree_text<- renderPrint({
    
    build_gamlss_model()
    print(getSmo(tr_))
    centiles(tr_,cent=c(2.5,50,97.5), plot=FALSE)
  })

  # Plotted Decision Tree
  output$rpart_tree <- renderPlot({
    
    build_gamlss_model()
    rpart.plot(getSmo(tr_), roundint=FALSE, box.palette = "RdBu")
  })
  
  # Wormplots from Decision Tree
  output$tr_wormplots <- renderPlot({
    
    build_gamlss_model()
    try(wp(tr_, ylim.all = 3, col = "cornflowerblue"))
  })
  
  # Comparism #####################################################################################
  
  # Comparison Table for all GAMLSS models and LMS
  output$table_compare <- DT::renderDataTable({
  
    if(lms_ready == TRUE){ 
      build_gamlss_model()
      lms_reactive()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(pb_,cs_,poly_, poly4_, nn_,tr_, lms_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(pb_,cs_,poly_, poly4_, nn_,tr_,lms_, k=3))
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
      compare_models <- merge(AIC_,GAIC_,by=c("model"))
      compare_models <- merge(compare_models,BIC_,by=c("model"))
      compare_models <- merge(compare_models,R_2,by=c("model"))
      
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
    
    if(lms_ready==FALSE){
      
      build_gamlss_model()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(pb_,cs_,poly_, poly4_, nn_,tr_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(pb_,cs_,poly_, poly4_, nn_,tr_, k=3))
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
      
      # Merge the Metrics
      compare_models <- merge(AIC_,GAIC_,by=c("model"))
      compare_models <- merge(compare_models,BIC_,by=c("model"))
      compare_models <- merge(compare_models,R_2,by=c("model"))
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
    
    if(residuals_ready == TRUE){
     
      build_gamlss_model()
      build_outlier()
      
      # Akaike Information Criterion (AIC)
      AIC_ <- data.frame(AIC(opb_,ocs_,opoly_, opoly4_, onn_,otr_))
      AIC_$model <- rownames(AIC_)
      rownames(AIC_) <- c()
      
      # Generalized Akaike Information Criterion (GAIC)
      GAIC_ <- data.frame(GAIC(opb_,ocs_,opoly_, opoly4_, onn_,otr_, k=3))
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
      compare_models_residuals <- merge(AIC_,GAIC_,by=c("model"))
      compare_models_residuals <- merge(compare_models_residuals,BIC_,by=c("model"))
      compare_models_residuals <- merge(compare_models_residuals,R_2,by=c("model"))
      
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
      DT:: formatStyle(columns = "AIC", background = styleEqual(row_smallest_aic, "cornflowerblue")) %>%
      DT:: formatStyle(columns = "GAIC", background = styleEqual(row_smallest_gaic, "indianred")) %>%
      DT:: formatStyle(columns = "BIC", background = styleEqual(row_smallest_bic, "seagreen")) %>%
      DT:: formatStyle(columns = "R2", background = styleEqual(biggest_r2, "lavender")) 
  })
  
  # Plot with the Metrics (AIC, GAIC, BIC and R^2)
  output$metrics <- renderPlot({
    
    build_gamlss_model()
    if(lms_ready == TRUE){lms_reactive()}
    
    par(mar = c(12, 3, 3, 3))
    par(mfrow = c(1,2))

    barplot(rbind(compare_models[,2],compare_models[,3],compare_models[,4]), ylab = "Value", 
            ylim = c(min(rbind(compare_models[,2],compare_models[,3],compare_models[,4])) - min(rbind(compare_models[,2],compare_models[,3],compare_models[,4])/100), 
                     max(rbind(compare_models[,2],compare_models[,3],compare_models[,4])) + max(rbind(compare_models[,2],compare_models[,3],compare_models[,4])/100)),
            xpd = FALSE, beside = TRUE, las = 2, names.arg=c(compare_models[,1]), col = c("cornflowerblue","indianred","seagreen3"))
    legend("topright", legend = c("AIC","GAIC","BIC"),
           col = c("cornflowerblue","indianred","seagreen3"), pch = 20)
    
    barplot(compare_models[,5], ylab = "Value",  ylim = c(0, 1), las = 2,
         names.arg=c(compare_models[,1]), col = c("lavender"))
    legend("topright", legend = c("Pseudo R^2"),col = c("lavender"), pch = 20)
  })
  
  ##################################### Prediction ################################################
  
  # Predict new values with the fitted models
  output$gamlss_prediction <- renderPlot({
    
    par(mfrow = c(3,2))
    
    # Create new x_values with all possible days in the age range
    data_subset <- data_analyte()
    subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
    x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by=1)
    
    build_gamlss_model()
    
    pb_ri <<- centiles.pred(pb_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    plot(pb_ri$x, pb_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3,  main = "GAMLSS with P-Splines", type = "l",
         col = "indianred", ylim = c(0,max(pb_ri$`97.5`)))
    lines(pb_ri$x, pb_ri$`50`, cex = 0.5, lty = 1, col = "black")
    lines(pb_ri$x, pb_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
    
    cs_ri <<- centiles.pred(cs_, xname="age_days",  xvalues=x_values, cent = c(2.5,50,97.5))
    plot(cs_ri$x, cs_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3,  main = "GAMLSS with Cubic Splines", type = "l",
         col = "indianred", ylim = c(0,max(cs_ri$`97.5`)))
    lines(cs_ri$x, cs_ri$`50`, cex = 0.5, lty = 1, col = "black")
    lines(cs_ri$x, cs_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
    
    poly_ri <<- centiles.pred(poly_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    plot(poly_ri$x, poly_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3,
         main = "GAMLSS with Polynomials (Degree 3)", type = "l", col = "indianred", ylim = c(0,max(poly_ri$`97.5`)))
    lines(poly_ri$x, poly_ri$`50`, cex = 0.5, lty = 1, col = "black")
    lines(poly_ri$x, poly_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
    
    poly4_ri <<- centiles.pred(poly4_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    plot(poly4_ri$x, poly4_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3,  type = "l",
         main = "GAMLSS with Polynomials (Degree 4)", col = "indianred", ylim = c(0,max(poly4_ri$`97.5`)))
    lines(poly4_ri$x, poly4_ri$`50`, cex = 0.5, lty = 1, col = "black")
    lines(poly4_ri$x, poly4_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
    
    nn_ri <<- centiles.pred(nn_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    plot(nn_ri$x, nn_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3, type = "l",
         main = "GAMLSS with Neural Network", col = "indianred", ylim = c(0,max(nn_ri$`97.5`)))
    lines(nn_ri$x, nn_ri$`50`, cex = 0.5, lty = 1, col = "black")
    lines(nn_ri$x, nn_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
    
    tr_ri <<- centiles.pred(tr_, xname="age_days", xvalues=x_values, cent = c(2.5,50,97.5))
    plot(tr_ri$x, tr_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3, type = "l",
         main = "GAMLSS with Decision Tree", col = "indianred", ylim = c(0,max(tr_ri$`97.5`)))
    lines(tr_ri$x, tr_ri$`50`, cex = 0.5, lty = 1, col = "black")
    lines(tr_ri$x, tr_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")
  })
      
  # Predict new values with the fitted LMS model 
  output$gamlss_prediction_lms_plot <- renderPlot({
    
    data_subset <- data_analyte()
    subset_age_days <- max(subset(data_subset, age == max(data_subset$age), c(age_days)))
    
    lms_reactive()
    x_values <- seq(round(min(data_analyte()[,4]),1), subset_age_days, by=1)
    
    if(lms_ready){
      
      lms_ri <<- centiles.pred(lms_, xname="age_lms", xvalues=x_values, cent = c(2.5,50,97.5))
      
      plot(lms_ri$x, lms_ri$`2.5`, xlab = "Age [Days]", ylab = ylab_, cex = 0.5, lty = 3, type = "l",
           main = "LMS", col = "indianred", ylim = c(0,max(lms_ri$`97.5`)))
      lines(lms_ri$x, lms_ri$`50`, cex = 0.5, lty = 1, col = "black")
      lines(lms_ri$x, lms_ri$`97.5`, cex = 0.5, lty = 3, col = "cornflowerblue")}
  })

  # Tables of the predicted values ################################################################
  
  # Table for prediction for the P-Splines
  output$gamlss_table_pb <- DT::renderDataTable({
    
    build_gamlss_model()
    
    if(exists("pb_ri")){
    table_pb_ri <- data.frame(pb_ri)
    colnames(table_pb_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
    DT::datatable(table_pb_ri, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the P-Splines'))}
  })
  
  # Table for prediction for the Cubic Splines
  output$gamlss_table_cs <- DT::renderDataTable({
    
    build_gamlss_model()
   
    if(exists("cs_ri")){
    table_cs_ri <- data.frame(cs_ri)
    colnames(table_cs_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
    DT::datatable(table_cs_ri, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Prediction of the Cubic Splines'))}
  })
  
  # Table for prediction for the  Polynomials (Degree 3)
  output$gamlss_table_poly <- DT::renderDataTable({
    
    build_gamlss_model()
    
    if(exists("poly_ri")){
    table_poly_ri <- data.frame(poly_ri)
    colnames(table_poly_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
    DT::datatable(table_poly_ri, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Prediction of the  Polynomials (Degree 3)'))}
  })
  
  # Table for prediction for the  Polynomials (Degree 4)
  output$gamlss_table_poly4 <- DT::renderDataTable({
    
    build_gamlss_model()
    
    if(exists("poly4_ri")){
    table_poly4_ri <- data.frame(poly4_ri)
    colnames(table_poly4_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
    DT::datatable(table_poly4_ri, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the  Polynomials (Degree 4)'))}
  })
  
  # Table for prediction for the Neural Network
  output$gamlss_table_nn <- DT::renderDataTable({
    
    build_gamlss_model()
   
    if(exists("nn_ri")){
    table_nn_ri <- data.frame(nn_ri)
    colnames(table_nn_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
    DT::datatable(table_nn_ri, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the Neural Network'))}
  })
  
  # Table for prediction for the Decision Tree
  output$gamlss_table_tr <- DT::renderDataTable({
    
    build_gamlss_model()
   
    if(exists("tr_ri")){
    table_tr_ri <- data.frame(tr_ri)
    colnames(table_tr_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
    DT::datatable(table_tr_ri, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the Decision Tree'))}
  })
  
  # Table for prediction with the LMS model
  output$gamlss_table_lms <- DT::renderDataTable({
    
    if(lms_ready == TRUE){
      build_gamlss_model()
      lms_reactive()
      
      if(exists("lms_ri")){
      table_lms_ri <- data.frame(lms_ri)
      colnames(table_lms_ri) <- c("Age [Days]", "2.5% Percentile", "50% Percentile", "97.5% Percentile")
      DT::datatable(table_lms_ri, rownames = FALSE, extensions = 'Buttons',
                    options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                    caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the LMS'))}}
  })
  
  # Tool to predict reference intervals for the different gamlss models
  output$prediction_gamlss <- renderText({
    
    if(input$select_model == "lms_ri"){text_model <- "LMS"}
    if(input$select_model == "pb_ri"){text_model <- "P-Splines"}
    if(input$select_model == "cs_ri"){text_model <- "Cubic Splines"}
    if(input$select_model == "poly_ri"){text_model <- " Polynomials (Degree 3)"}
    if(input$select_model == "poly4_ri"){text_model <- "Polynomials (Degree 4)"}
    if(input$select_model == "tr_ri"){text_model <- "Decsion Tree"}
    if(input$select_model == "nn_ri"){text_model <- "Neural Network"}
   
    if(modelsprediction == TRUE){
      if(input$select_model == "lms_ri" && lms_ready == FALSE){
        prediction_text <- "First you need to make LMS Models, than you can predict with these!"
      }
    
      else{
        value <- eval(parse(text = input$select_model))[input$prediction_age+1,]
        colnames(value) <- c("Age [Days]", "2.5% Percentile", "50% Prcentile", "97.5% Percentile")
        prediction_text <- paste("The Reference Intervals for", input$prediction_age, "days for the dataset", 
                                 input$dataset, "is for the GAMLSS Model", text_model,":", round(value[1,2], digits = 2),
                                 "(2.5% Percentil) to", round(value[1,4], digits = 2),
                                 "(97.5% Percentil) with the Median (50% Percentil):", round(value[1,3], digits = 2))}}
    else{prediction_text <- "First you need to make GAMLSS models, than you can predict with these!"}
  
    prediction_text
  })

  # Tables with the discrete values from the predicted GAMLSS models ##############################
  output$gamlss_split <- DT::renderDataTable({
    
    if(input$select_model == "lms_ri"){
      if(lms_ready == TRUE){text_model <- "LMS"}
      else{validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))}}
    
    if(input$select_model == "pb_ri"){text_model <- "P-Splines"}
    if(input$select_model == "cs_ri"){text_model <- "Cubic Splines"}
    if(input$select_model == "poly_ri"){text_model <- " Polynomials (Degree 3)"}
    if(input$select_model == "poly4_ri"){text_model <- "Polynomials (Degree 4)"}
    if(input$select_model == "tr_ri"){text_model <- "Decision Tree"}
    if(input$select_model == "nn_ri"){text_model <- "Neural Network"}
    
    build_gamlss_model()
    
    deviation_gamlss <- split_gamlss(eval(parse(text = input$select_model)), input$deviation/100)
    deviation_gamlss <- rbind(0, deviation_gamlss, max(eval(parse(text = input$select_model))))
    
    mean_gamlss <- data.frame()
    gamlss_2_5 <- data.frame()
    gamlss_97_5 <- data.frame()
    model <- eval(parse(text = input$select_model))
      
    for (i in seq(2,nrow(deviation_gamlss))){
      
      # The data subset 
      age_data <- subset(model, model$x <= deviation_gamlss[i,])  
      age_data_ready <- subset(age_data, age_data$x > deviation_gamlss[i-1,])  # Below the lowest condition
    
      mean_gamlss_ <- mean(age_data_ready$`50`) 
      mean_gamlss <- rbind(mean_gamlss,mean_gamlss_) 
      
      gamlss_2_5_ <- mean(age_data_ready$`2.5`) 
      gamlss_2_5 <- rbind(gamlss_2_5,gamlss_2_5_) 
      
      gamlss_97_5_ <- mean(age_data_ready$`97.5`) 
      gamlss_97_5 <- rbind(gamlss_97_5,gamlss_97_5_) 
    }
    
    if(lms_ready == TRUE){
      
      mean_gamlss <- data.frame()
      gamlss_2_5 <- data.frame()
      gamlss_97_5 <- data.frame()
      
      lms_reactive()
      model <- eval(parse(text = "lms_ri"))
      
      for (i in seq(2,nrow(deviation_gamlss))){
        
        # The data subset 
        age_data <- subset(model, model$x <= deviation_gamlss[i,])  
        age_data_ready <- subset(age_data, age_data$x > deviation_gamlss[i-1,])  # Below the lowest condition
        
        mean_gamlss_ <- mean(age_data_ready$`50`) 
        mean_gamlss <- rbind(mean_gamlss,mean_gamlss_) 
        
        gamlss_2_5_ <- mean(age_data_ready$`2.5`) 
        gamlss_2_5 <- rbind(gamlss_2_5,gamlss_2_5_) 
        
        gamlss_97_5_ <- mean(age_data_ready$`97.5`) 
        gamlss_97_5 <- rbind(gamlss_97_5,gamlss_97_5_) 
      }
    }
    
    deviation_gamlss <- cbind(head(deviation_gamlss,-1), tail(deviation_gamlss,-1), head(deviation_gamlss,-1)/365, tail(deviation_gamlss,-1)/365,
                              gamlss_2_5, mean_gamlss, gamlss_97_5)
    colnames(deviation_gamlss) <- c("Age-range from", "to [Days]","Age from", "to [Years]",
                                    "2.5% Percentil","50% Percentil","97.5% Percentil")
    deviation_gamlss <<- deviation_gamlss
    DT::datatable(deviation_gamlss, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Prediction of the GAMLSS Models with', text_model)) %>%
    DT::formatRound(c(3:length(deviation_gamlss)), 2)
  })
  
  # Plot for the discrete reference intervals from the GAMLSS-models
  output$gamlss_plot <- renderPlot({
    
    input$deviation
    input$select_model
    
    build_gamlss_model()
   
    if(input$select_model == "lms_ri"){
      validate(need(lms_ready == TRUE, "Please use the LMS-Method first!"))
    }
    
    plot(deviation_gamlss$`Age-range from`, deviation_gamlss$`97.5% Percentil`, type = "s", col = "cornflowerblue", xlab = "Age [Days]",
         ylab = ylab_, lwd = 2, ylim = c(min(deviation_gamlss$`2.5% Percentil`, na.rm = TRUE), max(deviation_gamlss$`97.5% Percentil`, na.rm = TRUE)))
    lines(deviation_gamlss$`Age-range from`, deviation_gamlss$`2.5% Percentil`, type = "s", col = "indianred", lwd = 2)
    lines(deviation_gamlss$`Age-range from`, deviation_gamlss$`50% Percentil`, type = "s", col = "black")
  })
  
  ################################ Residuals #######################################
  
  # Plot the residuals from the GAMLSS models
  output$outlier <- renderPlot({
    
    build_gamlss_model()
    data_analyte <- data_analyte()
    
    # Make a dataframe with the residuals 
    residuals_pb <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                               resid = pb_$residuals, patient = data_analyte$patient)
    residuals_cs <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                               resid = cs_$residuals, patient = data_analyte$patient)
    residuals_poly <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                                 resid = poly_$residuals, patient = data_analyte$patient)
    residuals_poly4 <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                                  resid = poly4_$residuals, patient = data_analyte$patient)
    residuals_nn <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                               resid = nn_$residuals, patient = data_analyte$patient)
    residuals_tr <- data.frame(value = data_analyte$value, age_days = data_analyte$age_days, 
                               resid = tr_$residuals, patient = data_analyte$patient)
    
    # Delete possible very high values
    residuals_pb <- residuals_pb[!is.infinite(residuals_pb$resid),]
    residuals_cs <- residuals_cs[!is.infinite(residuals_cs$resid),]
    residuals_poly <- residuals_poly[!is.infinite(residuals_poly$resid),]
    residuals_poly4 <- residuals_poly4[!is.infinite(residuals_poly4$resid),]
    residuals_nn <- residuals_nn[!is.infinite(residuals_nn$resid),]
    residuals_tr <- residuals_tr[!is.infinite(residuals_tr$resid),]
    
    value_max <- max(residuals_pb$resid, residuals_cs$resid, residuals_poly$resid,
                     residuals_poly4$resid, residuals_nn$resid, residuals_tr$resid)
    value_min <- min(residuals_pb$resid, residuals_cs$resid, residuals_poly$resid,
                     residuals_poly4$resid, residuals_nn$resid, residuals_tr$resid)
    
    cat(paste("The maximal values for the Residuals is for all models:", round_df(value_max,3), round_df(value_min, 3), "\n"))
    
    par(mfrow = c(2,3))
    palette <- colorRampPalette(c("indianred","seagreen3","cornflowerblue","seagreen3","indianred"))(50)
    
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_pb$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = ylab_, main = "Residuals from P-Splines")
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_cs$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = ylab_, main = "Residuals from Cubic Splines")
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_poly$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = ylab_, main = "Residuals from  Polynomials (Degree 3)")
    
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_poly4$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = ylab_, main = "Residuals from  Polynomials (Degree 4)")
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_nn$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = ylab_, main = "Residuals from Neural Network")
    plot(data_analyte()[,4],data_analyte()[,5], col = palette[cut(residuals_tr$resid, 50)], pch = 20, cex = 1.5, xaxs = "i",
         xlab = "Age [Days]", ylab = ylab_, main = "Residuals from Decision Tree")
  })
  
  # Plot the centiles from the cutted dataset with a small residual
  output$gamlss_outlier <- renderPlot({
    
    build_gamlss_model()
    build_outlier()

    par(mfrow=c(2,3))
    
    centiles(opb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), legend = FALSE, lwd.centiles = 1.5)
    centiles(ocs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), legend = FALSE, lwd.centiles = 1.5)
    centiles(opoly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, lwd.centiles = 1.5,
             pch = 20, cex = 0.75, col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), legend = FALSE)
    centiles(opoly4_, main = "GAMLSS with Polynomials (Degree 4)", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, lwd.centiles = 1.5,
             pch = 20, cex = 0.75, col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), legend = FALSE)
    centiles(onn_, main = "GAMLSS with Neural Network", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75, lwd.centiles = 1.5,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), legend = FALSE)
    centiles(otr_, main = "GAMLSS with Decision Tree", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75, lwd.centiles = 1.5,
             col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), legend = FALSE)
  })
  
  # Plot the changed terms from refitted model with P-Splines  
  output$outlier_term_pb <- renderPlot({
    
    build_gamlss_model()
    build_outlier()
    
    plot(opb_,parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
    })
  
  # Plot the changed terms from refitted model with Cubic Splines
  output$outlier_term_cs <- renderPlot({
      
    build_gamlss_model()
    build_outlier()
    
    plot(ocs_,parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
    })
    
  # Plot the changed terms from refitted model with  Polynomials (Degree 3)
  output$outlier_term_poly <- renderPlot({
      
    build_gamlss_model()
    build_outlier()
      
    plot(opoly_,parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
  
  # Plot the changed terms from refitted model with  Polynomials (Degree 4)
  output$outlier_term_poly4 <- renderPlot({
      
    build_gamlss_model()
    build_outlier()
    
    plot(opoly4_,parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                                 col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
    
  # Plot the changed terms from refitted model with Neural Network
  output$outlier_term_nn <- renderPlot({
      
    build_gamlss_model()
    build_outlier()
    
    plot(onn_,parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
    
  # Plot the changed terms from refitted model with Decision Tree
  output$outlier_term_tr <- renderPlot({
      
    build_gamlss_model()
    build_outlier()
    
    plot(otr_,parameters = par(mfrow = c(2,2), mar = par("mar") + c(0,1,0,0), col.axis = "black", col = "grey", col.main = "black",
                              col.lab = "black", pch = 20, cex = 0.5, cex.lab = 1.5, cex.axis = 1, cex.main = 1.5))
  })
  
  # Tables with the cutted datatsets #################################################
  
  # Table for prediction for the P-Splines
  output$gamlss_residuals_pb <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    DT::datatable(outlierfree_pb, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Dataset without the Residuals of the P-Splines'))
  })
  
  # Table for prediction for the Cubic Splines
  output$gamlss_residuals_cs <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    DT::datatable(outlierfree_cs, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Cubic Splines'))
  })
  
  # Table for prediction for the Polynomials (Degree 3)
  output$gamlss_residuals_poly <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    DT::datatable(outlierfree_poly, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Polynomial Degree 3'))
  })
  
  # Table for prediction for the Polynomials (Degree 4)
  output$gamlss_residuals_poly4 <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    DT::datatable(outlierfree_poly4, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Polynomial Degree 4'))
  })
  
  # Table for prediction for the Neural Network
  output$gamlss_residuals_nn <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    DT::datatable(outlierfree_nn, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Neural Network'))
  })
  
  # Table for prediction for the Decision Tree
  output$gamlss_residuals_tr <- DT::renderDataTable({
    
    build_gamlss_model()
    build_outlier()
    
    DT::datatable(outlierfree_tr, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table: Dataset without the Residuals of the Decision Tree'))
  })

  ##################################### Regressionen ##############################################
  
  # Regression (Linear, Polynomials (Degree 10, 3 and 4))
  output$regression <- renderPlot({
    
    par(mfrow = c(2,2))
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    data_regression <- data_analyte()

    ################################### Linear regression #########################################
    
    linear_regression <<- lm(value~age_days, data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
         ylab = ylab_, main = paste("Linear Regression of the data from", data_analyte()[1,7]), xaxs = "i")
    abline(linear_regression)
    
    reg_p <- predict(linear_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    reg_c <- predict(linear_regression, newdata=data.frame(age_days=regression_pred), interval="confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty=2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty=2) 
    
    ###################################  Polynomials (Degree 10) ##################################
    
    poly_2_regression <<- lm(value ~ poly(age_days,10), data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
         ylab = ylab_, main = paste(" Polynomials (10) of the data from", data_analyte()[1,7]), xaxs = "i")
    lines(sort(data_analyte()[,4]), fitted(poly_2_regression)[order(data_analyte()[,4])])
    
    reg_p <- predict(poly_2_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    reg_c <- predict(poly_2_regression, newdata=data.frame(age_days=regression_pred), interval="confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty=2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty=2)
    
    ###################################  Polynomials (Degree 3) ###################################
    
    poly_3_regression <<- lm(value ~ poly(age_days,3), data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
         ylab = ylab_, main = paste(" Polynomials (3) of the data from", data_analyte()[1,7]), xaxs = "i")
    lines(sort(data_analyte()[,4]), fitted(poly_3_regression)[order(data_analyte()[,4])])
    
    reg_p <- predict(poly_3_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    reg_c <- predict(poly_3_regression, newdata=data.frame(age_days=regression_pred), interval="confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty=2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty=2)
  
    ###################################  Polynomials (Degree 4) ###################################
    
    poly_4_regression <<- lm(value ~ poly(age_days,4), data = data_regression)
    plot(data_analyte()[,5] ~ data_analyte()[,4], pch = 20, cex = 0.75, col = "grey", xlab = "Age [Days]",
         ylab = ylab_, main = paste(" Polynomials (4) of the data from", data_analyte()[1,7]), xaxs = "i")
    lines(sort(data_analyte()[,4]), fitted(poly_4_regression)[order(data_analyte()[,4])])
    
    reg_p <- predict(poly_4_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    reg_c <- predict(poly_4_regression, newdata=data.frame(age_days=regression_pred), interval="confidence", level = 0.95)
    lines(regression_pred, reg_c[,3], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_c[,2], col = "cornflowerblue", lty=2)
    lines(regression_pred, reg_p[,3], col = "indianred", lty=2)
    lines(regression_pred, reg_p[,2], col = "indianred", lty=2)
  })
  
  # Metrics from the regressions
  output$regression_table <- DT::renderDataTable({
    
    data_regression <- data_analyte()
    
    # Predict data 
    prediction_linear <- as.data.frame(predict(linear_regression, newdata=data.frame(age_days=data_regression$age_days)))
    prediction_poly_2 <- as.data.frame(predict(poly_2_regression, newdata=data.frame(age_days=data_regression$age_days)))
    prediction_poly_3 <- as.data.frame(predict(poly_3_regression, newdata=data.frame(age_days=data_regression$age_days)))
    prediction_poly_4 <- as.data.frame(predict(poly_4_regression, newdata=data.frame(age_days=data_regression$age_days)))
    
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
    
    regression_table <<- data.frame("Regression" = c("Lineare Regression"," Polynomials (10)", 
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
      DT:: formatStyle(columns = "AIC", background = styleEqual(smallest_aic, "cornflowerblue")) %>%
      DT:: formatStyle(columns = "BIC", background = styleEqual(smallest_bic, "seagreen")) %>%
      DT:: formatStyle(columns = "R2", background = styleEqual(biggest_r2, "lavender")) %>%
      DT:: formatStyle(columns = "MAE", background = styleEqual(smallest_mae, "lavender")) %>%
      DT:: formatStyle(columns = "MSE", background = styleEqual(smallest_mse, "lavender")) %>%
      DT:: formatStyle(columns = "RMSE", background = styleEqual(smallest_rmse, "lavender"))
  }) 
  
  # Analysis from the linear regression
  output$regression_stats_linear <- renderPlot({
    par(mfrow=c(2,2))
    plot(linear_regression)
  })
  
  # Analysis from the  Polynomials (Degree 2)
  output$regression_stats_poly10 <- renderPlot({
    par(mfrow=c(2,2))
    plot(poly_2_regression)
  })
  
  # Analysis from the  Polynomials (Degree 3)
  output$regression_stats_poly3 <- renderPlot({
    par(mfrow=c(2,2))
    plot(poly_3_regression)
  })
  
  # Analysis from the  Polynomials (Degree 4)
  output$regression_stats_poly4 <- renderPlot({
    par(mfrow=c(2,2))
    plot(poly_4_regression)
  })
   
  
  # Table for the linear Regression
  output$regression_linear <- DT::renderDataTable({

    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(linear_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    regression_ <- data.frame(regression_pred, reg_p)
    
    colnames(regression_) <- c("Age [Days]", "50% Regression", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Linear Regression'))
  })
  
  output$regression_poly10 <- DT::renderDataTable({
    
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(poly_4_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    regression_ <- data.frame(regression_pred, reg_p)
    
    colnames(regression_) <- c("Age [Days]", "50% Regression", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Polynomial Regression (10)'))
  })
  
  output$regression_poly2 <- DT::renderDataTable({
    
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(poly_2_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    regression_ <- data.frame(regression_pred, reg_p)
    
    colnames(regression_) <- c("Age [Days]", "50% Regression", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Polynomial Regression (2)'))
  })
  
  output$regression_poly3 <- DT::renderDataTable({
    
    regression_pred <- seq(min(data_analyte()[,4]),max(data_analyte()[,4]))
    reg_p <- predict(poly_3_regression, newdata=data.frame(age_days=regression_pred), interval="prediction")
    regression_ <- data.frame(regression_pred, reg_p)
    
    colnames(regression_) <- c("Age [Days]", "50% Regression", "2.5% Prediction Interval", "97.5% Prediction Interval")
    DT::datatable(regression_, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Blfrtip', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')), 
                  caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Table: Polynomial Regression (3)'))
  })
  
  ##################################### Download ##################################################

  # Regular Window without Outlierdetection
  output$Download_window_data_all_outlier <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Regular_Window.csv")},
    content = function(file) {
      write.csv2(window_data_all_outlier, file, row.names = FALSE)})

  # Regular Window with RefLim
  output$Download_window_data_all_tukey <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Regular_Window_With_RefLim.csv")},
    content = function(file) {
      write.csv2(window_data_all_tukey, file, row.names = FALSE)})

  # Regular Window coupled Decision Tree without Outlierdetection
  output$Download_window_data_split_outlier <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Windowtree.csv")},
    content = function(file) {
      write.csv2(window_data_split_outlier, file, row.names = FALSE)})

  # Regular Window coupled Decision Tree with RefLim
  output$Download_window_data_split_tukey <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_Windowtree_With_RefLim.csv")},
    content = function(file) {
      write.csv2(window_data_split_tukey, file, row.names = FALSE)})

  # Regular Window coupled Decision Tree without Outlierdetection (Sliding-Windowmethod)
  output$Download_sliding <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_SlidingWindow.csv")},
    content = function(file) {
      write.csv2(slide, file, row.names = FALSE)})
  
  # Regular Window coupled Decision Tree with RefLim (Sliding-Windowmethod)
  output$Download_sliding_tukey <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_SlidingWindow_With_RefLim.csv")},
    content = function(file) {
      write.csv2(slide_tukey, file, row.names = FALSE)})
  
  # LIS Window-Method with no outlierdetection
  output$Download_lis_table_o <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_LIS.csv")},
    content = function(file) {
      write.csv2(window_data_lis_outlier, file, row.names = FALSE)})
  
  # LIS Window-Method with RefLim
  output$Download_lis_table_t <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_LIS_With_RefLim.csv")},
    content = function(file) {
      write.csv2(window_data_lis_tukey, file, row.names = FALSE)})
  
  # Discrete GAMLSS-Models
  output$Download_deviation_gamlss <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"_GAMLSS.csv")},
    content = function(file) {
      write.csv2(deviation_gamlss, file, row.names = FALSE)})


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

  output$Download_rsquared_table <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Window_Comparison.csv")},
    content = function(file) {
      write.csv2(rsquared_table, file, row.names = FALSE)})

  output$downloadData_comparison <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_GAMLSS_Comparison.csv")},
    content = function(file) {
      write.csv2(compare_models, file, row.names = FALSE)})

  output$downloadData_regression_table <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(),"_Regression_Comparison.csv")},
    content = function(file) {
      write.csv2(regression_table, file, row.names = FALSE)})

  ##################################### Plots #####################################################

  output$download_lms <- downloadHandler(
    filename =  function(){
      "LMS.eps"},
    content = function(file){
      setEPS()
      postscript(file)
      centiles(lms_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      dev.off()})
  
  output$download_lms_jpeg <- downloadHandler(
    filename =  function(){
      "LMS.jpeg"},
    content = function(file){
      jpeg(file)
      centiles(lms_, cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      dev.off()})
  
  output$download_gamlss <- downloadHandler(
    filename =  function(){
      "GAMLSS.eps"},
    content = function(file){
      setEPS()
      postscript(file)
      
      par(mfrow=c(3,2))
      
      centiles(pb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75, 
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(cs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]",
               ylab = ylab_, pch = 20, cex = 0.75, 
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent=c(2.5,50,97.5), xlab = "Age [Days]", 
               ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(nn_, main = "GAMLSS with Neural Network", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(tr_, main = "GAMLSS with Decision Tree", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      dev.off()})
  
  output$download_gamlss_jpeg <- downloadHandler(
    filename =  function(){
      "GAMLSS.jpeg"},
    content = function(file){
      jpeg(file)
      
      par(mfrow=c(3,2))
      
      centiles(pb_, main = "GAMLSS with P-Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75, 
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(cs_, main = "GAMLSS with Cubic Splines", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(poly_, main = "GAMLSS with Polynomials (Degree 3)", cent=c(2.5,50,97.5), xlab = "Age [Days]",
               ylab = ylab_, pch = 20, cex = 0.75, 
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(poly4_, main = "GAMLSS with Polynomials (Degree 4)",cent=c(2.5,50,97.5), xlab = "Age [Days]", 
               ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(nn_, main = "GAMLSS with Neural Network", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      centiles(tr_, main = "GAMLSS with Decision Tree", cent=c(2.5,50,97.5), xlab = "Age [Days]", ylab = ylab_, pch = 20, cex = 0.75,
               col.cent=c("indianred","black","cornflowerblue"), lty.centiles=c(3,1,3), lwd.centiles = 1.5, 
               legend = FALSE, col = "lightgrey")
      dev.off()})

  output$download_tree <- downloadHandler(
    filename =  function() {
      "Decision_Tree.eps"},
    content = function(file) {
      setEPS()
      postscript(file)
      build_rpart()
      rpart.plot(rpart_, box.palette = "RdBu", roundint = FALSE)
      dev.off()})
}  

####################################### Run the application #######################################
shinyApp(ui = ui, server = server)