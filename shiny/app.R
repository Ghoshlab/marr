library(shiny)
library(shinyBS)
library(shinyjs)
library(marr)
library(MSPrep)
library(DT)
library(ggplot2)
library(dplyr)
library(rlang)

# Prepare example data using MSPrep
data(msquant)

summarizedDF <- msSummarize(msquant,
                            compVars = c("mz", "rt"),
                            sampleVars = c("spike", "batch", "replicate", 
                                           "subject_id"),
                            cvMax = 0.50,
                            minPropPresent = 1/3,
                            returnSummaryDetails = FALSE,
                            colExtraText = "Neutral_Operator_Dif_Pos_",
                            separator = "_",
                            missingValue = 1,
                            returnToSE = FALSE)

filteredDF <- msFilter(summarizedDF,
                       filterPercent = 0.8,
                       compVars = c("mz", "rt"),
                       sampleVars = c("spike", "batch", "subject_id"),
                       separator = "_",
                       returnToSE = FALSE)

bpcaImputedDF <- msImpute(filteredDF, imputeMethod = "bpca",
                          compVars = c("mz", "rt"),
                          sampleVars = c("spike", "batch", "subject_id"),
                          separator = "_",
                          returnToSE = FALSE,
                          missingValue = 0)

msprepData <- bpcaImputedDF %>%
    mutate_all(as.numeric)
msprepDataNoFeats <- bpcaImputedDF[, 3:20]
colnames(msprepData)[3:20] <- paste0("X", colnames(msprepData))[3:20]
colnames(msprepDataNoFeats) <- paste0("X", colnames(msprepDataNoFeats))

# 

# Define UI
ui <- navbarPage("MaRR - Maximum Rank Reproducibility",
    # Introduction Tab UI ------------------------------------------------------
    tabPanel("Introduction",
             column(1),
             column(10, 
                    withMathJax(),
            # tags$div(HTML("<script type='text/x-mathjax-config' >
            # MathJax.Hub.Config({
            # tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            # });
            # </script >
            # ")),
                    h4("Introduction"),
                    HTML("<p>This app provides a user interface for the open-source R package marr.
                         <b>Maximum rank reproducbility (MaRR)</b> is a nonparametric approach that detects reproducible signals using a maximal rank statistic for high-dimensional biological data.</p>"),
                    p("Instructions for using the app are provided as tooltips in 
                      the 'Example' tab. An interface to use the software with
                      uploaded data is provided in the 'Analysis' tab. For more
                      information about the MaRR method and marr software, see 
                      below."),
                    h4("About MaRR"),
                    p("Reproducibility is an on-going challenge with high-throughput
                    technologies that have been developed in the last two decades for
                    quantifying a wide range of biological processes. One of the main 
                    difficulties faced by researchers is the variability of output across 
                    replicate experiments (Li et al. (2011)). In each high-throughput experiment (e.g., arrays,
                    sequencing, mass spectrometry), a large number of features are 
                    measured simultaneously, and candidates are often subjected for 
                    follow-up statistical analysis. We use the term features to refer to 
                    biological features (e.g., metabolites, genes) resulting from a 
                    high-throughput experiment throughout this app. When 
                    measurements show consistency across replicate experiments, we define 
                    that measurement to be reproducible. Similarly, measurements that are 
                    not consistent across replicates may be problematic and should be 
                    identified. In this app, features that show consistency across 
                    high-dimensional replicate experiments are termed reproducible and the
                    ones that are not consistent are termed irreproducible. The 
                    reproducibility of a high-throughput experiment primarily depends on 
                    the technical variables, such as run time, technical replicates, 
                    laboratory operators and biological variables, such as healthy and 
                    diseased subjects. A critical step toward making optimal design 
                    choices is to assess how these biological and technical variables 
                    affect reproducibility across replicate experiments (Talloen et al. 
                      (2010), Arvidsson et al. (2008))."),
                    HTML("<p>In this app, we implement the procedure Philtron et al. (2018), referred to as maximum rank reproducibility (MaRR) to identify reproducible features in high-throughput replicate experiments. 
                         The marr procedure was originally proposed to assess reproducibility of gene ranks in replicate experiments.
                         This app implements the marr package (links below), which
                         calculates a matrix of signals with M rows (total number of features) and J = nCr(I, 2) columns, where I is the number of samples and J is the total number of sample pairs.
                         We assign feature m to be reproducible if a certain percentage of signals are reproducible for pairwise combinations of replicate experiments, i.e., if</p>"),
                    p("$$ \\frac{{\\sum_{i<i'}{{{r_{m,{(i,i')}}}}}}}{J} >c_s, c_s \\in (0,1).$$"),
                    HTML("<p>Similarly, we assign a sample pair (i, i') to be reproducible if a certain percentage signals are reproducible across all features, i.e., if</p>"),
                    p("$$\\frac{\\sum_{m}{{r{_{m,(i,i')}}}}}{M}>c_m, c_m \\in (0,1)$$"),
                    HTML("<p>The reproducible signal matrix is shown below.</p>"),
                    img(src='Marr_schematic.png', style="display: block; margin-left: auto; margin-right: auto; max-width: 747px; width: 100%; max-height: 421px; height: 100%;"),
                    h4("Further Information"),
                    HTML("<p>Link to MaRR paper (coming soon)</p>"),
                    HTML("<p><a href='https://bioconductor.org/packages/release/bioc/html/marr.html'>Link to Bioconductor</a></p>"),
                    HTML("<p><a href='https://github.com/Ghoshlab/marr'>Link to Github</a></p>")
                    ),
             column(1)
    
    ),
    
    # Example Tab UI -----------------------------------------------------------
    
    tabPanel("Example",
             column(1),
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("exampleDataSelect", "Choose CSV File",
                                  choices = list("Without Features" = FALSE,
                                                 "With Features" = TRUE)),
                     tags$hr(),
                     bsTooltip("exampleDataSelect",
                               paste0("Two example data sets are provided to d",
                                      "emonstrate MaRRs functionality. This f",
                                      "irst contains only abundance columns an",
                                      "d does not require any modification to the data options below. Th",
                                      "e second has two feature identifying ",
                                      "columns: mass-to-charge ratio (mz) and ",
                                      "retention time (rt). When using this da",
                                      "ta set, specify the mz and rt colum",
                                      "ns using the Feature Identifying Colum",
                                      "ns selection tool before clicking Run Analysis")),
                     actionButton("exampleRunAnalysis", "Run Analysis", 
                                  width = "100%"),
                     bsTooltip("exampleRunAnalysis",
                               paste0("Once all the MaRR Options and Data Options have been set according to your needs, click Run Analysis to apply MaRR to your data set.")),
                     tags$hr(),
                     h4("MaRR Options"),
                     sliderInput("examplePSamplepairs", "P Sample Pairs",
                                 min = 0, max = 1, value = 0.75),
                     bsTooltip("examplePSamplepairs",
                               paste0("A threshold value that lies between 0 a",
                                      "nd 1, used to assign a feature to be re",
                                      "producible based on the reproducibility",
                                      "output of the sample pairs per feature"),
                               "right"),
                     sliderInput("examplePFeatures", "P Features",
                                 min = 0, max = 1, value = 0.75),
                     bsTooltip("examplePFeatures",
                               paste0("A threshold value that lies between 0 a",
                                      "nd 1, used to assign a sample pair to b",
                                      "e reproducible based on the reproducibi",
                                      "lity output of the features per sample ",
                                      "pair"),
                               "right"),
                     sliderInput("exampleAlpha", "Alpha", 
                                 min = 0.01, max = 1, value = 0.05),
                     bsTooltip("exampleAlpha",
                               paste0("Level of significance to control the Fa",
                                      "lse Discovery Rate (FDR)"),
                               "right"),
                     tags$hr(),
                     h4("Data Options"), 
                     selectInput("exampleTranspose", "Data Format", 
                                 choices = c("Samples as Columns", 
                                             "Samples as Rows")),
                     bsTooltip("exampleTranspose",
                               paste0("Here you may specify whether your data set has samples as columns or samples as rows. Both example data sets have samples as rows so no change is needed, but you may find this feature useful when working with your data."),
                               "top"),
                     selectInput("exampleRowNames", label = NULL, 
                                 choices = c("No Row Names", 
                                             "Has Row Names")),
                     bsTooltip("exampleRowNames",
                               paste0("Some data sets may have a redundant leading column labeling each row, such as numbering column. This option is not needed for the example data sets, but you may choose Has Rows to exclude the first column."),
                               "top"),
                     varSelectInput("exampleFeatureVars", 
                                    "Feature Identifying Columns",
                                    data = data.frame(),
                                    multiple = TRUE), 
                     bsTooltip("exampleFeatureVars",
                               paste0("Here you may specify variables which identify features, such as mass-to-charge ratio, retention time, or feature name. These variables will be excluded from the analysis but will remain as identifiers of each feature. The example data set With Features has two feature identifying columns, mz and rt, which should be specified here."),
                               "top"),
                     varSelectInput("exampleExcludedVars", 
                                    "Exclude Samples",
                                    data = data.frame(), 
                                    multiple = TRUE), 
                     bsTooltip("exampleExcludedVars",
                               paste0("Here you may specify samples which you wish to exclude from analysis. When you select a sample name, it will be removed from the Data tab to the right. Use the backspace key to unselect columns."),
                               "top"),
                     width = 3
                 ),
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Data", 
                                  h1(), 
                                  DT::dataTableOutput("exampleInData")),
                         tabPanel("Sample Pairs",
                                  h4("Summary"),
                                  htmlOutput("exampleSummaryHTMLfPerS"),
                                  tags$hr(),
                                  h4("Distribution"),
                                  plotOutput("exampleFPerSP"),
                                  tags$hr(),
                                  h4("% Reproducible by Sample Pair"),
                                  DT::dataTableOutput("exampleSamplePairs")),
                         tabPanel("Features",
                                  h4("Summary"),
                                  htmlOutput("exampleSummaryHTMLsPerF"),
                                  tags$hr(),
                                  h4("Distribution"),
                                  plotOutput("exampleSPerF"),
                                  tags$hr(),
                                  h4("% Reproducible by Feature"),
                                  DT::dataTableOutput("exampleFeatures")),
                         tabPanel("Filtered Data",
                                  h4("Filtered by Features and Sample Pairs"),
                                  DT::dataTableOutput("exampleFilteredByBoth"),
                                  h1(),
                                  disabled(downloadButton("exampleDownloadFilteredByBoth",
                                                 paste0("Download data filtered by Sample Pairs and Features"))),
                                  tags$hr(),
                                  h4("Filtered by only Sample Pairs"),
                                  DT::dataTableOutput("exampleFilteredBySP"),
                                  h1(),
                                  disabled(downloadButton("exampleDownloadFilteredBySP",
                                                 paste0("Download data filtered by only Sample Pairs"))),
                                  tags$hr(),
                                  h4("Filtered by only Features"),
                                  DT::dataTableOutput("exampleFilteredByF"),
                                  h1(),
                                  disabled(downloadButton("exampleDownloadFilteredByF",
                                                 paste0("Download data filtered by only Features"))),
                                  tags$hr())
                     ),
                     width = 7
                 )
             ),
             column(1)
    ),
    
    # Analysis Tab UI ----------------------------------------------------------
    tabPanel("Analysis",
        column(1),
        sidebarLayout(
            sidebarPanel(
                shinyjs::useShinyjs(), 
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                tags$hr(),
                actionButton("runAnalysis", "Run Analysis", width = "100%"),
                tags$hr(),
                h4("MaRR Options"),
                sliderInput("pSamplepairs", "P Sample Pairs",
                            min = 0, max = 1, value = 0.75),
                sliderInput("pFeatures", "P Features",
                            min = 0, max = 1, value = 0.75),
                sliderInput("alpha", "Alpha", 
                            min = 0.01, max = 1, value = 0.05),
                tags$hr(),
                h4("Data Options"),
                selectInput("transpose", "Data Format", 
                            choices = c("Samples as Columns", 
                                        "Samples as Rows")),
                selectInput("rowNames", label = NULL, 
                            choices = c("No Row Names", 
                                        "Has Row Names")),
                varSelectInput("featureVars", 
                               "Feature Identifying Columns",
                               data = data.frame(),
                               multiple = TRUE), 
                varSelectInput("excludedVars", 
                               "Exclude Samples",
                               data = data.frame(), 
                               multiple = TRUE),
                
                width = 3
            ),
    
            mainPanel(
               tabsetPanel(
                   tabPanel("Data", 
                            h1(), 
                            DT::dataTableOutput("inData")),
                   tabPanel("Sample Pairs",
                            h4("Summary"),
                            htmlOutput("summaryHTMLfPerS"),
                            tags$hr(),
                            h4("Distribution"),
                            plotOutput("fPerSP"),
                            tags$hr(),
                            h4("% Reproducible by Sample Pair"),
                            DT::dataTableOutput("samplePairs")),
                   tabPanel("Features",
                            h4("Summary"),
                            htmlOutput("summaryHTMLsPerF"),
                            tags$hr(),
                            h4("Distribution"),
                            plotOutput("sPerF"),
                            tags$hr(),
                            h4("% Reproducible by Feature"),
                            DT::dataTableOutput("features")),
                   tabPanel("Filtered Data",
                            h4("Filtered by Features and Sample Pairs"),
                            DT::dataTableOutput("filteredByBoth"),
                            h1(),
                            disabled(downloadButton("downloadFilteredByBoth",
                                           paste0("Download data filtered by Sample Pairs and Features"))),
                            tags$hr(),
                            h4("Filtered by only Sample Pairs"),
                            DT::dataTableOutput("filteredBySP"),
                            h1(),
                            disabled(downloadButton("downloadFilteredBySP",
                                           paste0("Download data filtered by only Sample Pairs"))),
                            tags$hr(),
                            h4("Filtered by only Features"),
                            DT::dataTableOutput("filteredByF"),
                            h1(),
                            disabled(downloadButton("downloadFilteredByF",
                                  paste0("Download data filtered by only Features"))),
                            tags$hr())
               ),
               width = 7
            )
        ),
        column(1)
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Data Validation ----------------------------------------------------------
    # Disables Run Analysis button until data is uploaded (in Analysis tab)
    observe({
        file <- input$file1
        if (is.null(file) || file == "") {
            shinyjs::disable("runAnalysis")
        } else {
            shinyjs::enable("runAnalysis")
        }
    })
    
    # Disables download of filtered files until analysis is run
    observeEvent(input$runAnalysis, {
        shinyjs::enable("downloadFilteredByBoth")
        shinyjs::enable("downloadFilteredBySP")
        shinyjs::enable("downloadFilteredByF")
    })
    
    observeEvent(input$exampleRunAnalysis, {
        shinyjs::enable("exampleDownloadFilteredByBoth")
        shinyjs::enable("exampleDownloadFilteredBySP")
        shinyjs::enable("exampleDownloadFilteredByF")
    })
    
    validateMarrOutput <- function() {
        validate(need(input$file1 != "", "Please add data set"))
        validate(need(input$runAnalysis, "Please click Run Analysis"))
        
        validate(need(any(!(any(input$excludedVars %in% input$featureVars)),
                          length(input$excludedVars) == 0,
                          length(input$featureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
    }
    
    exampleValidateMarrOutput <- function() {
        validate(need(input$exampleRunAnalysis, "Please click Run Analysis"))
        
        validate(need(any(!(any(input$exampleExcludedVars %in% 
                                    input$exampleFeatureVars)),
                          length(input$exampleExcludedVars) == 0,
                          length(input$exampleFeatureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
    }
    
    # Data / variable prep -----------------------------------------------------
    # Get data from user
    inData <- reactive({
        validate(need(input$file1 != "", "\nPlease add a data set"))
        data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        
        # if (input$rowNames == "Has Row Names") {
        #     data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE,
        #                      row.names = 1)
        # } else {
        #     data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        # }
        
        if (input$rowNames == "Has Row Names") {
            data <- data[2:ncol(data)]
        }
        
        if(input$transpose == "Samples as Rows") {
            data <- data %>%
                t() %>%
                as.data.frame()
        }
        
        validate(need(!(any(is.na(data))),
                      "\nData cannot contain missing values"))
        
        data
    })
    
    exampleInData <- reactive({
        if (input$exampleDataSelect) {
            data <- msprepData
        } else {
            data <- msprepDataNoFeats
        }
        
        if (input$exampleRowNames == "Has Row Names") {
            data <- data[2:ncol(data)]
        }
        
        if(input$exampleTranspose == "Samples as Rows") {
            data <- data %>%
                t() %>%
                as.data.frame()
        }
        
        data
    })
    
    # Remove variables specified by "Excluded Vars", transpose if asked
    updatedInData <- reactive({
        if(length(input$excludedVars) != 0) {
            updatedData <- inData() %>% select(!(c(!!!input$excludedVars)))
        } else {
            updatedData <- inData()
        }
        
        updatedData
    })
    
    exampleUpdatedInData <- reactive({
        
        if(length(input$exampleExcludedVars) != 0) {
            exampleUpdatedInData <- 
                exampleInData() %>% select(!(c(!!!input$exampleExcludedVars)))
        } else {
            exampleUpdatedInData <- exampleInData()
        }
        
        exampleUpdatedInData
    })
    
    # These observe functions enable variable selection from data to remain
    #   up-to-date as users make changes
    observe({
        updateVarSelectInput(session, "excludedVars", data = inData())
    })
    
    observe({
        updateVarSelectInput(session, "exampleExcludedVars", 
                             data = exampleInData())
    })
    
    observe({
        updateVarSelectInput(session, "featureVars", data = inData())
    })
    
    observe({
        updateVarSelectInput(session, "exampleFeatureVars", 
                             data = exampleInData())
    })
    
    # Selection of Feature ID Variables is validated then manipulated to create
    #   a reactive variable which can be used as an argument in Marr
    featureVars <- reactive({
        validate(need(any(!(any(input$excludedVars %in% input$featureVars)),
                          length(input$excludedVars) == 0,
                          length(input$featureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
        
        if(length(input$featureVars) != 0) {
            featureVars <- unlist(strsplit(toString(input$featureVars), 
                                           split = ", "))
        } else {
            featureVars <- NULL
        }
        
    })
    
    exampleFeatureVars <- reactive({
        validate(need(any(!(any(input$exampleExcludedVars %in% 
                                    input$exampleFeatureVars)),
                          length(input$exampleExcludedVars) == 0,
                          length(input$exampleFeatureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
        
        if(length(input$exampleFeatureVars) != 0) {
            exampleFeatureVars <- 
                unlist(strsplit(toString(input$exampleFeatureVars), 
                                split = ", "))
        } else {
            exampleFeatureVars <- NULL
        }
        
    })
    
    # Run main Marr function ---------------------------------------------------
    marrOutput <- eventReactive(input$runAnalysis, {
        #validate(need(input$file1 != "", "\nPlease add a data set"))
        marrOutput <- Marr(updatedInData(), pSamplepairs = input$pSamplepairs,
                           pFeatures = input$pFeatures, alpha = input$alpha,
                           featureVars = featureVars())
    })
    
    exampleMarrOutput <- eventReactive(input$exampleRunAnalysis, {
        exampleMarrOutput <- Marr(exampleUpdatedInData(), 
                                  pSamplepairs = input$examplePSamplepairs,
                                  pFeatures = input$examplePFeatures, 
                                  alpha = input$exampleAlpha,
                                  featureVars = exampleFeatureVars())
    })
    
    # Render inData tables -----------------------------------------------------
    output$inData <- DT::renderDataTable({
        data <- updatedInData() %>%
            mutate_if(is.numeric, round, digits = 2)
        DT::datatable(data, options = list(scrollX = TRUE,
                                           pageLength = 15),
                      rownames = FALSE)
    })
    
    output$exampleInData <- DT::renderDataTable({
        data <- exampleUpdatedInData() %>%
            mutate_if(is.numeric, round, digits = 2)
        DT::datatable(data, options = list(scrollX = TRUE,
                                           pageLength = 15),
                      rownames = FALSE)
    })
    
    # Render sample pairs table ------------------------------------------------
    samplePairData <- eventReactive(input$runAnalysis, {
        samplePairData <- marrOutput() %>%
            MarrSamplepairs() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$samplePairs <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(samplePairData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    exampleSamplePairData <- eventReactive(input$exampleRunAnalysis, {
        exampleSamplePairData <- exampleMarrOutput() %>%
            MarrSamplepairs() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleSamplePairs <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleSamplePairData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    # Render features tables ---------------------------------------------------
    featureData <- eventReactive(input$runAnalysis, {
        featureData <- marrOutput() %>%
            MarrFeatures() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$features <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(featureData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    exampleFeatureData <- eventReactive(input$exampleRunAnalysis, {
        exampleFeatureData <- exampleMarrOutput() %>%
            MarrFeatures() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleFeatures <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleFeatureData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    # Render filtered data tables ----------------------------------------------
    dataFilteredByBoth <- reactive({
        filteredData <- marrOutput() %>%
            MarrFilterData(by = "both") %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$filteredByBoth <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(dataFilteredByBoth(),
                      #extensions = "Buttons",
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                                     #dom = "ti<>B",
                                     #buttons = c("csv", "excel")),
                      rownames = FALSE)},
        server = FALSE,
    )
    
    exampleDataFilteredByBoth <- reactive({
        filteredData <- exampleMarrOutput() %>%
            MarrFilterData(by = "both") %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleFilteredByBoth <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleDataFilteredByBoth(),
                      #extensions = "Buttons",
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      #dom = "ti<>B",
                      #buttons = c("csv", "excel")),
                      rownames = FALSE)},
        server = FALSE,
    )
    
    # Data filtered by only sample pairs
    dataFilteredBySP <- reactive({
        filteredData <- marrOutput() %>%
            MarrFilterData(by = "samplePairs") %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$filteredBySP <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(dataFilteredBySP(), options = list(scrollX = TRUE,
                                                         pageLength = 3,
                                                         searching = FALSE),
                      rownames = FALSE)
    })
    
    exampleDataFilteredBySP <- reactive({
        filteredData <- exampleMarrOutput() %>%
            MarrFilterData(by = "samplePairs") %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleFilteredBySP <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleDataFilteredBySP(), options = list(scrollX = TRUE,
                                                         pageLength = 3,
                                                         searching = FALSE),
                      rownames = FALSE)
    })
    
    # Data filtered by only features
    dataFilteredByF <- reactive({
        filteredData <- marrOutput() %>%
            MarrFilterData(by = "feature") %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$filteredByF <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(dataFilteredByF(), options = list(scrollX = TRUE,
                                                        pageLength = 3,
                                                        searching = FALSE),
                      rownames = FALSE)
    })
    
    exampleDataFilteredByF <- reactive({
        filteredData <- exampleMarrOutput() %>%
            MarrFilterData(by = "feature") %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleFilteredByF <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleDataFilteredByF(), options = list(scrollX = TRUE,
                                                        pageLength = 3,
                                                        searching = FALSE),
                      rownames = FALSE)
    })
    
    # Example table tooltips
    addTooltip(session, "exampleInData", #"Uploaded Data",
               title = paste0("Here your uploaded data will be displayed for you to examine prior to analyzing it with MaRR. Note that samples selected with Exclude Samples will disappear from this table, but those selected with Feature Identifying Columns will remain."),
               placement = "bottom")
    
    addTooltip(session, "exampleSamplePairs", #"Uploaded Data",
               title = paste0("Here, each sample pair's reproducibility is given. For each sample pair, its reproducibility is determined as the percentage of features per sample pair which are reproducible according to the MaRR procedure based on the chosen Alpha."),
               placement = "top")
    
    addTooltip(session, "exampleFeatures", #"Uploaded Data",
               title = paste0("Here, each features's reproducibility is given. For each feature, its reproducibility is determined as the percentage of sample pairs per feature which are reproducible according to the MaRR procedure based on the chosen Alpha."),
               placement = "top")
    
    addTooltip(session, "exampleFilteredByBoth", #"Uploaded Data",
               title = paste0("This table displays data after both its samples and features have been filtered according to the MaRR procedure. For further explanation on when a feature or sample pair is filtered, see the tooltips below. You may download this table with the button directly below it."),
               placement = "bottom")
    
    addTooltip(session, "exampleFilteredBySP", #"Uploaded Data",
               title = paste0("This table displays data after its samples have been filtered. A sample is removed if its reproducibility is below P Sample Pairs for ALL sample pairs in which it is a member. In other words, a sample that's reproducibility is above the threshold P Sample Pairs when paired with any other sample will not be removed. You may download this table with the button directly below it."),
               placement = "top")
    
    addTooltip(session, "exampleFilteredByF", #"Uploaded Data",
               title = paste0("This table displays data after its features have been filtered. A feature is removed if its reproducibility is below P Features. You may download this table with the button directly below it."),
               placement = "top")
    
    # Download Button Helpers --------------------------------------------------
    output$downloadFilteredByBoth <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(dataFilteredByBoth(), file, row.names = FALSE)
        }
    )
    
    output$downloadFilteredBySP <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(dataFilteredBySP(), file, row.names = FALSE)
        }
    )
    
    output$downloadFilteredByF <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(dataFilteredByF(), file, row.names = FALSE)
        }
    )
    
    output$exampleDownloadFilteredByBoth <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(exampleDataFilteredByBoth(), file, row.names = FALSE)
        }
    )
    
    output$exampleDownloadFilteredBySP <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(exampleDataFilteredBySP(), file, row.names = FALSE)
        }
    )
    
    output$exampleDownloadFilteredByF <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(exampleDataFilteredByF(), file, row.names = FALSE)
        }
    )
    
    # Render text --------------------------------------------------------------
    samplePairsFiltered <- eventReactive(input$runAnalysis, {
        samplePairsFiltered <- marrOutput() %>%
            MarrSamplepairsfiltered()
    })
    
    output$summaryHTMLfPerS <- renderText({
        validateMarrOutput()
        paste0("Percent of sample pairs with ",
               "greater than ", input$pSamplepairs*100, "% ",
               " reproducible features: <b>", 
               round(samplePairsFiltered(), 2), "%</b>")
    })
    
    exampleSamplePairsFiltered <- eventReactive(input$exampleRunAnalysis,{
        exampleSamplePairsFiltered <- exampleMarrOutput() %>%
            MarrSamplepairsfiltered()
    })
    
    output$exampleSummaryHTMLfPerS <- renderText({
        exampleValidateMarrOutput()
        paste0("Percent of sample pairs with ",
               "greater than ", input$pSamplepairs*100, "% ",
               " reproducible features: <b>", 
               round(exampleSamplePairsFiltered(), 2), "%</b>")
    })
    
    featuresFiltered <- eventReactive(input$runAnalysis, {
        featuresFiltered <- marrOutput() %>%
            MarrFeaturesfiltered()
    })
    
    output$summaryHTMLsPerF <- renderText({
        validateMarrOutput()
        paste0("Percent of features with ",
               "greater than ", input$pFeatures*100, "% ",
               " reproducible sample pairs: <b>", 
               round(featuresFiltered(), 2), "%</b>")
    })
    
    exampleFeaturesFiltered <- eventReactive(input$exampleRunAnalysis, {
        exampleFeaturesFiltered <- exampleMarrOutput() %>%
            MarrFeaturesfiltered()
    })
    
    output$exampleSummaryHTMLsPerF <- renderText({
        exampleValidateMarrOutput()
        paste0("Percent of features with ",
               "greater than ", input$examplePFeatures*100, "% ",
               " reproducible sample pairs: <b>", 
               round(exampleFeaturesFiltered(), 2), "%</b>")
    })
    
    # Example text tooltips
    addTooltip(session, "exampleSummaryHTMLfPerS", 
               title = paste0("Here the percentage of reproducible sample pairs, as determined by P Samples Pairs and Alpha, is given."))
    
    addTooltip(session, "exampleSummaryHTMLsPerF", 
               title = paste0("Here the percentage of reproducible features, as determined by P Features and Alpha, is given."))
    
    # Render plots -------------------------------------------------------------
    output$fPerSP <- renderPlot({
        validateMarrOutput()
        MarrPlotSamplepairs(marrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$pSamplepairs*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$pSamplepairs*100 + 2, y = Inf,
                     label = "P Sample Pairs", color = "red", angle = 90,
                     hjust = 1.2)
    })
    
    output$exampleFPerSP <- renderPlot({
        exampleValidateMarrOutput()
        MarrPlotSamplepairs(exampleMarrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$examplePSamplepairs*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$examplePSamplepairs*100 + 2, 
                     y = Inf, label = "P Sample Pairs", color = "red", 
                     angle = 90, hjust = 1.2)
    })
    
    output$sPerF <- renderPlot({
        validateMarrOutput()
        MarrPlotFeatures(marrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$pFeatures*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$pFeatures*100 + 2, y = Inf,
                     label = "P Features", color = "red", angle = 90,
                     hjust = 1.2)
    })
    
    output$exampleSPerF <- renderPlot({
        exampleValidateMarrOutput()
        MarrPlotFeatures(exampleMarrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$examplePFeatures*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$examplePFeatures*100 + 2, 
                     y = Inf, label = "P Features", color = "red", angle = 90, 
                     hjust = 1.2)
    })
    
    # Example plot tooltips
    addTooltip(session, "exampleFPerSP",
               paste0("The above plot shows the distribution of sample pairs according to their respective features per sample pair reproducibility."))
    
    addTooltip(session, "exampleSPerF", #"Sample Pair per Feature",
               paste0("The above plot shows the distribution of features according to each their respective sample pairs per feature reproducibility."))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
