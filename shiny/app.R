library(shiny)
library(marr)
library(DT)

# Define UI for application that draws a histogram
ui <- navbarPage("MaRR - Maximum Rank Reproducibility",
    tabPanel("Introduction",
             column(1),
             column(10, h4("Introduction"),
        p("Reproducibility is an on-going challenge with high-throughput 
          technologies that have been developed in the last two decades for 
          quantifying a wide range of biological processes. One of the main 
          difficulties faced by researchers is the variability of output across 
          replicate experiments (Li et al. (2011)). Several authors have 
          addressed the issue of reproducibility among high-throughput 
          experiments (Porazinska et al. (2010), Marioni et al. (2008), AC’t 
          Hoen et al. (2013)). In each high-throughput experiment (e.g., arrays,
          sequencing, mass spectrometry), a large number of features are 
          measured simultaneously, and candidates are often subjected for 
          follow-up statistical analysis. We use the term features to refer to 
          biological features (e.g., metabolites, genes) resulting from a 
          high-throughput experiment in the rest of this article. When 
          measurements show consistency across replicate experiments, we define 
          that measurement to be reproducible. Similarly, measurements that are 
          not consistent across replicates may be problematic and should be 
          identified. In this vignette, features that show consistency across 
          high-dimensional replicate experiments are termed reproducible and the
          ones that are not consistent are termed irreproducible. The 
          reproducibility of a high-throughput experiment primarily depends on 
          the technical variables, such as run time, technical replicates, 
          laboratory operators and biological variables, such as healthy and 
          diseased subjects. A critical step toward making optimal design 
          choices is to assess how these biological and technical variables 
          affect reproducibility across replicate experiments (Talloen et al. 
          (2010), Arvidsson et al. (2008)).")),
        column(1)
    
    ),
    
    tabPanel("Analysis",
        # Application title
        titlePanel("Analysis"),
    
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                tags$hr(),
                
                sliderInput("pSamplepairs", "P Sample Pairs",
                            min = 0, max = 1, value = 0.75),
                
                sliderInput("pFeatures", "P Features", 
                            min = 0, max = 1, value = 0.75),
                
                sliderInput("alpha", "Alpha", 
                            min = 0, max = 1, value = 0.05)
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
               tabsetPanel(
                   tabPanel("Data", DT::dataTableOutput("inData")),
                   tabPanel("Sample Pair per Feature", plotOutput("sperF")),
                   tabPanel("Features per Sample Pair", plotOutput("fperSP"))
               )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    inData <- reactive({
        validate(need(input$file1 != "", "\nPlease add a data set"))
        read.csv(input$file1$datapath)
    })

    output$inData <- DT::renderDataTable({
        #validate(need(input$file1 != "", "\nPlease add a data set"))
        #inData()[1:5, 1:10]
        DT::datatable(inData(), options = list(scrollX = TRUE))
    })
    
    output$sperF <- renderPlot({
        #validate(need(input$file1 != "", "\nPlease add a data set"))
        marrOutput <- marr(inData(), pSamplepairs = input$pSamplepairs,
                           pFeatures = input$pFeatures, alpha = input$alpha)
        
        marrPlotFeatures(marrOutput)
        
    })
    
    output$fperSP <- renderPlot({
        #validate(need(input$file1 != "", "\nPlease add a data set"))
        marrOutput <- marr(inData(), pSamplepairs = input$pSamplepairs,
                        pFeatures = input$pFeatures, alpha = input$alpha)
        marrPlotSamplepairs(marrOutput)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
