library(shiny)
library(fpp3)
library(autoplotly)
library(shinythemes)
library(ggthemes)
library(ggdark)
library(shinycustomloader)
library(plotly)
library(shinyWidgets)

# Getting time series datasets from fpp3 package and the names of the datasets I want
D <- data(package = "fpp3")
names <- D$results[, "Title"]
dataSet <- D$results[, "Item"]
Names <- names[c(1, 3, 6, 8, 10, 11, 13)]



ui <- navbarPage(
    "Time Series Analysis",
    
    # Creating Home tab, and inserted a background image and title
    tabPanel(
        "Home",
        # Adding title and background image for home page
        absolutePanel(
            HTML("<h1> Time Series Analysis Project
         <br> by Justin Hairston </h1>"),
            top = "25vh",
            left = "5vw"
        ),
        
        tags$img(
            width = '100%',
            height = '95%',
            src = "https://wallpaperaccess.com/full/1325192.jpg",
            style = 'display: block; margin-left: auto; margin-right: auto;'
        ),
        icon =  icon("home")
        
    ),
    # Creating panel for viewing full, seasonal, and autocorrelation plots
    tabPanel(
        "Time Plots",
        
        # User will select a dataset
        sidebarPanel(
            selectizeInput("data", label = "Select a Dataframe",
                           choices = dataSet[c(1, 3, 6, 8, 10, 11, 13)]),
            
            # User will select a y variable
            pickerInput(
                inputId = 'var',
                label = 'Y Variable',
                choices = NULL,
                options = list(`style` = "btn-warning")
            ),
            
            # User can choose between seasonal or autocorrelation plot
            radioGroupButtons(
                inputId = "plotOptions",
                label = "Type of plot (for one on right)",
                choices = c("Seasonal", "Autocorr"),
                status = "primary",
                checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"),
                    no = icon("remove",
                              lib = "glyphicon")
                )
            ),
            
            
            # User can click here for instructions
            actionButton("show", "Help")
        ),
        
        # Plots will be displayed with dna loaders and manually positioned and sized
        absolutePanel(
            withLoader(plotOutput("timePlot"), type = "html",
                       loader = "dnaspin"),
            
            height = '50vh',
            width = '60vw',
            left = "35vw"
            
        ),
        br(),
        br(),
        
        absolutePanel(
            withLoader(
                plotlyOutput("timePlot2"),
                type = "html",
                loader = "dnaspin"
            ),
            top = "70vh",
            width = "100vw"
        ),
        
        icon = icon("fas fa-chart-bar")
    ),
    
    # tab for creating  decompostion plots for pre-selected y variables
    tabPanel(
        "Decomposition",
        
        fluidRow(
            width = 6,
            sidebarPanel(
                selectizeInput("data2", label = "Select a Dataframe",
                               choices = dataSet[c(1, 3, 6, 8, 10, 13)]),
                
                # User can choose between Additve or Multiplicative decomp.
                radioGroupButtons(
                    inputId = "plotOptions2",
                    label = "Type of Decomp.",
                    choices = c("Additive", "Multiplicative"),
                    status = "primary",
                    checkIcon = list(
                        yes = icon("ok",
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon")
                    )
                ),
                
                actionButton("show2", "Help")
            )
        ),
        
        
        withLoader(type = "html",
                   loader = "dnaspin",
                   
                   plotlyOutput("decomp")),
        
        icon = icon("fas fa-chart-line")
    ),
    
    # Tab for forecasting on datasets
    tabPanel(
        "Forecasting",
        
        fluidRow(
            width = 6,
            sidebarPanel(
                selectizeInput(
                    "data3",
                    label = "Simple Forecasts",
                    choices = c(
                        "Naive",
                        "Seasonal Naive",
                        "Mean",
                        "Drift",
                        "ETS-HOLTS",
                        "ETS-HOLTS/WINTER"
                    )
                ),
                
                actionButton("runForecast", "Run Forecast"),
                
                br(),
                br(),
                
                radioGroupButtons(
                    inputId = "plotOptions3",
                    label = "Select Forecasts Types",
                    choices = c("Simple", "ARIMA"),
                    status = "primary",
                    checkIcon = list(
                        yes = icon("ok",
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon")
                    )
                ),
                
                actionButton("show3", "Help")
            )
        ),
        
        
        withLoader(
            type = "html",
            loader = "dnaspin",
            
            plotOutput("predictionModel")
        ),
        
        withLoader(
            type = "html",
            loader = "dnaspin",
            
            plotOutput("predictionModel2")
        )
    ),
    
    # Other menu options
    navbarMenu(
        "Info",
        icon = icon("far fa-info-circle"),
        
        # Info about data sets will be shown here
        tabPanel(
            "Dataset Info",
            icon = icon("fas fa-database"),
            dataTableOutput("moreInfo")
        ),
        
        "----",
        # Interpretations for aus_arrival dataset will be shown here
        tabPanel(
            "Interpretation",
            icon = icon("book"),
            fluidRow(column(
                width = 6, sidebarPanel(
                    # User can choose what plot is shown
                    radioGroupButtons(
                        inputId = "plotOptions4",
                        label = "Type of plot",
                        choices = c("Full", "Seasonal", "Autocorr", "Additive", "Multiplicative"),
                        status = "primary",
                        checkIcon = list(
                            yes = icon("ok",
                                       lib = "glyphicon"),
                            no = icon("remove",
                                      lib = "glyphicon")
                        )
                    ),
                    
                    actionButton("show4", "Help")
                )
            )),
            
            # plot data for interpretations will be shown and positioned in middle
            absolutePanel(
                withLoader(
                    type = "html",
                    loader = "dnaspin",
                    plotOutput("interpretatedPlots")
                ),
                br(),
                
                width = "50vw",
                height = "40vh",
                left = "25vw",
                top = "10vh"
            ),
            
            # text will be generated using raw HTML and interpretations will change based off of what plot is chosen
            conditionalPanel(
                condition = "input.plotOptions4 == 'Full'",
                absolutePanel(
                    tags$div(
                        HTML(
                            "
               <font size =+2> <br> <center><strong> Full Plot Interpretation </strong> </center>
                <br> <p align = left> The full plot shows that Australia is seeing an upward trend of travelers coming from the U.S., NZ, and
                      the UK, but there seems to be a sharp decreasing trend in travelers from Japan. There also appears to be a very consistent
                                      pattern of peaks and troughs in the trends, indicating seasonality.</p> "
                        )
                    ),
                    width = "55vw",
                    height = "30vh",
                    top = "65vh",
                    left = "22vw"
                )
            ),
            
            conditionalPanel(
                condition = "input.plotOptions4 == 'Seasonal'",
                absolutePanel(
                    tags$div(
                        HTML(
                            "
               <font size =+2> <br> <center><strong> Seasonal Plot Interpretation </strong> </center>
                <br> <p align = left> The seasonal plot shows that travelers coming from the UK follow a seasonal pattern, where more
                                       travelers arrive to Australia in Q1 and then it slows down during Q2-Q3,
                                       then picks back up in Q4. Japan and the U.S. also show some slight seasonal patterns, with them slowing down during
                                       Q2. NZ seems to slow down during Q1 and then pick back up for the rest of the year. </p> "
                        )
                    ),
                    width = "55vw",
                    height = "25vh",
                    top = "65vh",
                    left = "21vw"
                )
            ),
            
            conditionalPanel(
                condition = "input.plotOptions4 == 'Autocorr'",
                absolutePanel(
                    tags$div(
                        HTML(
                            "
               <font size =+2> <br> <center><strong> AutoCorrelation Plot Interpretation </strong> </center>
                <br> <p align = left> The lags for each of the countries are all very high in the positive direction, indicating large positve correlations
                                       and statistical significance. This is most likely due to the strong trends and seasonal patterns in the data.
                                      This data will most likely need to be differenced, to detrend the data and take a closer look.</p> "
                        )
                    ),
                    width = "55vw",
                    height = "25vh",
                    top = "65vh",
                    left = "21vw"
                )
            ),
            
            conditionalPanel(
                condition = "input.plotOptions4 == 'Additive'",
                absolutePanel(
                    tags$div(
                        HTML(
                            "
               <font size =+2> <br> <center><strong> Additive Decomp. Plot Interpretation </strong> </center>
                <br> <p align = left> The trend plot shows that each country except Japan seem to be following a positive trend. The bar on the side
                                       is pretty small indicating that the trend plot explains a large amount of the data. The seasonal plot shows strong
                                       seasonality in each of the countries and the bar is very big, indicating that seasonality doesn't explain much of
                                       the data. The remainder seems to follow a random pattern indicating white noise. It also has a large bar, so it doesn't
                                       explain much of the data.</p> "
                        )
                    ),
                    width = "55vw",
                    height = "25vh",
                    top = "65vh",
                    left = "21vw"
                )
            ),
            
            conditionalPanel(
                condition = "input.plotOptions4 == 'Multiplicative'",
                absolutePanel(
                    tags$div(
                        HTML(
                            "
               <font size =+2> <br> <center><strong> Multiplicative Decomp. Plot Interpretation </strong> </center>
                <br> <p align = left>  The trend plot shows that NZ is the only country trending upwards while the rest are all trending down. The bar is not visible,
                                        meaning the trend plot must explain a great deal of the data. The seasonal plot much like the additive decomp. shows very strong
                                       seasonality in each of the countries, especially the UK, and the bar is very big, indicating that seasonality doesn't explain much of
                                       the data. The remainder also seems to follow a random pattern indicating white noise. It also has a large bar, so it doesn't
                                       explain much of the data.</p> "
                        )
                    ),
                    width = "55vw",
                    height = "25vh",
                    top = "65vh",
                    left = "21vw"
                )
            )
            
            
        )
    ),
    
    
    
    
    
    # Using cyborg theme for design
    theme = shinytheme("cyborg")
    
    
)
