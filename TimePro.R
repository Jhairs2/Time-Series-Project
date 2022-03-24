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
    "Plots",
    
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
        inputId = "plotOptions2",
        label = "Displayed Plot",
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
          inputId = "plotOptions3",
          label = "Displayed Plot",
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
        width = 6,  sidebarPanel(
          # User can choose what plot is shown
          radioGroupButtons(
            inputId = "plotOptions4",
            label = "Displayed Plot",
            choices = c("Full", "Seasonal", "Autocorr", "Additive", "Multiplicative"),
            status = "primary",
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon")
            )
          )
        )
      )),
      
      # plot data for interpretations will be shown and positioned in middle
      absolutePanel(
        withLoader(type = "html", loader = "dnaspin", plotOutput("test")),
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
                                       then picks back up in Q4. Japan also shows some slight seasonal patterns, with it slowing down during
                                       Q2.</p> "
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
                <br> <p align = left> The lags for each of the countries are all very high in the positive direction, indicating large positve correlation
                                       and statistical significance. This data also needs to differenced, to detrend the data..</p> "
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


server <- function(input, output, session) {
  # Manually putting help info, names, and sources of data sets into data table to show.
  output$moreInfo <- renderDataTable({
    TimeSeries <-
      c(
        "aus_accommodation",
        "aus_arrivals",
        "canadian_gas",
        "insurance",
        "souvenirs",
        "us_change",
        "us_gasoline"
      )
    Description <- c(
      "aus_accommodation is a quarterly 'tsibble' containing data on Australian tourist accommodation from short-term non-residential
  accommodation with 15 or more rooms, 1998 Q1 - 2016 Q2. The data set also contains the Australian Consumer Price Index (CPI) for the same period.
  Takings are in millions of Australian dollars, Occupancy is a percentage of rooms occupied, CPI is an index with value 100 in 2012 Q1.
",
      "Quarterly international arrivals to Australia from Japan, New Zealand, UK and the US. 1981Q1 - 2012Q3.",
      
      "Monthly Canadian gas production, billions of cubic metres, January 1960 - February 2005",
      
      "Monthly quotations and monthly television advertising expenditure for a US insurance company. January 2002 to April 2005",
      
      "Monthly sales for a souvenir shop on the wharf at a beach resort town in Queensland, Australia.",
      
      "us_change is a quarterly 'tsibble' containing percentage changes in quarterly personal consumption expenditure, personal disposable
  income, production, savings and the unemployment rate for the US, 1970 to 2016. Original $ values were in chained 2012 US dollars.",
      
      "Weekly data beginning Week 6, 1991, ending Week 3, 2017. Units are 'million barrels per day'."
    )
    Source <-
      c(
        "Australian Bureau of Statistics, Cat No 8635.0, Table 10, and Cat No 6401.0, Table 1.",
        "Tourism Research Australia.",
        "Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D., (2008) Forecasting with exponential
            smoothing: the state space approach, Springer.",
        "Kindly provided by Dave Reilly, Automatic Forecasting Systems.",
        "Makridakis, Wheelwright and Hyndman (1998) *Forecasting: methods and applications*,
            John Wiley & Sons: New York. Exercise 5.8.",
        "Federal Reserve Bank of St Louis.",
        "US Energy Information Administration."
      )
    
    dataInfo <- data.frame(Names, TimeSeries, Description, Source)
    dataInfo
  })
  
  # Getting data for full, seasonal, autocorr. plots
  plotData <- eventReactive(input$data, {
    Info <<- get(input$data)
  })
  # Getting data for decomp plots
  plotData2 <<- eventReactive(input$data2, {
    Info2 <<- get(input$data2)
  })
  
  # updating variable choices for input
  observeEvent(plotData(), {
    updatePickerInput(
      session,
      "var",
      choices = names(plotData()),
      selected = names(plotData()[length(plotData())])
    )
    
  })
  
  # Displaying seasonal and auto plots based off chosen option
  
  output$timePlot <- renderPlot({
    switch(
      input$plotOptions2,
      Autocorr = plotData() %>%
        ACF(Info[!!input$var]) %>%
        autoplot() + dark_theme_light() +
        ggtitle(names[which(D$results[, "Item"] == input$data)]) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = paste("ACF", input$var)),
      
      
      Seasonal = plotData() %>%
        fill_gaps(.full = TRUE) %>%
        gg_season(as.ts(Info[!!input$var])) + dark_theme_light() +
        ggtitle(names[which(D$results[, "Item"] == input$data)]) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = input$var)
    )
    
  })
  
  # Displaying full plot
  
  output$timePlot2 <- renderPlotly({
    plotData() %>%
      autoplot(as.ts(Info[!!input$var])) + dark_theme_light() +
      ggtitle(names[which(D$results[, "Item"] == input$data)]) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = input$var)
    
  })
  
  # Displaying additive and multiplicative decomp plots based off what dataset is chosen
  output$decomp <- renderPlotly({
    if (input$plotOptions3 == "Additive") {
      switch(
        input$data2,
        
        aus_accommodation = plotData2() %>%
          model (classical_decomposition(Takings, type = "additive")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Additive Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Takings = trend + seasonal + random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y = "Takings"),
        
        aus_arrivals = plotData2() %>%
          model (classical_decomposition(Arrivals, type = "additive")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Additive Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Arrivals = trend + seasonal + random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Arrivals"),
        
        canadian_gas = plotData2() %>%
          model (classical_decomposition(Volume, type = "additive")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Additive Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Volume = trend + seasonal + random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Volumes"),
        
        insurance = plotData2() %>%
          model (classical_decomposition(TVadverts, type = "additive")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Additive Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> TVadverts = trend + seasonal + random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "TVadverts"),
        
        souvenirs = plotData2() %>%
          model (classical_decomposition(Sales, type = "additive")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Additive Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Sales = trend + seasonal + random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Sales"),
        
        us_gasoline = plotData2() %>%
          model (classical_decomposition(Barrels, type = "additive")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Additive Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Barrels = trend + seasonal + random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Barrels")
      )
    }
    
    else{
      switch(
        input$data2,
        aus_accommodation = plotData2() %>%
          model (
            classical_decomposition(Takings, type = "multiplicative")
          ) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Multiplicative Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Takings = trend * seasonal * random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y = "Takings"),
        
        aus_arrivals = plotData2() %>%
          model (
            classical_decomposition(Arrivals, type = "multiplicative")
          ) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Multiplicative Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Arrivals = trend * seasonal * random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Arrivals"),
        
        canadian_gas = plotData2() %>%
          model (
            classical_decomposition(Volume, type = "multiplicative")
          ) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Multiplicative Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Volume = trend * seasonal * random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Volumes"),
        
        insurance = plotData2() %>%
          model (
            classical_decomposition(TVadverts, type = "multiplicative")
          ) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Multiplicative Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> TVadverts = trend * seasonal * random"
            )
          ) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "TVadverts"),
        
        souvenirs = plotData2() %>%
          model (classical_decomposition(Sales, type = "multiplicative")) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Multiplicative Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Sales = trend * seasonal * random"
            )
          )  +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Sales"),
        
        us_gasoline = plotData2() %>%
          model (
            classical_decomposition(Barrels, type = "multiplicative")
          ) %>%
          components() %>%
          autoplot() + dark_theme_light() +
          ggtitle(
            paste(
              "Multiplicative Decomp for",
              names[which(D$results[, "Item"] == input$data2)],
              "<br> Barrels = trend * seasonal * random"
            )
          )  +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(y  = "Barrels")
      )
    }
  })
  
  
  
  # Outputting plot based of Users choice for interpretations
  
  output$test <- renderPlot({
    switch (
      input$plotOptions4,
      
      
      Full = aus_arrivals %>%
        autoplot(Arrivals) + dark_theme_light() +
        ggtitle(Names[2]) +
        theme(plot.title = element_text(hjust = 0.5)),
      
      
      
      Seasonal = aus_arrivals %>%
        gg_season(Arrivals) + dark_theme_light() +
        ggtitle(Names[2]) +
        theme(plot.title = element_text(hjust = 0.5)),
      
      Autocorr = aus_arrivals %>%
        ACF(Arrivals) %>%
        autoplot() + dark_theme_light() +
        ggtitle(Names[2]) +
        theme(plot.title = element_text(hjust = 0.5)),
      
      Additive = aus_arrivals  %>%
        model (classical_decomposition(Arrivals, type = "additive")) %>%
        components() %>%
        autoplot() + dark_theme_light() +
        ggtitle(paste("Additive Decomp for",
                      Names[2])),
      
      Multiplicative =  aus_arrivals  %>%
        model (
          classical_decomposition(Arrivals, type = "multiplicative")
        ) %>%
        components() %>%
        autoplot() + dark_theme_light() +
        ggtitle(paste("Multiplicative Decomp for",
                      Names[2]))
      
    )
  })
  
  
  # Showing instructions for plot page
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Instructions",
      
      HTML(
        "<font size=+1>   <li> Select a time series from the list. </li>
                        <li> Choose a Y variable to look at and whether you want a seasonal or autocorrelation plot</li>
                        <li> A interactive full plot of time series will be shown below as well as the chosen seasonal or autocorrelation
                             plot on the top right. </li>
                        <li> On the full plot click the legend names to hide it from the plot </li>
                        <li> For more info on datasets check info tab </li> </font>"
        
      ),
      easyClose = TRUE,
      footer = NULL,
      fade = T
    ))
  })
  
  # Showing instructions for decomposition page
  observeEvent(input$show2, {
    showModal(modalDialog(
      title = "Instructions",
      
      HTML(
        "<font size=+1>
                      <li> Select a time series from the list </li>
                      <li> Choose what type of Decomp. plot you want </li>
                      <li> A interactive Decomp. plot of the series will be displayed below. </li>
                      <li> Click the legend names to hide it from the plot </li>
                      <li> For more info on datasets check info tab </li></font>
                      **** Y variables are already chosen for each decomp **** "
      ),
      easyClose = TRUE,
      footer = NULL,
      fade = T
    ))
  })
  
  
}


shinyApp(ui, server)