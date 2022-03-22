library(shiny)
library(fpp3)
library(autoplotly)
library(shinythemes)
library(ggthemes)
library(ggdark)
library(shinycustomloader)
library(plotly)
library(shinyWidgets)

# Getting time series datasets from fpp3 package
D <- data(package = "fpp3")
names <- D$results[, "Title"]
dataSet <- D$results[, "Item"]

ui <- navbarPage(
  "Time Series Analysis",
  
  # Creating Home tab, and inserted a background image and title
  tabPanel(
    "Home",
    
    absolutePanel(
      HTML("<h1> Time Series Analysis App
         <br> by Justin Hairston </h1>"),
      top = "25%",
      left = "5%"
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
    
    # User will select a data
    sidebarPanel(
      selectizeInput("data", label = "Select a Dataframe",
                     choices = dataSet[c(1, 3, 6, 8, 10, 11, 13)]),
      
      # User will select a why variable
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
      actionButton("flip", label = "toggle"),
      
      # Creating dropdown menu for changing plot themes and other plot settings
      tags$h6("Variable Settings"),
      br(),
      dropdown(
        title = ("Input Settings"),
        
        
        style = "unite",
        icon = icon("cog"),
        status = "danger",
        width = "300px",
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig
        )
      )
    ),
    
    
    
    # Plots will be displayed with dna loaders
    absolutePanel(
      withLoader(plotOutput("timePlot"), type = "html",
                 loader = "dnaspin"),
      
      height = '100%',
      width = '60%',
      left = "35%"
    ),
    
    absolutePanel(
      withLoader(
        plotlyOutput("timePlot2"),
        type = "html",
        loader = "dnaspin"
      ),
      top = "65%",
      width = "100%"
    ),
    
    icon = icon("fas fa-chart-bar")
  ),
  
  # tab for creating  decompostion plots for pre-selected variables
  tabPanel(
    "Decomposition",
    
    fluidRow(
      width = 6,
      sidebarPanel(
        selectizeInput("data2", label = "Select a Dataframe",
                       choices = dataSet[c(1, 3, 6, 8, 10, 13)]),
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
        
        tags$h6("Variable Settings"),
        br(),
        dropdown(
          title = ("Input Settings"),
          
          textInput(
            inputId = 'filter',
            label = 'Filter',
            placeholder = "type here"
          ),
          
          
          style = "unite",
          icon = icon("cog"),
          status = "danger",
          width = "300px",
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig
          )
        )
      )
    ),
    
 
      withLoader(type = "html",
                 loader = "dnaspin",
                 
                 plotlyOutput("decomp")),
    
    icon = icon("fas fa-chart-line")
  ),
  
  # Other menu options
  navbarMenu(
    "More",
    tabPanel("Help", icon = icon("question")),
    "----",
    tabPanel(
      "Other Feature",
      h2("Instructions"),
      HTML("<font size=+2> This app is for looking at time-series from the fpp3 R package in closer detail. Full, Seaasonal, Additive and
      Multiplicative Decomp., and autocorrelation plots will be available to view. For more info on data sets check the  help section
      in ***.</font> "),
    
      br(),
      br(),
      
      HTML(
        "
        <h4> Plots Tab </h4>
                  <ul> <font size=+2> 
                        <li> Select a time series from the list. </li>
                        <li> Choose a Y variable to look at and whether you want a seasonal or autocorrelation plot</li>
                        <li> A interactive full plot of time series will be shown below as well as the chosen seasonal or autocorrelation
                        plot.</li>
                        <li> In Plot settings menu, you can choose from a list and
                        change theme of plots </li>
                        </font> 
                  </ul>"
      ),
      br(),
      HTML(
        "<h4> Decomposition Tab </h4> <font size=+2> 
                  <ul>
                      <li> Select a time series from the list </li>
                      <li> Choose what type of Decomp. plot you want </li>
                      <li> A interactive Decomp. plot of the series(Decomp. variable is pre-selected) will be displayed below. </li>
                  </ul> </font>"
      ),
      
    ),
    "----",
    tabPanel("Interpretations")
  ),
  
  
  theme = shinytheme("cyborg")
  
  
)


server <- function(input, output, session) {
  output$timeplot <- renderUI({
    side <- if (input$flip1)
      "front"
    else
      "back"
  })
  
  observeEvent(input$flip, {
    updateFlipBox("flip1")
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
  
}

shinyApp(ui, server)