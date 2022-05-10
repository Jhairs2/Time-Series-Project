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
    
    dataInfo <-
      data.frame(Names, TimeSeries, Description, Source)
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
  
  #Options for models will change based on input
  observeEvent (input$plotOptions3, {
    if (input$plotOptions3 == "ARIMA") {
      updateSelectizeInput(
        session,
        "data4",
        label = "ARIMA Forecasts",
        choices = c(
          "White Noise(Non-seasonal)",
          "Random Walk(Non-seasonal)",
          "Random Walk(Seasonal)",
          "Auto Arima"
        ),
        server = T
      )
    }
    
    else {
      updateSelectizeInput(
        session,
        "data4",
        label = "Simple Forecasts",
        choices = c(
          "Naive",
          "Seasonal Naive",
          "Mean",
          "Drift",
          "ETS-HOLTS",
          "ETS-HOLTS/WINTER"
        ),
        server = T
      )
    }
  })
  
  # Displaying seasonal and auto plots based off chosen option
  
  output$timePlot <- renderPlot({
    require(input$data)
    switch(
      input$plotOptions,
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
    require(input$data)
    plotData() %>%
      autoplot(as.ts(Info[!!input$var])) + dark_theme_light() +
      ggtitle(names[which(D$results[, "Item"] == input$data)]) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = input$var)
    
  })
  
  # Displaying additive and multiplicative decomp plots based off what dataset is chosen
  output$decomp <- renderPlotly({
    require(input$data2)
    if (input$plotOptions2 == "Additive") {
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
  
  # Creating button to run forecast models
  runButton <- eventReactive(input$runForecast, {
    input$data4
  })
  
  # Will create a plot of the dataset and forecast based of user choice
  output$predictionModel <- renderPlot({
    switch (
      input$data3,
      
      aus_accommodation = {
        switch (
          runButton(),
          
          Naive = aus_accommodation %>%
            model(NAIVE(Takings)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = aus_accommodation %>%
            model(SNAIVE(Takings)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = aus_accommodation %>%
            model(MEAN(Takings)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = aus_accommodation %>%
            model(RW(Takings ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = aus_accommodation %>%
            model(ETS(Takings ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = aus_accommodation %>%
            model(ETS(Takings ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        aus_accommodation %>%
          autoplot(Takings) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[1], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  aus_accommodation %>%
            model(ARIMA(Takings ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = aus_accommodation %>%
            model(ARIMA(Takings ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = aus_accommodation %>%
            model(ARIMA(Takings ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = aus_accommodation %>%
            model(ARIMA(Takings, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        aus_accommodation %>%
          autoplot(Takings) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[1], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
      },
      
      
      
      aus_arrivals = {
        switch (
          runButton(),
          
          Naive = aus_arrivals %>%
            model(NAIVE(Arrivals)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = aus_arrivals %>%
            model(SNAIVE(Arrivals)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = aus_arrivals %>%
            model(MEAN(Arrivals)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = aus_arrivals %>%
            model(RW(Arrivals ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = aus_arrivals %>%
            model(ETS(Arrivals ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = aus_arrivals %>%
            model(ETS(Arrivals ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        aus_arrivals %>%
          autoplot(Arrivals) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[2], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  aus_arrivals %>%
            model(ARIMA(Arrivals ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = aus_arrivals %>%
            model(ARIMA(Arrivals ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = aus_arrivals %>%
            model(ARIMA(Arrivals ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = aus_arrivals %>%
            model(ARIMA(Arrivals, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        aus_arrivals %>%
          autoplot(Arrivals) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[2], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
      },
      
      canadian_gas = {
        switch (
          runButton(),
          
          Naive = canadian_gas %>%
            model(NAIVE(Volume)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = canadian_gas %>%
            model(SNAIVE(Volume)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = canadian_gas %>%
            model(MEAN(Volume)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = canadian_gas %>%
            model(RW(Volume ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = canadian_gas %>%
            model(ETS(Volume ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = canadian_gas %>%
            model(ETS(Volume ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        canadian_gas %>%
          autoplot(Volume) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[3], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  canadian_gas %>%
            model(ARIMA(Volume ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = canadian_gas %>%
            model(ARIMA(Volume ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = canadian_gas %>%
            model(ARIMA(Volume ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = canadian_gas %>%
            model(ARIMA(Volume, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        canadian_gas %>%
          autoplot(Volume) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[3], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
      },
      
      insurance = {
        switch (
          runButton(),
          
          Naive = insurance %>%
            model(NAIVE(TVadverts)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = insurance %>%
            model(SNAIVE(TVadverts)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = insurance %>%
            model(MEAN(TVadverts)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = insurance %>%
            model(RW(TVadverts ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = insurance %>%
            model(ETS(TVadverts ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = insurance %>%
            model(ETS(TVadverts ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        insurance %>%
          autoplot(TVadverts) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[4], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  insurance %>%
            model(ARIMA(TVadverts ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = insurance %>%
            model(ARIMA(TVadverts ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = insurance %>%
            model(ARIMA(TVadverts ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = insurance %>%
            model(ARIMA(TVadverts, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        insurance %>%
          autoplot(TVadverts) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[4], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
      },
      
      souvenirs = {
        switch (
          runButton(),
          
          Naive = souvenirs %>%
            model(NAIVE(Sales)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = souvenirs %>%
            model(SNAIVE(Sales)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = souvenirs %>%
            model(MEAN(Sales)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = souvenirs %>%
            model(RW(Sales ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = souvenirs %>%
            model(ETS(Sales ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = souvenirs %>%
            model(ETS(Sales ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        souvenirs %>%
          autoplot(Sales) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[4], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  souvenirs %>%
            model(ARIMA(Sales ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = souvenirs %>%
            model(ARIMA(Sales ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = souvenirs %>%
            model(ARIMA(Sales ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = souvenirs %>%
            model(ARIMA(Sales, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        souvenirs %>%
          autoplot(Sales) + autolayer(fc) + dark_theme_light() +
          ggtitle(paste(Names[5], "With Forecasts")) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      
      
      
      
      
    )
    
    
  })
  
  # Will create a plot of the forecast selected to get a closer look
  output$predictionModel2 <- renderPlot({
    switch (
      input$data3,
      
      aus_accommodation = {
        switch (
          runButton(),
          
          Naive = aus_accommodation %>%
            model(NAIVE(Takings)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = aus_accommodation %>%
            model(SNAIVE(Takings)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = aus_accommodation %>%
            model(MEAN(Takings)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = aus_accommodation %>%
            model(RW(Takings ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = aus_accommodation %>%
            model(ETS(Takings ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = aus_accommodation %>%
            model(ETS(Takings ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  aus_accommodation %>%
            model(ARIMA(Takings ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = aus_accommodation %>%
            model(ARIMA(Takings ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = aus_accommodation %>%
            model(ARIMA(Takings ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = aus_accommodation %>%
            model(ARIMA(Takings, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
      },
      
      
      
      aus_arrivals = {
        switch (
          runButton(),
          
          Naive = aus_arrivals %>%
            model(NAIVE(Arrivals)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = aus_arrivals %>%
            model(SNAIVE(Arrivals)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = aus_arrivals %>%
            model(MEAN(Arrivals)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = aus_arrivals %>%
            model(RW(Arrivals ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = aus_arrivals %>%
            model(ETS(Arrivals ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = aus_arrivals %>%
            model(ETS(Arrivals ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  aus_arrivals %>%
            model(ARIMA(Arrivals ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = aus_arrivals %>%
            model(ARIMA(Arrivals ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = aus_arrivals %>%
            model(ARIMA(Arrivals ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = aus_arrivals %>%
            model(ARIMA(Arrivals, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
      },
      
      canadian_gas = {
        switch (
          runButton(),
          
          Naive = canadian_gas %>%
            model(NAIVE(Volume)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = canadian_gas %>%
            model(SNAIVE(Volume)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = canadian_gas %>%
            model(MEAN(Volume)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = canadian_gas %>%
            model(RW(Volume ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = canadian_gas %>%
            model(ETS(Volume ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = canadian_gas %>%
            model(ETS(Volume ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  canadian_gas %>%
            model(ARIMA(Volume ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = canadian_gas %>%
            model(ARIMA(Volume ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = canadian_gas %>%
            model(ARIMA(Volume ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = canadian_gas %>%
            model(ARIMA(Volume, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        
      },
      
      insurance = {
        switch (
          runButton(),
          
          Naive = insurance %>%
            model(NAIVE(TVadverts)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = insurance %>%
            model(SNAIVE(TVadverts)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = insurance %>%
            model(MEAN(TVadverts)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = insurance %>%
            model(RW(TVadverts ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = insurance %>%
            model(ETS(TVadverts ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = insurance %>%
            model(ETS(TVadverts ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  insurance %>%
            model(ARIMA(TVadverts ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = insurance %>%
            model(ARIMA(TVadverts ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = insurance %>%
            model(ARIMA(TVadverts ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = insurance %>%
            model(ARIMA(TVadverts, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
        
        
      },
      
      souvenirs = {
        switch (
          runButton(),
          
          Naive = souvenirs %>%
            model(NAIVE(Sales)) %>%
            forecast(h = 5) ->> fc,
          
          "Seasonal Naive" = souvenirs %>%
            model(SNAIVE(Sales)) %>%
            forecast(h = 5) ->> fc,
          
          Mean = souvenirs %>%
            model(MEAN(Sales)) %>%
            forecast(h = 5) ->> fc,
          
          Drift = souvenirs %>%
            model(RW(Sales ~ drift())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS" = souvenirs %>%
            model(ETS(Sales ~ trend())) %>%
            forecast(h = 5) ->> fc,
          
          "ETS-HOLTS/WINTER" = souvenirs %>%
            model(ETS(Sales ~ trend() + season())) %>%
            forecast(h = 5) ->> fc
        )
        
        
        switch (
          runButton(),
          
          "White Noise(Non-seasonal)" =  souvenirs %>%
            model(ARIMA(Sales ~ pdq(0, 0, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Non-seasonal)" = souvenirs %>%
            model(ARIMA(Sales ~ pdq(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          "Random Walk(Seasonal)" = souvenirs %>%
            model(ARIMA(Sales ~ pdq(0, 1, 0) + PDQ(0, 1, 0))) %>%
            forecast(h = 5) ->> fc,
          
          
          "Auto Arima" = souvenirs %>%
            model(ARIMA(Sales, stepwise = FALSE)) %>%
            forecast(h = 5) ->> fc
        )
      }
    )
    
    
    fc %>%
      autoplot() + dark_theme_light() +
      ggtitle(paste("Forecasts for", names[which(D$results[, "Item"] == input$data3)])) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Outputting plot based of Users choice for interpretations
  
  output$interpretatedPlots <- renderPlot({
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
  
  # instructions for forecast page
  observeEvent(input$show3, {
    showModal(modalDialog(
      title = "Instructions",
      
      HTML(
        "<font size=+1> This tab is made to show forecasts on the datasets from the
                          fpp3 package. Choose whether you want to look at simple forecasts models or ARIMA forecasts models.
                          Then, choose one of the models you would like to use and click Run Forecast. Once Run Forecast is clicked,
                          there will be two plots shown below: the top plot, which shows the original data and forecasted data together, and
                          the bottom plot, which shows the forecasted data only. All forecasts are predicting the next
                          5 periods (months, years, quarters, etc.). Y variables are pre-selected. For more info on datasets, look on info tab.
                          **Note: Auto Arima might take a while to load**.</font>"
        
      ),
      easyClose = TRUE,
      footer = NULL,
      fade = T
    ))
  })
  
  #instructions for interpretation page
  observeEvent(input$show4, {
    showModal(modalDialog(
      title = "Instructions",
      
      HTML(
        "<font size=+1>  This tab is made to show interpretations on the aus_arrival dataset from the
                          fpp3 package. There will be an interpretation done on the full, seasonal, autocorrelation,
                          additive decomp., and multiplicative decomp. plots of the time series. To see the different
                          interpretations and plots, just click on one of the button choices.</font>"
        
      ),
      easyClose = TRUE,
      footer = NULL,
      fade = T
    ))
  })
  
  
}