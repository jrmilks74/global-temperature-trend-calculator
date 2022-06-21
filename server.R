library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(readr)

# Data sets of average annual global temperature
GISS <- read_table("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt", 
                    skip = 7)
GISS <- GISS %>%
        select("Year", anomaly = "J-D")
GISS$Year <- as.numeric(GISS$Year)
GISS$anomaly <- as.numeric(GISS$anomaly)
GISS$anomaly <- GISS$anomaly/100
GISS <- GISS %>%
        na.omit()

NOAA <- read_csv("NOAA.csv",
                 col_types = cols(Year = col_number(),
                                  Value = col_number()), 
                 skip = 4)
NOAA <- NOAA %>%
        rename(anomaly = Value)

HadCRUT <- read_csv("HadCRUT5.0.1.csv")
HadCRUT <- HadCRUT %>%
        rename(Year = "Time", anomaly = "Anomaly (deg C)") %>%
        select(Year, anomaly)

shinyServer(function(input, output) {
    # Create graph of temperature data and linear regression fit
        
    temperature_selection <- reactive({
            if (input$temperature == "GISS")
                    GISS
            else if (input$temperature == "NOAA")
                    NOAA
            else
                    HadCRUT
    })
    
    output$tempPlot <- renderPlotly({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
        p <- ggplot(temperature_sub, aes(x = Year, y = anomaly)) +
            theme_bw() +
            geom_line(colour = "blue") +
            geom_smooth(method = "lm", formula = y ~ x) +
            labs(x = "Year",
                 y = "Temperature anomaly (ºC)",
                 title = "Linear trend in global mean temperature")
        ggplotly(p)
    })
    
    # Raw output of the linear regression analysis
    
    output$sum <- renderPrint({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            fit <- lm(anomaly~Year, data = temperature_sub)
            summary(fit)
    })
    
    # Calculate the linear trend between the start and end points
    
    output$trend <- renderText({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            fit <- lm(anomaly~Year, data = temperature_sub)
            round(100*fit$coefficients[2], 2)
    })
    
    output$confidence <- renderText({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            fit <- lm(anomaly~Year, data = temperature_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })

})
