library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(forecast)
library(nlme)

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

NOAA <- read_csv("https://www.ncdc.noaa.gov/cag/global/time-series/globe/land_ocean/12/12/1880-2022/data.csv",
                 col_types = cols(Year = col_number(),
                                  Value = col_number()), 
                 skip = 4)
NOAA <- NOAA %>%
        rename(anomaly = Value)

HadCRUT <- read_csv("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/HadCRUT.5.0.1.0.analysis.summary_series.global.annual.csv")
HadCRUT <- HadCRUT %>%
        rename(Year = "Time", anomaly = "Anomaly (deg C)") %>%
        select(Year, anomaly)

RSS <- RSS <- read_table("https://images.remss.com/msu/graphics/TLT_v40/time_series/RSS_TS_channel_TLT_Global_Land_And_Sea_v04_0.txt", 
                          col_names = FALSE, skip = 17)
RSS <- RSS %>%
        rename(Year = X1, month = X2, anomaly = X3) %>%
        select(Year, anomaly)
RSS$anomaly <- na_if(RSS$anomaly, -99.9000)
RSS <- RSS %>%
        group_by(Year) %>%
        summarise(anomaly = mean(anomaly, na.rm = TRUE))

UAH <- read.table(text = paste0(head(readLines("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"), -12), collapse = "\n"), header = T)
UAH <- UAH %>%
        select(Year, Globe) %>%
        rename(anomaly = Globe) %>%
        filter(!row_number() %in% c(1)) %>%
        group_by(Year) %>%
        summarize(anomaly = mean(anomaly, na.rm = TRUE))

HadSST <- read_csv("https://www.metoffice.gov.uk/hadobs/hadsst4/data/csv/HadSST.4.0.1.0_annual_GLOBE.csv",
                   col_types = cols(year = col_number(),
                                    anomaly = col_number()))
HadSST <- HadSST %>%
        rename(Year = "year") %>%
        select(Year, anomaly)


sunspots <- read_table("https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt", col_names = FALSE)
sunspots <- sunspots %>%
        rename(Year = X1, number = X2) %>%
        select(Year, number)
sunspots$Year <- sunspots$Year - 0.5

CO2 <- read_table("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt", 
                   col_names = FALSE, skip = 57)
CO2 <- CO2 %>%
        rename(Year = X1, mean.CO2 = X2, uncertainty = X3)

ENSO <- read_table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
ENSO$Time <- seq.Date(from = as.Date("1950-01-01"), by = "month", length.out = nrow(ENSO))
ENSO$Status <- with(ENSO,
                    ifelse(ANOM >= 0.5, "El Niño",
                           ifelse(ANOM <= -0.5, "La Niña",
                                  "Neutral"
                                  )
                           )
                    )


shinyServer(function(input, output) {
    # Create graph of surface temperature data and linear regression fit
        
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
                geom_smooth(method = "loess", formula = y ~ x, aes(colour = "LOESS"), se = FALSE) +
                geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend")) +
            labs(x = "Year",
                 y = "Temperature anomaly (ºC)",
                 title = "Trend in global mean temperature")
        ggplotly(p)
    })
    
    # Raw output of the linear regression analysis of surface temperature
    
    output$sum <- renderPrint({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            start.year <- input$startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = temperature_sub)
            summary(fit)
    })
    
    # Calculate the linear trend between the start and end points for surface temperature
    
    output$trend <- renderText({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            start.year <- input$startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = temperature_sub)
            round(100*fit$coefficients[2], 2)
    })
    
    output$confidence <- renderText({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            start.year <- input$startdate
            fit <- lm(anomaly ~ I(Year -  start.year), data = temperature_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    # Create graph of satellite temperature data and linear regression fit
    
    sat_selection <- reactive({
            if (input$satellite == "RSS")
                    RSS
            else
                    UAH
    })
    
    output$sat_tempPlot <- renderPlotly({
            sat_sub <- subset(sat_selection(), sat_selection()$Year >= input$sat_startdate & sat_selection()$Year <= input$sat_enddate)
            sp <- ggplot(sat_sub, aes(x = Year, y = anomaly)) +
                    theme_bw() +
                    geom_line(colour = "blue") +
                    geom_smooth(method = "loess", formula = y ~ x, aes(colour = "LOESS"), se = FALSE) +
                    geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend")) +
                    labs(x = "Year",
                         y = "Temperature anomaly (ºC)",
                         title = "Trend in global mean temperature")
            ggplotly(sp)
    })
    
    # Raw output of the linear regression analysis of satellite temperature
    
    output$sat_sum <- renderPrint({
            sat_sub <- subset(sat_selection(), sat_selection()$Year >= input$sat_startdate & sat_selection()$Year <= input$sat_enddate)
            start.year <- input$sat_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = sat_sub)
            summary(fit)
    })
    
    # Calculate the linear trend between the start and end points for satellite temperature
    
    output$sat_trend <- renderText({
            sat_sub <- subset(sat_selection(), sat_selection()$Year >= input$sat_startdate & sat_selection()$Year <= input$sat_enddate)
            start.year <- input$sat_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = sat_sub)
            round(100*fit$coefficients[2], 2)
    })
    
    output$sat_confidence <- renderText({
            sat_sub <- subset(sat_selection(), sat_selection()$Year >= input$sat_startdate & sat_selection()$Year <= input$sat_enddate)
            start.year <- input$sat_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = sat_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    #HadSST plot
    output$ocean_tempPlot <- renderPlotly({
            ocean_sub <- subset(HadSST, Year >= input$ocean_startdate & Year <= input$ocean_enddate)
            sp <- ggplot(ocean_sub, aes(x = Year, y = anomaly)) +
                    theme_bw() +
                    geom_line(colour = "blue") +
                    geom_smooth(method = "loess", formula = y ~ x, aes(colour = "LOESS"), se = FALSE) +
                    geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend")) +
                    labs(x = "Year",
                         y = "Temperature anomaly (ºC)",
                         title = "Trend in ocean mean temperature")
            ggplotly(sp)
    })
    
    # Raw output of the linear regression analysis of ocean temperature
    
    output$ocean_sum <- renderPrint({
            ocean_sub <- subset(HadSST, Year >= input$ocean_startdate & Year <= input$ocean_enddate)
            start.year <- input$ocean_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = ocean_sub)
            summary(fit)
    })
    
    # Calculate the linear trend between the start and end points for ocean temperature
    
    output$ocean_trend <- renderText({
            ocean_sub <- subset(HadSST, Year >= input$ocean_startdate & Year <= input$ocean_enddate)
            start.year <- input$ocean_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = ocean_sub)
            round(100*fit$coefficients[2], 2)
    })
    
    output$ocean_confidence <- renderText({
            ocean_sub <- subset(HadSST, Year >= input$ocean_startdate & Year <= input$ocean_enddate)
            start.year <- input$ocean_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = ocean_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    #Sunspot numbers plot
    output$sun_plot <- renderPlotly({
            sun_sub <- subset(sunspots, Year >= input$sun_startdate & Year <= input$sun_enddate)
            sun_p <- ggplot(sun_sub, aes(x = Year, y = number)) +
                    theme_bw() +
                    geom_line(colour = "yellow") +
                    geom_smooth(method = loess, formula = y ~ x, aes(colour = "LOESS"), se = FALSE) +
                    geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend")) +
                    labs(x = "Year",
                         y = "Monthly mean sunspot number",
                         title = "Mean monthly sunspot number per year")
            ggplotly(sun_p)
            })
    
    #Sunspot regression fit
    output$sun_sum <- renderPrint({
            sun_sub <- subset(sunspots, sunspots$Year >= input$sun_startdate & sunspots$Year <= input$sun_enddate)
            start.year <- input$sun_startdate
            fit <- lm(number~I(Year - start.year), data = sun_sub)
            summary(fit)
            })
    
    #Sunspot trend and confidence interval
    output$sun_trend <- renderText({
            sun_sub <- subset(sunspots, sunspots$Year >= input$sun_startdate & sunspots$Year <= input$sun_enddate)
            start.year <- input$sun_startdate
            fit <- lm(number ~ I(Year - start.year), data = sun_sub)
            round(100*fit$coefficients[2], 2)
    })
    
    output$sun_confidence <- renderText({
            sun_sub <- subset(sunspots, sunspots$Year >= input$sun_startdate & sunspots$Year <= input$sun_enddate)
            start.year <- input$sun_startdate
            fit <- lm(number ~ I(Year - start.year), data = sun_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    #CO2 plot
    output$CO2_plot <- renderPlotly({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            CO2_p <- ggplot(CO2_sub, aes(x = Year, y = mean.CO2)) +
                    theme_bw() +
                    geom_line(colour = "black", size = 1) +
                    geom_smooth(method = loess, formula = y ~ x, aes(colour = "LOESS"), se = FALSE, size = 0.5) +
                    geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(colour = "Quadratic linear trend")) +
                    labs(x = "Year",
                         y = "Mean CO2 level (ppm)",
                         title = "Mean atmospheric CO2 levels per year")
            ggplotly(CO2_p)
    })
    
    #CO2 regression fit
    output$CO2_sum <- renderPrint({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$CO2_startdate
            fit <- lm(mean.CO2 ~ I(Year - start.year) + I((Year - start.year)^2), data = CO2_sub)
            summary(fit)
    })
    
    #Sunspot trend and confidence interval
    output$CO2_trend <- renderText({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$CO2_startdate
            fit <- lm(mean.CO2 ~ I(Year - start.year) + I((Year - start.year)^2), data = CO2_sub)
            10*round(fit$coefficients[2], 2)
    })
    
    output$CO2_confidence <- renderText({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$CO2_startdate
            fit <- lm(mean.CO2~I(Year - start.year) + I((Year - start.year)^2), data = CO2_sub)
            confidence <- confint(fit)
            c(10*round(confidence[2,1], 2), 10*round(confidence[2,2], 2))
    })
    
    #ENSO plot
    output$ENSO_plot <- renderPlotly({
            ENSO_sub <- subset(ENSO, YR >= input$ENSO_startdate & YR <= input$ENSO_enddate)
            ENSO_p <- ggplot(ENSO_sub, aes(x = Time, y = ANOM)) +
                    theme_bw() +
                    geom_path(aes(color = Status, group = 1)) +
                    geom_point(aes(color = Status), size = 0.5) +
                    geom_smooth(method = "loess", formula = y ~ x) +
                    scale_colour_manual(name = "Status", values = setNames(c("red", "black", "blue"), c("El Niño", "Neutral", "La Niña"))) +
                    labs(x = "Year",
                         y = "Sea surface temperature anomaly (ºC)",
                         title = "ENSO 3.4 sea surface temperature")
            ggplotly(ENSO_p)
    })
    
    #Sunspot regression fit
    output$ENSO_sum <- renderPrint({
            ENSO_sub <- subset(ENSO, YR >= input$ENSO_startdate & YR <= input$ENSO_enddate)
            fit <- lm(ANOM ~ Time, data = ENSO_sub)
            summary(fit)
    })
    
    #Sunspot trend and confidence interval
    output$ENSO_trend <- renderText({
            ENSO_sub <- subset(ENSO, YR >= input$ENSO_startdate & YR <= input$ENSO_enddate)
            fit <- lm(ANOM ~ Time, data = ENSO_sub)
            round(100*fit$coefficients[2], 2)
    })
    
    output$ENSO_confidence <- renderText({
            ENSO_sub <- subset(ENSO, YR >= input$ENSO_startdate & YR <= input$ENSO_enddate)
            fit <- lm(ANOM ~ Time, data = ENSO_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    
})
