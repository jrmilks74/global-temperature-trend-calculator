library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(forecast)
library(nlme)
library(naniar)

# Data sets of average annual global temperature
GISS <- read_table("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt", 
                    skip = 7) %>%
        filter(!row_number() %in% c(22,43, 64, 85, 106, 127, 148)) %>%
        filter(row_number() <= n() - 5)
GISS <- GISS %>%
        select("Year", anomaly = "J-D") %>%
        mutate(anomaly = na_if(anomaly, "****"))
GISS$Year <- as.numeric(GISS$Year)
GISS$anomaly <- as.numeric(GISS$anomaly)
GISS$anomaly <- GISS$anomaly/100
GISS <- GISS %>%
        na.omit()

HadCRUT <- read_csv("https://www.metoffice.gov.uk/hadobs/crutem5/data/CRUTEM.5.0.2.0/diagnostics/CRUTEM.5.0.2.0.summary_series.global.annual.csv")
HadCRUT <- HadCRUT %>%
        rename(Year = "Time", anomaly = "Anomaly (deg C)") %>%
        select(Year, anomaly)

BEST <- read_table("https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt",
                   col_names = FALSE, skip = 86) %>%
        rename(Year = "X1", month = "X2", anomaly = "X3") %>%
        select(Year, anomaly) %>%
        group_by(Year) %>%
        summarize(anomaly = mean(anomaly, na.rm = TRUE)) %>%
        filter(row_number() <= n() - 1)

# Satelite data sets
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

# Sea surface temperatures
HadSST <- read_csv("https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/HadSST.4.1.1.0_annual_GLOBE.csv",
                   col_types = cols(year = col_number(),
                                    anomaly = col_number()))
HadSST <- HadSST %>%
        rename(Year = "year") %>%
        select(Year, anomaly)

# Sunspot data
sunspots <- read_table("https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt", col_names = FALSE)
sunspots <- sunspots %>%
        rename(Year = X1, Solar = X2) %>%
        select(Year, Solar)
sunspots$Year <- sunspots$Year - 0.5

# Solar output
irradiance <- read_table("http://www2.mps.mpg.de/projects/sun-climate/data/SATIRE/SATIRE-S/SATIRE-S_TSI_latest.txt", 
           col_names = FALSE, 
           skip = 34) %>%
        rename(time = X1, 
               irradiance = X2) %>%
        select(time, irradiance) %>%
        mutate(daily = as.Date((time - 2396759), 
                               origin = as.Date("1850-01-01"))) %>%
        select(daily, irradiance) %>%
        mutate(Year = floor_date(daily, "year")) %>%
        mutate(Year = year(Year)) %>%
        group_by(Year) %>%
        summarize(irradiance = mean(irradiance, na.rm = TRUE)) %>%
        rename(Solar =  irradiance)

# CO2 annual data
CO2 <- read_table("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt", 
                   col_names = FALSE, skip = 45)
CO2 <- CO2 %>%
        rename(Year = X1, mean.CO2 = X2, uncertainty = X3)

# El Nino/Southern Oscillation data
ENSO <- read_table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
ENSO$Time <- seq.Date(from = as.Date("1950-01-01"), by = "month", length.out = nrow(ENSO))
ENSO$Status <- with(ENSO,
                    ifelse(ANOM > 0.5, "El Niño",
                           ifelse(ANOM < -0.5, "La Niña",
                                  "Neutral"
                                  )
                           )
                    )

# Sea Ice
## Arctic sea ice
Arctic_sea_ice <- read_table("https://psl.noaa.gov/data/timeseries/monthly/data/n_iceextent.mon.data", col_names = FALSE, skip = 1) %>%
        head(-11) %>%
        mutate_if(is.character, as.numeric) %>%
        rename(year = X1,
               January = X2,
               February = X3, 
               March = X4, 
               April = X5, 
               May = X6, 
               June = X7,
               July = X8,
               August = X9,
               September = X10,
               October = X11,
               November = X12,
               December = X13) %>%
        pivot_longer(!year, names_to = "month", values_to = "extent") %>%
        replace_with_na(replace = list(extent = -99.99))

Arctic_sea_ice$month <- match(Arctic_sea_ice$month, month.name)  %>%
        as.numeric()

Arctic_sea_ice$year_month <- make_date(year = Arctic_sea_ice$year, month = Arctic_sea_ice$month, day = 1)
Arctic_sea_ice$decimal.date <- Arctic_sea_ice$year + (Arctic_sea_ice$month - 1) / 12
Arctic_sea_ice$extent <- Arctic_sea_ice$extent*100000

## Antarctic sea ice data
Antarctic_sea_ice <- read_table("https://psl.noaa.gov/data/timeseries/monthly/data/s_iceextent.mon.data", col_names = FALSE, skip = 1) %>%
        head(-11) %>%
        mutate_if(is.character, as.numeric) %>%
        rename(year = X1,
               January = X2,
               February = X3, 
               March = X4, 
               April = X5, 
               May = X6, 
               June = X7,
               July = X8,
               August = X9,
               September = X10,
               October = X11,
               November = X12,
               December = X13) %>%
        pivot_longer(!year, names_to = "month", values_to = "extent") %>%
        replace_with_na(replace = list(extent = -99.99))

Antarctic_sea_ice$month <- match(Antarctic_sea_ice$month, month.name)  %>%
        as.numeric()

Antarctic_sea_ice$year_month <- make_date(year = Antarctic_sea_ice$year, month = Antarctic_sea_ice$month, day = 1)
Antarctic_sea_ice$decimal.date <- Antarctic_sea_ice$year + (Antarctic_sea_ice$month - 1) / 12
Antarctic_sea_ice$extent <- Antarctic_sea_ice$extent*100000

# Server function
shinyServer(function(input, output) {
    # Create graph of surface temperature data and linear regression fit
        
    temperature_selection <- reactive({
            if (input$temperature == "GISS")
                    GISS
            else if (input$temperature == "BEST")
                    BEST
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
            trend <- round(100*fit$coefficients[2], 2)
            trend <- paste(trend, "ºC", sep = "")
            trend
    })
    
    output$confidence <- renderText({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            start.year <- input$startdate
            fit <- lm(anomaly ~ I(Year -  start.year), data = temperature_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    output$total_change <- renderText({
            temperature_sub <- subset(temperature_selection(), temperature_selection()$Year >= input$startdate & temperature_selection()$Year <= input$enddate)
            start.year <- input$startdate
            loess_fit <- loess(anomaly ~ I(Year - start.year), data = temperature_sub)
            total <- tail(loess_fit$fitted)[6] - head(loess_fit$fitted)[1]
            total <- round(total, 1)
            total <- paste(total, "ºC", sep = "")
            total
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
            sat_trend <- round(100*fit$coefficients[2], 2)
            sat_trend <- paste(sat_trend, "ºC", sep = "")
            sat_trend
    })
    
    output$sat_confidence <- renderText({
            sat_sub <- subset(sat_selection(), sat_selection()$Year >= input$sat_startdate & sat_selection()$Year <= input$sat_enddate)
            start.year <- input$sat_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = sat_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    output$sat_total_change <- renderText({
            sat_sub <- subset(sat_selection(), sat_selection()$Year >= input$startdate & sat_selection()$Year <= input$enddate)
            start.year <- input$startdate
            sat_loess_fit <- loess(anomaly ~ I(Year - start.year), data = sat_sub)
            sat_total <- tail(sat_loess_fit$fitted)[6] - head(sat_loess_fit$fitted)[1]
            sat_total <- round(sat_total, 1)
            sat_total <- paste(sat_total, "ºC", sep = "")
            sat_total
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
            ocean_trend <- round(100*fit$coefficients[2], 2)
            ocean_trend <- paste(ocean_trend, "ºC", sep = "")
            ocean_trend
    })
    
    output$ocean_confidence <- renderText({
            ocean_sub <- subset(HadSST, Year >= input$ocean_startdate & Year <= input$ocean_enddate)
            start.year <- input$ocean_startdate
            fit <- lm(anomaly ~ I(Year - start.year), data = ocean_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    output$ocean_total_change <- renderText({
            ocean_sub <- subset(HadSST, Year >= input$startdate & Year <= input$enddate)
            start.year <- input$startdate
            ocean_loess_fit <- loess(anomaly ~ I(Year - start.year), data = ocean_sub)
            ocean_total <- tail(ocean_loess_fit$fitted)[6] - head(ocean_loess_fit$fitted)[1]
            ocean_total <- round(ocean_total, 1)
            ocean_total <- paste(ocean_total, "ºC", sep = "")
            ocean_total
            
    })
    
    #Sunspot numbers plot
    solar_selection <- reactive({
            if (input$solar_data == "sunspots")
                    sunspots
            else
                    irradiance
    })
    
    output$sun_plot <- renderPlotly({
            sun_sub <- subset(solar_selection(), Year >= input$sun_startdate & Year <= input$sun_enddate)
            sun_p <- ggplot(sun_sub, aes(x = Year, y = Solar)) +
                    theme_bw() +
                    geom_line(colour = "yellow") +
                    geom_smooth(method = loess, formula = y ~ x, aes(colour = "LOESS"), se = FALSE) +
                    geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend")) +
                    labs(x = "Year",
                         y = "Mean solar output (Watts per square meter)",
                         title = "Mean solar output per year")
            ggplotly(sun_p)
            })
    
    #Sunspot regression fit
    output$sun_sum <- renderPrint({
            sun_sub <- subset(solar_selection(), solar_selection()$Year >= input$sun_startdate & solar_selection()$Year <= input$sun_enddate)
            start.year <- input$sun_startdate
            fit <- lm(Solar~I(Year - start.year), data = sun_sub)
            summary(fit)
            })
    
    #Sunspot trend and confidence interval
    output$sun_trend <- renderText({
            sun_sub <- subset(solar_selection(), solar_selection()$Year >= input$sun_startdate & solar_selection()$Year <= input$sun_enddate)
            start.year <- input$sun_startdate
            fit <- lm(Solar ~ I(Year - start.year), data = sun_sub)
            solar_trend <- round(100*fit$coefficients[2], 2)
            solar_trend <- paste(solar_trend, "Watts per meter squared", sep = " ")
            solar_trend
    })
    
    output$sun_confidence <- renderText({
            sun_sub <- subset(solar_selection(), solar_selection()$Year >= input$sun_startdate & solar_selection()$Year <= input$sun_enddate)
            start.year <- input$sun_startdate
            fit <- lm(Solar ~ I(Year - start.year), data = sun_sub)
            confidence <- confint(fit)
            c(round(100*confidence[2,1], 2), round(100*confidence[2,2], 2))
    })
    
    output$solar_change <- renderText({
            sun_sub <- subset(solar_selection(), solar_selection()$Year >= input$startdate & solar_selection()$Year <= input$enddate)
            start.year <- input$startdate
            solar_loess_fit <- loess(Solar ~ I(Year - start.year), data = sun_sub)
            solar_total <- tail(solar_loess_fit$fitted)[6] - head(solar_loess_fit$fitted)[1]
            solar_total <- round(solar_total, 1)
            solar_total <- paste(solar_total, "Watts per meter squared", sep = " ")
            solar_total
    })
    
    #CO2 plot
    output$CO2_plot <- renderPlotly({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            CO2_p <- ggplot(CO2_sub, aes(x = Year, y = mean.CO2)) +
                    theme_bw() +
                    geom_line(colour = "black", size = 1) +
                    geom_smooth(method = loess, formula = y ~ x, aes(colour = "LOESS"), se = FALSE, size = 0.5) +
                    geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend")) +
                    labs(x = "Year",
                         y = "Mean CO2 level (ppm)",
                         title = "Mean atmospheric CO2 levels per year")
            ggplotly(CO2_p)
    })
    
    #CO2 regression fit
    output$CO2_sum <- renderPrint({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$CO2_startdate
            fit <- lm(mean.CO2 ~ I(Year - start.year), data = CO2_sub)
            summary(fit)
    })
    
    #CO2 trend and confidence interval
    output$CO2_trend <- renderText({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$CO2_startdate
            fit <- lm(mean.CO2 ~ I(Year - start.year) + I((Year - start.year)^2), data = CO2_sub)
            CO2_trend <- 10*round(fit$coefficients[2], 2)
            CO2_trend <- paste(CO2_trend, "ppmv", sep = " ")
            CO2_trend
    })
    
    output$CO2_confidence <- renderText({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$CO2_startdate
            fit <- lm(mean.CO2~I(Year - start.year) + I((Year - start.year)^2), data = CO2_sub)
            confidence <- confint(fit)
            c(10*round(confidence[2,1], 2), 10*round(confidence[2,2], 2))
    })
    
    output$CO2_change <- renderText({
            CO2_sub <- subset(CO2, Year >= input$CO2_startdate & Year <= input$CO2_enddate)
            start.year <- input$startdate
            CO2_loess_fit <- loess(mean.CO2 ~ I(Year - start.year), data = CO2_sub)
            CO2_total <- tail(CO2_loess_fit$fitted)[6] - head(CO2_loess_fit$fitted)[1]
            CO2_total <- round(CO2_total, 1)
            CO2_total <- paste(CO2_total, "ppmv", sep = " ")
            CO2_total
    })
    
    #ENSO plot
    output$ENSO_plot <- renderPlotly({
            ENSO_sub <- subset(ENSO, YR >= input$ENSO_startdate & YR <= input$ENSO_enddate)
            ENSO_p <- ggplot(ENSO_sub, aes(x = Time, y = ANOM)) +
                    theme_bw() +
                    geom_line() +
                    geom_smooth(method = "loess", formula = y ~ x) +
                    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "red", size = 0.25) +
                    geom_hline(yintercept = -0.5, linetype = "dashed", colour = "blue", size = 0.25)
                    labs(x = "Year",
                         y = "Sea surface temperature anomaly (ºC)",
                         title = "ENSO 3.4 sea surface temperature")
            ggplotly(ENSO_p)
    })
    
    #ENSO regression fit
    output$ENSO_sum <- renderPrint({
            ENSO_sub <- subset(ENSO, YR >= input$ENSO_startdate & YR <= input$ENSO_enddate)
            fit <- lm(ANOM ~ Time, data = ENSO_sub)
            summary(fit)
    })
    
    #ENSO trend and confidence interval
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
    
    # Sea Ice extent
    sea_ice_data_set <- reactive({
            if (input$sea_ice_data == "Arctic")
                    Arctic_sea_ice
            else
                    Antarctic_sea_ice
    })
    
    sea_ice_month <- reactive({
            if (input$sea_ice_month_choice == "January")
                    1
            else if (input$sea_ice_month_choice == "February")
                    2
            else if (input$sea_ice_month_choice == "March")
                    3
            else if (input$sea_ice_month_choice == "April")
                    4
            else if (input$sea_ice_month_choice == "May")
                    5
            else if (input$sea_ice_month_choice == "June")
                    6
            else if (input$sea_ice_month_choice == "July")
                    7
            else if (input$sea_ice_month_choice == "August")
                    8
            else if (input$sea_ice_month_choice == "September")
                    9
            else if (input$sea_ice_month_choice == "October")
                    10
            else if (input$sea_ice_month_choice == "November")
                    11
            else if (input$sea_ice_month_choice == "December")
                    12
            else 
                    1:12
    })
    
    output$sea_ice_plot <- renderPlotly({
            sea_ice_suba <- subset(sea_ice_data_set(), year >= input$sea_ice_startdate & year <= input$sea_ice_enddate)
            sea_ice_sub <- subset(sea_ice_suba, month %in% sea_ice_month())
            
            sea_ice_p <- ggplot(sea_ice_sub, aes(x = decimal.date, y = extent)) +
                    theme_bw() +
                    geom_line() +
                    geom_smooth(method = "loess", formula = y ~ x, aes(colour = "LOESS"), se = FALSE, size = 0.5) +
                    geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear Trend")) +
                    labs(x = "Year",
                         y = "Sea ice extent (millions sq km)",
                         title = "Sea ice extent")
            
            ggplotly(sea_ice_p)
    })
    
    output$sea_ice_sum <- renderPrint({
            sea_ice_suba <- subset(sea_ice_data_set(), year >= input$sea_ice_startdate & year <= input$sea_ice_enddate)
            sea_ice_sub <- subset(sea_ice_suba, month %in% sea_ice_month())
            fit <- lm(extent ~ decimal.date, data = sea_ice_sub)
            summary(fit)
    })
    
    #ENSO trend and confidence interval
    output$sea_ice_trend <- renderText({
            sea_ice_suba <- subset(sea_ice_data_set(), year >= input$sea_ice_startdate & year <= input$sea_ice_enddate)
            sea_ice_sub <- subset(sea_ice_suba, month %in% sea_ice_month())
            sea_fit <- lm(extent ~ decimal.date, data = sea_ice_sub)
            sea_trend <- round(10*sea_fit$coefficients[2], 2)
            sea_trend <- paste(sea_trend, "square kilometers", sep = " ")
            sea_trend
    })
    
    output$sea_ice_confidence <- renderText({
            sea_ice_suba <- subset(sea_ice_data_set(), year >= input$sea_ice_startdate & year <= input$sea_ice_enddate)
            sea_ice_sub <- subset(sea_ice_suba, month %in% sea_ice_month())
            fit <- lm(extent ~ decimal.date, data = sea_ice_sub)
            confidence <- confint(fit)
            c(round(10*confidence[2,1], 2), 10*round(confidence[2,2], 2))
    })
    
    output$sea_ice_change <- renderText({
            sea_ice_suba <- subset(sea_ice_data_set(), year >= input$sea_ice_startdate & year <= input$sea_ice_enddate)
            sea_ice_sub <- subset(sea_ice_suba, month %in% sea_ice_month())
            start.year <- input$startdate
            sea_loess_fit <- loess(extent ~ decimal.date, data = sea_ice_sub)
            sea_ice_total <- tail(sea_loess_fit$fitted)[6] - head(sea_loess_fit$fitted)[1]
            sea_ice_total <- round(sea_ice_total, 1)
            sea_ice_total <- paste(sea_ice_total, "square kilometers", sep = " ")
            sea_ice_total
    })
    
})
