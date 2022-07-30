library(shiny)
library(plotly)
library(ggplot2)

# Create an interactive web page with a sidebar containing input fields and a main panel containing the results

shinyUI(fluidPage(

    # Application title
    titlePanel("Climate Indicators"),
    tabsetPanel(
            
            # Sidebar with radio buttons for dataset selection and start and end years between 1880 and 2020
            tabPanel(
                    "Global surface temperature",
                    sidebarLayout(
                            sidebarPanel(
                                    selectInput("temperature", "Choose a data set:",
                                                list("GISS" = "GISS",
                                                     "NOAA" = "NOAA",
                                                     "HadCRUT" = "HadCRUT",
                                                     "BEST" = "BEST"),
                                                selected = "HadCRUT"),
                                    
                                    h5("Enter a year between 1850 and last year"),
                                    h6("Note: HadCRUT and BEST start in 1850, NOAA and GISS in 1880"),
                                    numericInput("startdate", "Start Year", 1880, min = 1850),
                                    numericInput("enddate", "End Year", 2021, min = 1851)
                                    ),
                            
                            # Trend, time series graph with regression line, and regression output.
                            mainPanel(
                                    h3("Trend per 100 years (ºC)"),
                                    textOutput("trend"),
                                    h5("95% confidence interval (ºC)"),
                                    textOutput("confidence"),
                                    h3("Change in global temperature over time"),
                                    h5("Trend with 95% confidence interval"),
                                    plotlyOutput("tempPlot"),
                                    h3("Regression fit"),
                                    verbatimTextOutput("sum")
                                    )
                            )
                    ),
            
            tabPanel(
                    "Global satellite temperature",
                    sidebarLayout(
                            sidebarPanel(
                                    selectInput("satellite", "Choose a data set:",
                                                list("RSS" = "RSS",
                                                     "UAH" = "UAH"),
                                                selected = "RSS"),
                            
                            h5("Enter a year between 1979 and last year"),
                            numericInput("sat_startdate", "Start Year", 1979, min = 1979),
                            numericInput("sat_enddate", "End Year", 2021, min = 1980)
                            ),
                            # Trend, time series graph with regression line, and regression output.
                            mainPanel(
                                    h3("Trend per 100 years (ºC)"),
                                    textOutput("sat_trend"),
                                    h5("95% confidence interval (ºC)"),
                                    textOutput("sat_confidence"),
                                    h3("Change in global temperature over time"),
                                    h5("Trend with 95% confidence interval"),
                                    plotlyOutput("sat_tempPlot"),
                                    h3("Regression fit"),
                                    verbatimTextOutput("sat_sum")
                                    )
                            )
                    ),
            
            tabPanel(
                    "Ocean surface temperature",
                    sidebarLayout(
                            sidebarPanel(
                                    h5("Enter a year between 1850 and last year"),
                                    numericInput("ocean_startdate", "Start Year", 1850, min = 1850),
                                    numericInput("ocean_enddate", "End Year", 2021, min = 1851)
                            ),
                            # Trend, time series graph with regression line, and regression output.
                            mainPanel(
                                    h3("Trend per 100 years (ºC)"),
                                    textOutput("ocean_trend"),
                                    h5("95% confidence interval (ºC)"),
                                    textOutput("ocean_confidence"),
                                    h3("Change in ocean temperature over time"),
                                    h5("Trend with 95% confidence interval"),
                                    plotlyOutput("ocean_tempPlot"),
                                    h3("Regression fit"),
                                    verbatimTextOutput("ocean_sum")
                            )
                    )
            ),
            
            tabPanel(
                    "Solar Activity",
                    sidebarLayout(
                            sidebarPanel(
                                    h5("Enter a year between 1750 and last year"),
                                    numericInput("sun_startdate", "Start Year", 1750, min = 1750),
                                    numericInput("sun_enddate", "End Year", 2021, min = 1751)
                                    ),
                    #Trend, time series graph, and regression output
                    mainPanel(
                            h3("Change in sunspots numbers per 100 years"),
                            textOutput("sun_trend"),
                            h5("95% confidence interval"),
                            textOutput("sun_confidence"),
                            h3("Change in sunspot numbers over time"),
                            h5("Trend with 95% confidence interval"),
                            plotlyOutput("sun_plot"),
                            h3("Regression fit"),
                            verbatimTextOutput("sun_sum")
                            )
                    )
            ),
            
            tabPanel(
                    HTML(paste0("CO",tags$sub("2"))),
                    sidebarLayout(
                            sidebarPanel(
                                    h5("Enter a year between 1959 and last year"),
                                    numericInput("CO2_startdate", "Start Year", 1959, min = 1959),
                                    numericInput("CO2_enddate", "End Year", 2021, min = 1960)
                            ),
                            #Trend, time series graph, and regression output
                            mainPanel(
                                    h3(HTML(paste0("Linear change in atmospheric CO",tags$sub("2"), " levels per decade (ppm)"))),
                                    textOutput("CO2_trend"),
                                    h5("95% confidence interval (ppm)"),
                                    textOutput("CO2_confidence"),
                                    h3(HTML(paste0("Change in atmospheric CO",tags$sub("2"), " levels over time"))),
                                    h5("Trend with 95% confidence interval"),
                                    plotlyOutput("CO2_plot"),
                                    h3("Regression fit"),
                                    verbatimTextOutput("CO2_sum")
                            )
                    )
            ),
            
            tabPanel(
                    "El Niñ0/Southern Oscillation",
                    sidebarLayout(
                            sidebarPanel(
                                    h5("Enter a year between 1950 and last year"),
                                    numericInput("ENSO_startdate", "Start Year", 1950, min = 1950),
                                    numericInput("ENSO_enddate", "End Year", 2022, min = 1951)
                            ),
                            #Trend, time series graph, and regression output
                            mainPanel(
                                    h3("Change in ENSO 3.4 area temperature anomalies per decade (ºC)"),
                                    textOutput("ENSO_trend"),
                                    h5("95% confidence interval (ºC)"),
                                    textOutput("ENSO_confidence"),
                                    h3("Change in ENSO temperature anomalies over time"),
                                    h5("Loess trend with 95% confidence interval"),
                                    plotlyOutput("ENSO_plot"),
                                    h3("Regression fit"),
                                    verbatimTextOutput("ENSO_sum")
                            )
                    )
            ),
            
            tabPanel(
                    "FAQ",
                    sidebarLayout(
                            sidebarPanel = NULL,
                            mainPanel(
                                    h4("Question: Where are you getting the data?"),
                                    "GISS:",
                                    a(href = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt", "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt"),
                                    br(),
                                    br(),
                                    "NOAA:",
                                    a(href = "https://www.ncei.noaa.gov/access/monitoring/global-temperature-anomalies/anomalies", "https://www.ncei.noaa.gov/access/monitoring/global-temperature-anomalies/anomalies"),
                                    br(),
                                    br(),
                                    "HadCRUT5:",
                                    a(href = "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html", "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html"),
                                    br(),
                                    br(),
                                    "BEST:",
                                    a(href = "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_summary.txt", "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_summary.txt"),
                                    br(),
                                    br(),
                                    "RSS TLT:",
                                    a(href = "https://images.remss.com/msu/graphics/TLT_v40/time_series/RSS_TS_channel_TLT_Global_Land_And_Sea_v04_0.txt", "https://images.remss.com/msu/graphics/TLT_v40/time_series/RSS_TS_channel_TLT_Global_Land_And_Sea_v04_0.txt"),
                                    br(),
                                    br(),
                                    "UAH:",
                                    a(href = "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"),
                                    br(),
                                    br(),
                                    "Ocean temperatures (HadSST):",
                                    a(href = "https://www.metoffice.gov.uk/hadobs/hadsst4/data/csv/HadSST.4.0.1.0_annual_GLOBE.csv", "https://www.metoffice.gov.uk/hadobs/hadsst4/data/csv/HadSST.4.0.1.0_annual_GLOBE.csv"),
                                    br(),
                                    br(),
                                    "Sunspots:",
                                    a(href = "https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt", "https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt"),
                                    br(),
                                    br(),
                                    "CO2:",
                                    a(href = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt", "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt"),
                                    br(),
                                    br(),
                                    "El Niño/Southern Oscillation:",
                                    a(href = "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt", "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"),
                                    br(),
                                    h4("Question: I've heard that the temperature data are fudged, that there's really no warming."),
                                   p("Answer: This one has been answered so many times, we're flogging a dead horse at this point. Yes, scientists make adjustments to the data. No, they're not fudging the data. Those adjustments are to make the data more accurate."), 
                                   br(),
                                   p("Think of it like this: I buy a new computerized thermostat for my house that shows consistently 2º higher than my old thermostat. If I want to compare the temperature data for my house between the old thermostat and the new, what should I do? Should I compare the raw data without adjusting for the difference between the two thermostats? Or should I add 2º to the old data (or substract 2º from the new) first?"),
                                   br(),
                                   p("I'm sure you'll agree that I should first adjust my data to get an accurate comparison. That's the same thing NOAA, NASA, and the Hadley Center do on a much larger scale. If you want to know precisely what they do and why, Carbon Brief has a nice rundown on the adjustments those agencies make and why:"),
                                    a(href = "https://www.carbonbrief.org/explainer-how-data-adjustments-affect-global-temperature-records/", "https://www.carbonbrief.org/explainer-how-data-adjustments-affect-global-temperature-records/"),
                                   br(),
                                   br(),
                                   HTML("But no, those adjustments don't create global warming from nothing. In fact, the adjustments actually make the temperature data show <i>less</i> global warming."),
                                   br(),
                                   h4("Question: Isn't this all part of a natural cycle?"),
                                   p("Answer: No. None of the natural cycles we currently know could explain the current warming trend. First, it's happening much faster than any natural cycle could cause. Second, all of the cycles we have discovered so far are either neutral or cooling."),
                                   p("The sun? Cooling since the 1950s."),
                                   p("Milankovic cycles? Neutral or cooling over the past 6,000 years and also take centuries to change the average global temperature by 1ºC."),
                                   p("El Niño? No discernable trend and also has the wrong period. While El Niño explains some of the wiggles around the trend, it does not match the trend itself."),
                                   p("Pacific Decadal Oscillation? Again, doesn't match the trend. Beyond not matching the trend, if ocean cycles were causing global warming, we'd expect ocean temperatures to cool as the atmosphere warmed. That hasn't happened."),
                                   p("Cosmic rays? Cosmic rays create C-14 in the atmosphere. If it was cosmic rays, we'd see more C-14 being created. That hasn't happened."),
                                   HTML("The planet been both warmer and cooler than today. However, just because what happened in the past was natural does not mean that what is happening today is also natural. Change happens because something is <i>causing</i> that change. If you truly believe that global warming is due to a natural cycle, the burden of proof is on <b>you</b> to identify that cycle and show that it is causing the current warming trend. So, which cycle is it and where's your evidence?")
                                        )
                            )
                    )
            ),
    hr(),
    h5("Created by: Jim Milks"),
    "Version 3",
    br(),
    "Updated: 05 July 2022",
    br(),
    "Code available at:",
    a(href = "https://github.com/jrmilks74/global-temperature-trend-calculator/tree/main", "https://github.com/jrmilks74/global-temperature-trend-calculator/tree/main")
    )
)