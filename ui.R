# ui.R
library(shiny)
library(plotly)
library(ggplot2)
library(bslib)

shinyUI(
        fluidPage(
                theme = bs_theme(version = 5),
                titlePanel("Climate Indicators"),
                
                # Global controls (theme + palette) always visible above tabs
                fluidRow(
                        column(
                                width = 4,
                                selectInput(
                                        "theme_choice", "Theme",
                                        choices = c("Light" = "flatly", "Dark" = "darkly"),
                                        selected = "flatly"
                                )
                        ),
                        column(
                                width = 4,
                                selectInput(
                                        "palette_choice", "Palette",
                                        choices = c("Default", "Viridis (color-blind friendly)", "Okabe-Ito (color-blind friendly)"),
                                        selected = "Viridis (color-blind friendly)"
                                )
                        )
                ),
                
                tabsetPanel(
                        # --------------------------- Global surface temperature ---------------------------
                        tabPanel(
                                "Global surface temperature",
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                selectInput(
                                                        "temperature", "Choose a data set:",
                                                        list("GISS" = "GISS", "HadCRUT" = "HadCRUT", "BEST" = "BEST"),
                                                        selected = "HadCRUT"
                                                ),
                                                h5("Enter a year between 1850 and last year"),
                                                h6("Note: HadCRUT and BEST start in 1850, GISS in 1880"),
                                                numericInput("startdate", "Start Year", 1880, min = 1850),
                                                numericInput("enddate", "End Year", 2024, min = 1851)
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3("Linear trend per 100 years"),
                                                textOutput("trend"),
                                                h5("95% confidence interval (ºC)"),
                                                textOutput("confidence"),
                                                h3("Net change over the selected time period"),
                                                textOutput("total_change"),
                                                h3("Change in global temperature over time"),
                                                h5("Trend with 95% confidence interval"),
                                                plotlyOutput("tempPlot", height = "500px"),
                                                br(),
                                                downloadButton("dl_temp", "Download filtered data (CSV)"),
                                                h3("Regression fit"),
                                                verbatimTextOutput("sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- Global satellite temperature ---------------------------
                        tabPanel(
                                "Global satellite temperature",
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                selectInput("satellite", "Choose a data set:", list("RSS" = "RSS", "UAH" = "UAH"), selected = "RSS"),
                                                h5("Enter a year between 1979 and last year"),
                                                numericInput("sat_startdate", "Start Year", 1979, min = 1979),
                                                numericInput("sat_enddate", "End Year", 2024, min = 1980)
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3("Linear trend per 100 years"),
                                                textOutput("sat_trend"),
                                                h5("95% confidence interval (ºC)"),
                                                textOutput("sat_confidence"),
                                                h3("Net change over the selected time period"),
                                                textOutput("sat_total_change"),
                                                h3("Change in global temperature over time"),
                                                h5("Trend with 95% confidence interval"),
                                                plotlyOutput("sat_tempPlot", height = "500px"),
                                                br(),
                                                downloadButton("dl_sat", "Download filtered data (CSV)"),
                                                h3("Regression fit"),
                                                verbatimTextOutput("sat_sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- Ocean surface temperature ---------------------------
                        tabPanel(
                                "Ocean surface temperature",
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                h5("Enter a year between 1850 and last year"),
                                                numericInput("ocean_startdate", "Start Year", 1850, min = 1850),
                                                numericInput("ocean_enddate", "End Year", 2024, min = 1851)
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3("Trend per 100 years"),
                                                textOutput("ocean_trend"),
                                                h5("95% confidence interval (ºC)"),
                                                textOutput("ocean_confidence"),
                                                h3("Net change over the selected time period"),
                                                textOutput("ocean_total_change"),
                                                h3("Change in ocean temperature over time"),
                                                h5("Trend with 95% confidence interval"),
                                                plotlyOutput("ocean_tempPlot", height = "500px"),
                                                br(),
                                                downloadButton("dl_ocean", "Download filtered data (CSV)"),
                                                h3("Regression fit"),
                                                verbatimTextOutput("ocean_sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- Solar Activity ---------------------------
                        tabPanel(
                                "Solar Activity",
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                selectInput(
                                                        "solar_data", "Choose a data set:",
                                                        list("Sunspots" = "sunspots", "Solar irradiance" = "irradiance"),
                                                        selected = "irradiance"
                                                ),
                                                h5("Enter a year between 1610 and last year"),
                                                numericInput("sun_startdate", "Start Year", 1850, min = 1610),
                                                numericInput("sun_enddate", "End Year", 2025, min = 1751)
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3("Linear trend in solar output per 100 years"),
                                                textOutput("sun_trend"),
                                                h5("95% confidence interval (Watts per meter squared)"),
                                                textOutput("sun_confidence"),
                                                h3("Net change over the selected time period"),
                                                textOutput("solar_change"),
                                                h3("Change in solar output over time"),
                                                h5("Trend with 95% confidence interval"),
                                                plotlyOutput("sun_plot", height = "500px"),
                                                br(),
                                                downloadButton("dl_solar", "Download filtered data (CSV)"),
                                                h3("Regression fit"),
                                                verbatimTextOutput("sun_sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- CO2 ---------------------------
                        tabPanel(
                                HTML(paste0("CO", tags$sub("2"))),
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                h5("Enter a year between 1959 and last year"),
                                                numericInput("CO2_startdate", "Start Year", 1959, min = 1959),
                                                numericInput("CO2_enddate", "End Year", 2025, min = 1960)
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3(HTML(paste0("Linear change in atmospheric CO", tags$sub("2"), " levels per decade"))),
                                                textOutput("CO2_trend"),
                                                h5("95% confidence interval (ppmv)"),
                                                textOutput("CO2_confidence"),
                                                h3("Net change over selected time period"),
                                                textOutput("CO2_change"),
                                                h3(HTML(paste0("Change in atmospheric CO", tags$sub("2"), " levels over time"))),
                                                h5("Trend with 95% confidence interval"),
                                                plotlyOutput("CO2_plot", height = "500px"),
                                                br(),
                                                downloadButton("dl_CO2", "Download filtered data (CSV)"),
                                                h3("Regression fit"),
                                                verbatimTextOutput("CO2_sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- ENSO ---------------------------
                        tabPanel(
                                "El Niño/Southern Oscillation",
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                h5("Enter a year between 1950 and last year"),
                                                numericInput("ENSO_startdate", "Start Year", 1950, min = 1950),
                                                numericInput("ENSO_enddate", "End Year", 2025, min = 1951)
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3("Change in ENSO 3.4 area temperature anomalies per decade (ºC)"),
                                                textOutput("ENSO_trend"),
                                                h5("95% confidence interval (ºC)"),
                                                textOutput("ENSO_confidence"),
                                                h3("Change in ENSO temperature anomalies over time"),
                                                h5("Loess trend with 95% confidence interval"),
                                                plotlyOutput("ENSO_plot", height = "500px"),
                                                br(),
                                                downloadButton("dl_ENSO", "Download filtered data (CSV)"),
                                                h3("Regression fit"),
                                                verbatimTextOutput("ENSO_sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- Sea Ice ---------------------------
                        tabPanel(
                                "Sea Ice Area",
                                sidebarLayout(
                                        sidebarPanel(
                                                width = 3,
                                                h5("Pick a data set"),
                                                selectInput("sea_ice_data", "Select data", choices = c("Arctic", "Antarctic")),
                                                h5("Enter a year between 1979 and last year"),
                                                numericInput("sea_ice_startdate", "Start Year", 1979, min = 1979),
                                                numericInput("sea_ice_enddate", "End Year", 2025, min = 1980),
                                                h5("Select a month"),
                                                selectInput("sea_ice_month_choice", "Select month", choices = c("All", month.name))
                                        ),
                                        mainPanel(
                                                width = 9,
                                                h3("Change in sea ice area per decade"),
                                                textOutput("sea_ice_trend"),
                                                h5("95% confidence interval (million square kilometers)"),
                                                textOutput("sea_ice_confidence"),
                                                h3("Net change over selected time period"),
                                                textOutput("sea_ice_change"),
                                                h3("Change in sea ice area over time"),
                                                h5("Linear trend with 95% confidence interval"),
                                                plotlyOutput("sea_ice_plot", height = "500px"),
                                                br(),
                                                downloadButton("dl_seaice", "Download filtered data (CSV)"),
                                                h3("Linear Regression fit"),
                                                verbatimTextOutput("sea_ice_sum")
                                        )
                                )
                        ),
                        
                        # --------------------------- FAQ ---------------------------
                        tabPanel(
                                "FAQ",
                                sidebarLayout(
                                        sidebarPanel = NULL,
                                        mainPanel(
                                                h4("Question: Where are you getting the data?"),
                                                "GISS:",
                                                a(href = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt", "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt"),
                                                br(), br(),
                                                "HadCRUT5:",
                                                a(href = "https://www.metoffice.gov.uk/hadobs/crutem5/data/CRUTEM.5.0.2.0/diagnostics/CRUTEM.5.0.2.0.summary_series.global.annual.csv", "https://www.metoffice.gov.uk/hadobs/crutem5/data/CRUTEM.5.0.2.0/diagnostics/CRUTEM.5.0.2.0.summary_series.global.annual.csv"),
                                                br(), br(),
                                                "BEST:",
                                                a(href = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt", "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt"),
                                                br(), br(),
                                                "RSS TLT:",
                                                a(href = "https://images.remss.com/msu/graphics/TLT_v40/time_series/RSS_TS_channel_TLT_Global_Land_And_Sea_v04_0.txt", "https://images.remss.com/msu/graphics/TLT_v40/time_series/RSS_TS_channel_TLT_Global_Land_And_Sea_v04_0.txt"),
                                                br(), br(),
                                                "UAH:",
                                                a(href = "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"),
                                                br(), br(),
                                                "Ocean temperatures (HadSST):",
                                                a(href = "https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/HadSST.4.1.1.0_annual_GLOBE.csv", "https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/HadSST.4.1.1.0_annual_GLOBE.csv"),
                                                br(), br(),
                                                "Sunspots:",
                                                a(href = "https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt", "https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt"),
                                                br(), br(),
                                                "Solar irradiance:",
                                                a(href = "https://www2.mps.mpg.de/projects/sun-climate/data/SATIRE/SATIRE-S/SATIRE-S_TSI_latest.txt", "https://www2.mps.mpg.de/projects/sun-climate/data/SATIRE/SATIRE-S/SATIRE-S_TSI_latest.txt"),
                                                br(), br(),
                                                "CO2:",
                                                a(href = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt", "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt"),
                                                br(), br(),
                                                "El Niño/Southern Oscillation:",
                                                a(href = "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt", "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"),
                                                br(), br(),
                                                "Arctic Sea Ice:",
                                                a(href = "https://psl.noaa.gov/data/timeseries/month/data/n_icearea.mon.csv", "https://psl.noaa.gov/data/timeseries/month/data/n_icearea.mon.csv"),
                                                br(), br(),
                                                "Antarctic Sea Ice:",
                                                a(href = "https://psl.noaa.gov/data/timeseries/month/data/s_icearea.mon.csv", "https://psl.noaa.gov/data/timeseries/month/data/s_icearea.mon.csv"),
                                                br(),
                                                h4("Question: I've heard that the temperature data are fudged, that there's really no warming."),
                                                p("Answer: This one has been answered so many times, we're flogging a dead horse at this point. Yes, scientists make adjustments to the data. No, they're not fudging the data. Those adjustments are to make the data more accurate."),
                                                br(),
                                                p("Think of it like this: I buy a new computerized thermostat for my house that shows consistently 2º higher than my old thermostat. If I want to compare the temperature data for my house between the old thermostat and the new, what should I do? Should I compare the raw data without adjusting for the difference between the two thermostats? Or should I add 2º to the old data (or substract 2º from the new) first?"),
                                                br(),
                                                p("I'm sure you'll agree that I should first adjust my data to get an accurate comparison. That's the same thing NOAA, NASA, and the Hadley Center do on a much larger scale. If you want to know precisely what they do and why, Carbon Brief has a nice rundown on the adjustments those agencies make and why:"),
                                                a(href = "https://www.carbonbrief.org/explainer-how-data-adjustments-affect-global-temperature-records/", "https://www.carbonbrief.org/explainer-how-data-adjustments-affect-global-temperature-records/"),
                                                br(), br(),
                                                HTML("But no, those adjustments don't create global warming from nothing. In fact, the adjustments actually make the temperature data show <i>less</i> global warming."),
                                                br(),
                                                h4("Question: Isn't this all part of a natural cycle?"),
                                                p("Answer: No. None of the natural cycles we currently know could explain the current warming trend. First, it's happening much faster than any natural cycle could cause. Second, all of the cycles we have discovered so far are either neutral or cooling."),
                                                p("The sun? Cooling since the 1950s, as you can verify yourselves with the Solar Activity tab."),
                                                p("Milankovic cycles? Warming due to Milankovic cycles peaked 6,000 years ago and has been cooling since. If it were up to Milankovic cycles, we'd still be cooling down to the next ice age. Also take centuries to change the average global temperature by 1ºC."),
                                                p("El Niño? No discernable trend and also has the wrong period, again as you can verify yourself with the ENSO tab. While El Niño explains some of the wiggles around the trend, it does not match the trend itself."),
                                                p("Pacific Decadal Oscillation? Again, doesn't match the trend. Beyond not matching the trend, if ocean cycles were causing global warming, we'd expect ocean temperatures to cool as the atmosphere warmed. That hasn't happened."),
                                                p("Cosmic rays? Cosmic rays create C-14 in the atmosphere. If it was cosmic rays, we'd see more C-14 being created. That hasn't happened."),
                                                HTML("The planet been both warmer and cooler than today. However, just because what happened in the past was natural does not mean that what is happening today is also natural. Change happens because something is <i>causing</i> that change. If you truly believe that global warming is due to a natural cycle, the burden of proof is on <b>you</b> to identify that cycle and show that it is causing the current warming trend. So, which cycle is it and where's your evidence?")
                                        )
                                )
                        )
                ),
                
                hr(),
                h5("Created by: Jim Milks"),
                "Version 5",
                br(),
                "Updated: 2025 Sept 9",
                br(),
                a(actionButton(inputId = "email1", label = "Contact Admin", icon = icon("envelope", lib = "font-awesome")), href = "mailto: jrmilks@gmail.com"),
                br(),
                "Code available at:",
                a(href = "https://github.com/jrmilks74/global-temperature-trend-calculator/tree/main",
                  "https://github.com/jrmilks74/global-temperature-trend-calculator/tree/main")
        )
)
