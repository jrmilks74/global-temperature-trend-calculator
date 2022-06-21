library(shiny)
library(plotly)
library(ggplot2)

# Create an interactive web page with a sidebar containing input fields and a main panel containing the results

shinyUI(fluidPage(

    # Application title
    titlePanel("Change in global temperature at Earth's surface"),
    tabsetPanel(
            
            # Sidebar with radio buttons for dataset selection and start and end years between 1880 and 2020
            tabPanel(
                    "Calculations",
                    sidebarLayout(
                            sidebarPanel(
                                    selectInput("temperature", "Choose a data set:",
                                                list("GISS" = "GISS",
                                                     "NOAA" = "NOAA",
                                                     "HadCRUT" = "HadCRUT"),
                                                selected = "HadCRUT"),
                                    
                                    h5("Enter a year between 1850 and last year"),
                                    h6("Note: HadCRUT starts in 1850, NOAA and GISS in 1880"),
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
                    "FAQ",
                    sidebarLayout(
                            sidebarPanel = NULL,
                            mainPanel(
                                    h4("Question: Why pick GISS, NOAA, and HadCRUT5?"),
                                    br(),
                                    "Answer: They are the main global temperature data sets measuring global tempertures within 2 meters (6 feet 6 inches) of Earth's surface, where humans actually live.",
                                    h4("Question: Why not include satellite data sets like UAH?"),
                                    br(),
                                    "Answer: Satellite data sets generally measure atmospheric temperatures 1 km (0.6 miles) above the Earth's surface. Last time I checked, humans don't live floating in midair.",
                                    h4("Question: Where are you getting the data?"),
                                    br(),
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
                                    h4("Question: I've heard that the numbers are fudged, that there's really no warming."),
                                   p("Answer: This one has been answered so many times, we're flogging a dead horse at this point. Yes, scientists make adjustments to the data. No, they're not fudging the data. Those adjustments are to make the data more accurate."), 
                                   br(),
                                   p("Think of it like this: I buy a new computerized thermostat for my house that shows consistently 2º higher than my old thermostat. If I want to compare the temperature data for my house between the old thermostat and the new, what should I do? Should I compare the raw data without adjusting for the difference between the two thermostats? Or should I add 2º to the old data (or substract 2º from the new) first?"),
                                   br(),
                                   p("I'm sure you'll agree that I should first adjust my data to get an accurate comparison. That's the same thing NOAA, NASA, and the Hadley Center do on a much larger scale. If you want to know precisely what they do and why, Carbon Brief has a nice rundown on the adjustments those agencies make and why:"),
                                    a(href = "https://www.carbonbrief.org/explainer-how-data-adjustments-affect-global-temperature-records/", "https://www.carbonbrief.org/explainer-how-data-adjustments-affect-global-temperature-records/"),
                                   br(),
                                   br(),
                                   HTML("But no, those adjustments don't create global warming from nothing. In fact, the adjustments actually make it so the temperature data shows <i>less</i> global warming.")
                                    )
                            )
                    )
            ),
    hr(),
    h5("Created by: Jim Milks"),
    "Version 2",
    br(),
    "Updated: 21 June 2022"
    )
)
