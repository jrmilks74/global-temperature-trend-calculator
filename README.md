# global-temperature-trend-calculator
This is a Shiny app that automatically calculates linear trends in global temperatures and other data sets using annual means to avoid autocorrelation. The data sets include GISS, NOAA, and HadCRUT5.0.1, UAH, RSS, HadSST, sunspots, atmospheric carbon dioxide, and ENSO.

Originally, I used only the Cowtan-Way data set as it was the easiest to import into R and use with minimal processing. Unfortunately, Cowtan and Way are no longer maintaining that data set, necessitating the switch to the more pre-processing intensive GISS, NOAA, and HadCRUT data sets. I added a FAQ page in this version of the app and later, satellite data, ocean temperature data, atmospheric carbon dioxide, and others.

The app will automatically calculate the linear trend between any beginning and endpoints you specifiy, along with the trend per 100 years and the 95% confidence interval of that trend. Below the graph of the trend you asked for, you'll find the raw R output if you desire to get the P-value or similar output.

You can find the trend calculator online at https://jrmilks.shinyapps.io/global_temperature_trend/
