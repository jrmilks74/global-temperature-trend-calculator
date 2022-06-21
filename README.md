# global-temperature-trend-calculator
This is a Shiny app that automatically calculates linear trends in global temperature data sets using annual means to avoid autocorrelation. The threee data sets included are GISS, NOAA, and HadCRUT5.0.1.

Originally, I used only the Cowtan-Way data set as it was the easiest to import into R and use with minimal processing. Unfortunately, Cowtan and Way are no longer maintaining that data set, necessitating the switch to the more demanding GISS, NOAA, and HadCRUT data sets. I also added a FAQ page in this version of the app.

The app will automatically calculate the linear trend between any beginning and endpoints you specifiy, along with the trend per 100 years and the 95% confidence interval of that trend. Just remember that GISS and NOAA begin in 1880 whereas HadCRUT starts in 1850. If you try to calculate a trend starting 1879 using either GISS or NOAA, you'll get an error message. Below the graph of the trend you asked for, you'll find the raw R output if you desire to get the P-value or similar output.
