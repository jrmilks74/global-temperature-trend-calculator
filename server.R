# server.R
library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(lubridate)
library(naniar)
library(bslib)
library(viridisLite)

# --------------------------- Helpers ---------------------------
validate_range <- function(df, x, start, end) {
        df |> dplyr::filter({{ x }} >= start, {{ x }} <= end)
}

trend_ci_text <- function(fit, scale_by = 1) {
        ci <- confint(fit)[2, ] * scale_by
        paste0(round(ci[1], 2), " to ", round(ci[2], 2))
}

loess_net_change <- function(x, y) {
        x <- suppressWarnings(as.numeric(x))
        y <- suppressWarnings(as.numeric(y))
        ok <- is.finite(x) & is.finite(y)
        x <- x[ok]; y <- y[ok]
        if (length(x) < 3 || length(unique(x)) < 3) return(NA_real_)
        o <- order(x); x <- x[o]; y <- y[o]
        fit <- tryCatch(stats::loess(y ~ x), error = function(e) NULL)
        if (is.null(fit)) return(NA_real_)
        preds <- tryCatch(predict(fit), error = function(e) rep(NA_real_, length(y)))
        if (!any(is.finite(preds))) return(NA_real_)
        round(tail(preds[is.finite(preds)], 1) - head(preds[is.finite(preds)], 1), 2)
}

# ---- Palette helpers (for legend-mapped layers) ----
okabe_ito <- function(n) {
        base <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        rep(base, length.out = n)
}

apply_palette <- function(p, palette_name, n = 3) {
        if (identical(palette_name, "Default")) return(p)
        vals <- if (palette_name == "Viridis (color-blind friendly)") viridis(n) else okabe_ito(n)
        p + scale_colour_manual(values = vals)
}

# Reusable builder for consistent time-series styling
make_ts_plot <- function(df, x, y, ylab, title, show_trend = TRUE, show_loess = TRUE) {
        g <- ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
                theme_bw() +
                geom_line(aes(colour = "Data")) +
                labs(x = "Year", y = ylab, title = title) +
                guides(colour = guide_legend(title = NULL))
        if (show_loess) {
                g <- g + geom_smooth(method = "loess", formula = y ~ x, aes(colour = "LOESS"), se = FALSE)
        }
        if (show_trend) {
                g <- g + geom_smooth(method = "lm", formula = y ~ x, aes(colour = "Linear trend"))
        }
        g
}

# --------------------------- Data (cached at startup) ---------------------------
# Global surface temperature
GISS <- read_table(
        "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt",
        skip = 7
) |>
        filter(!row_number() %in% c(22, 43, 64, 85, 106, 127, 148)) |>
        filter(row_number() <= n() - 5) |>
        select(Year, anomaly = `J-D`) |>
        mutate(anomaly = na_if(anomaly, "****")) |>
        mutate(Year = as.numeric(Year), anomaly = as.numeric(anomaly) / 100) |>
        tidyr::drop_na()

HadCRUT <- read_csv(
        "https://www.metoffice.gov.uk/hadobs/crutem5/data/CRUTEM.5.0.2.0/diagnostics/CRUTEM.5.0.2.0.summary_series.global.annual.csv"
) |>
        rename(Year = Time, anomaly = `Anomaly (deg C)`) |>
        select(Year, anomaly)

# --- BEST loader (robust) ---
options(timeout = 60)

read_best_global <- function() {
        urls <- c(
                "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt",
                "https://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt",
                "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt",
                "https://berkeley-earth-temperature.s3-website-us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt"
        )
        fetch_lines <- function(u) {
                ln <- tryCatch(readr::read_lines(u, progress = FALSE), error = function(e) character())
                if (length(ln)) return(ln)
                con <- try(url(u, open = "rb"), silent = TRUE)
                if (!inherits(con, "try-error")) {
                        on.exit(try(close(con), silent = TRUE), add = TRUE)
                        ln <- tryCatch(readLines(con, warn = FALSE), error = function(e) character())
                        if (length(ln)) return(ln)
                }
                if (requireNamespace("curl", quietly = TRUE)) {
                        h <- curl::new_handle(useragent = "R climate-indicators", followlocation = TRUE, timeout = 20)
                        res <- try(curl::curl_fetch_memory(u, handle = h), silent = TRUE)
                        if (!inherits(res, "try-error") && res$status_code >= 200 && res$status_code < 300) {
                                txt <- rawToChar(res$content); Encoding(txt) <- "UTF-8"
                                return(strsplit(txt, "\n", fixed = TRUE)[[1]])
                        }
                }
                character()
        }
        lines <- character()
        for (u in urls) { lines <- fetch_lines(u); if (length(lines)) break }
        if (!length(lines)) stop("BEST: could not download from any known URL")
        
        rx <- "^\\s*(\\d{4})\\s+(\\d{1,2})\\s+((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|NaN|nan)"
        mm <- regmatches(lines, regexec(rx, lines, perl = TRUE))
        mm <- mm[lengths(mm) > 0]
        if (!length(mm)) stop("BEST: file downloaded but no data-like lines parsed")
        
        y_chr <- vapply(mm, `[[`, "", 2)
        m_chr <- vapply(mm, `[[`, "", 3)
        a_chr <- vapply(mm, `[[`, "", 4)
        
        Year    <- suppressWarnings(as.integer(y_chr))
        Month   <- suppressWarnings(as.integer(m_chr))
        anomaly <- suppressWarnings(as.numeric(ifelse(tolower(a_chr) == "nan", NA, a_chr)))
        
        tibble::tibble(Year, Month, anomaly) |>
                dplyr::filter(is.finite(Year), Month >= 1, Month <= 12) |>
                dplyr::group_by(Year) |>
                dplyr::summarise(anomaly = mean(anomaly, na.rm = TRUE), .groups = "drop") |>
                dplyr::arrange(Year)
}
BEST <- read_best_global()

# Satellites
RSS <- read_table(
        "https://images.remss.com/msu/graphics/TLT_v40/time_series/RSS_TS_channel_TLT_Global_Land_And_Sea_v04_0.txt",
        col_names = FALSE, skip = 17
) |>
        rename(Year = X1, month = X2, anomaly = X3) |>
        select(Year, anomaly) |>
        mutate(anomaly = na_if(anomaly, -99.9000)) |>
        group_by(Year) |>
        summarize(anomaly = mean(anomaly, na.rm = TRUE), .groups = "drop")

UAH <- read.table(
        text = paste0(head(readLines("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"), -12), collapse = "\n"),
        header = TRUE
) |>
        select(Year, Globe) |>
        rename(anomaly = Globe) |>
        filter(!row_number() %in% 1) |>
        group_by(Year) |>
        summarize(anomaly = mean(anomaly, na.rm = TRUE), .groups = "drop")

# Sea surface temperatures
HadSST <- read_csv(
        "https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/HadSST.4.1.1.0_annual_GLOBE.csv",
        col_types = cols(year = col_number(), anomaly = col_number())
) |>
        rename(Year = year) |>
        select(Year, anomaly)

# Sunspots (annual)
sunspots <- read_table("https://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt", col_names = FALSE) |>
        rename(Year = X1, Solar = X2) |>
        select(Year, Solar) |>
        mutate(Year = Year - 0.5)

# --- Solar irradiance (SATIRE-T + SATIRE-S; long record back to 1610) ---
read_satire_t <- function(url = "https://www2.mps.mpg.de/projects/sun-climate/data/SATIRE-T_wu18_tsi.txt") {
        raw <- readr::read_lines(url, progress = FALSE)
        raw <- raw[!grepl("^\\s*;", raw)]
        raw <- raw[nzchar(trimws(raw))]
        txt  <- paste(raw, collapse = "\n")
        
        readr::read_table(
                file = txt,
                col_names = c("JD", "TSI"),
                col_types = readr::cols(
                        JD  = readr::col_double(),
                        TSI = readr::col_double()
                ),
                na = c("NaN", "nan", "NA"),
                show_col_types = FALSE
        ) |>
                dplyr::mutate(date = as.Date(JD - 2440587.5, origin = "1970-01-01")) |>
                dplyr::filter(is.finite(TSI), TSI > 1200, TSI < 1500)
}

read_satire_s <- function(
                url = "https://www2.mps.mpg.de/projects/sun-climate/data/SATIRE/SATIRE-S/SATIRE-S_TSI_latest.txt"
) {
        raw <- readr::read_lines(url, progress = FALSE)
        raw <- raw[!grepl("^\\s*;", raw)]
        raw <- raw[nzchar(trimws(raw))]
        txt  <- paste(raw, collapse = "\n")
        
        readr::read_table(
                file = txt,
                col_names = c("JD", "TSI", "lo", "hi", "source"),
                col_types = readr::cols(
                        JD     = readr::col_double(),
                        TSI    = readr::col_double(),
                        lo     = readr::col_double(),
                        hi     = readr::col_double(),
                        source = readr::col_double()
                ),
                na = c("NaN"),
                show_col_types = FALSE
        ) |>
                dplyr::mutate(date = as.Date(JD - 2440587.5, origin = "1970-01-01")) |>
                dplyr::filter(is.finite(JD), is.finite(TSI), TSI > 1200, TSI < 1500)
}

read_satire_combined <- function() {
        sat_t <- read_satire_t()
        sat_s <- read_satire_s()
        cutoff <- as.Date("1974-08-22")
        df <- dplyr::bind_rows(
                dplyr::filter(sat_t, date <= cutoff),
                dplyr::filter(sat_s, date >  cutoff)
        )
        df |>
                dplyr::mutate(Year = lubridate::year(date)) |>
                dplyr::group_by(Year) |>
                dplyr::summarise(Solar = mean(TSI, na.rm = TRUE), .groups = "drop") |>
                dplyr::arrange(Year)
}
irradiance <- read_satire_combined()

# CO2 annual
CO2 <- read_table(
        "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt",
        col_names = FALSE, skip = 45
) |>
        rename(Year = X1, mean.CO2 = X2, uncertainty = X3)

# ENSO (ONI)
ENSO <- read_table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
ENSO$Time <- seq.Date(from = as.Date("1950-01-01"), by = "month", length.out = nrow(ENSO))
ENSO$Status <- with(ENSO, ifelse(ANOM > 0.5, "El Niño", ifelse(ANOM < -0.5, "La Niña", "Neutral")))

# ---- PSL monthly sea-ice AREA (million km^2) ----
read_psl_ice_area <- function(url) {
        lines <- readr::read_lines(url, progress = FALSE)
        lines <- lines[nzchar(lines)]
        if (!length(lines)) stop("PSL sea-ice area: no content at ", url)
        body <- paste(lines[-1], collapse = "\n")  # drop human header
        df <- readr::read_csv(
                body,
                col_names = c("date", "area"),
                trim_ws = TRUE,
                show_col_types = FALSE,
                na = c("", "NA", "-9999", "-999.9", "-99.99"),
                col_types = readr::cols(
                        date = readr::col_date(format = ""),
                        area = readr::col_double()
                )
        )
        df |>
                dplyr::mutate(area = dplyr::if_else(is.na(area) | area < 0, NA_real_, area)) |>
                dplyr::filter(!is.na(date), !is.na(area)) |>
                dplyr::mutate(
                        year         = lubridate::year(date),
                        month        = lubridate::month(date),
                        month_name   = month.name[month],
                        year_month   = lubridate::floor_date(date, "month"),
                        decimal.date = year + (month - 1) / 12
                ) |>
                dplyr::select(year, month_name, area, month, year_month, decimal.date) |>
                dplyr::arrange(year_month)
}

Arctic_sea_ice    <- read_psl_ice_area("https://psl.noaa.gov/data/timeseries/month/data/n_icearea.mon.csv")
Antarctic_sea_ice <- read_psl_ice_area("https://psl.noaa.gov/data/timeseries/month/data/s_icearea.mon.csv")

# --------------------------- Server ---------------------------
shinyServer(function(input, output, session) {
        
        # Live theme switching (Flatly/Darkly via UI select)
        observeEvent(input$theme_choice, {
                session$setCurrentTheme(bs_theme(version = 5, bootswatch = input$theme_choice))
        }, ignoreInit = FALSE)
        
        # -------- Surface temps --------
        temperature_selection <- reactive({
                switch(input$temperature, GISS = GISS, BEST = BEST, HadCRUT = HadCRUT)
        })
        
        output$tempPlot <- renderPlotly({
                df <- validate_range(temperature_selection(), Year, input$startdate, input$enddate)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, Year, anomaly,
                                  ylab = "Temperature anomaly (ºC)",
                                  title = "Trend in global mean temperature")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$sum <- renderPrint({
                df <- validate_range(temperature_selection(), Year, input$startdate, input$enddate)
                summary(lm(anomaly ~ I(Year - input$startdate), data = df))
        })
        
        output$trend <- renderText({
                df <- validate_range(temperature_selection(), Year, input$startdate, input$enddate)
                fit <- lm(anomaly ~ I(Year - input$startdate), data = df)
                paste0(round(100 * coef(fit)[2], 2), " ºC")
        })
        
        output$confidence <- renderText({
                df <- validate_range(temperature_selection(), Year, input$startdate, input$enddate)
                fit <- lm(anomaly ~ I(Year - input$startdate), data = df)
                trend_ci_text(fit, 100)
        })
        
        output$total_change <- renderText({
                df <- validate_range(temperature_selection(), Year, input$startdate, input$enddate)
                if (nrow(df) < 3) return("N/A")
                delta <- loess_net_change(df$Year, df$anomaly)
                if (!is.finite(delta)) return("N/A")
                paste0(delta, " ºC")
        })
        
        # -------- Satellites --------
        sat_selection <- reactive(if (input$satellite == "RSS") RSS else UAH)
        
        output$sat_tempPlot <- renderPlotly({
                df <- validate_range(sat_selection(), Year, input$sat_startdate, input$sat_enddate)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, Year, anomaly,
                                  ylab = "Temperature anomaly (ºC)",
                                  title = "Trend in global mean temperature")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$sat_sum <- renderPrint({
                df <- validate_range(sat_selection(), Year, input$sat_startdate, input$sat_enddate)
                summary(lm(anomaly ~ I(Year - input$sat_startdate), data = df))
        })
        
        output$sat_trend <- renderText({
                df <- validate_range(sat_selection(), Year, input$sat_startdate, input$sat_enddate)
                fit <- lm(anomaly ~ I(Year - input$sat_startdate), data = df)
                paste0(round(100 * coef(fit)[2], 2), " ºC")
        })
        
        output$sat_confidence <- renderText({
                df <- validate_range(sat_selection(), Year, input$sat_startdate, input$sat_enddate)
                fit <- lm(anomaly ~ I(Year - input$sat_startdate), data = df)
                trend_ci_text(fit, 100)
        })
        
        output$sat_total_change <- renderText({
                df <- validate_range(sat_selection(), Year, input$sat_startdate, input$sat_enddate)
                paste0(loess_net_change(df$Year, df$anomaly), " ºC")
        })
        
        # -------- Ocean (HadSST) --------
        output$ocean_tempPlot <- renderPlotly({
                df <- validate_range(HadSST, Year, input$ocean_startdate, input$ocean_enddate)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, Year, anomaly,
                                  ylab = "Temperature anomaly (ºC)",
                                  title = "Trend in ocean mean temperature")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$ocean_sum <- renderPrint({
                df <- validate_range(HadSST, Year, input$ocean_startdate, input$ocean_enddate)
                summary(lm(anomaly ~ I(Year - input$ocean_startdate), data = df))
        })
        
        output$ocean_trend <- renderText({
                df <- validate_range(HadSST, Year, input$ocean_startdate, input$ocean_enddate)
                fit <- lm(anomaly ~ I(Year - input$ocean_startdate), data = df)
                paste0(round(100 * coef(fit)[2], 2), " ºC")
        })
        
        output$ocean_confidence <- renderText({
                df <- validate_range(HadSST, Year, input$ocean_startdate, input$ocean_enddate)
                fit <- lm(anomaly ~ I(Year - input$ocean_startdate), data = df)
                trend_ci_text(fit, 100)
        })
        
        output$ocean_total_change <- renderText({
                df <- validate_range(HadSST, Year, input$ocean_startdate, input$ocean_enddate)
                paste0(loess_net_change(df$Year, df$anomaly), " ºC")
        })
        
        # -------- Solar (long record) --------
        solar_selection <- reactive(if (input$solar_data == "sunspots") sunspots else irradiance)
        
        output$sun_plot <- renderPlotly({
                df <- validate_range(solar_selection(), Year, input$sun_startdate, input$sun_enddate)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, Year, Solar,
                                  ylab = "Total solar irradiance (W/m²)",
                                  title = "Mean solar output per year")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$sun_sum <- renderPrint({
                df <- validate_range(solar_selection(), Year, input$sun_startdate, input$sun_enddate)
                summary(lm(Solar ~ I(Year - input$sun_startdate), data = df))
        })
        
        output$sun_trend <- renderText({
                df <- validate_range(solar_selection(), Year, input$sun_startdate, input$sun_enddate)
                fit <- lm(Solar ~ I(Year - input$sun_startdate), data = df)
                paste0(round(100 * coef(fit)[2], 2), " W/m²")
        })
        
        output$sun_confidence <- renderText({
                df <- validate_range(solar_selection(), Year, input$sun_startdate, input$sun_enddate)
                fit <- lm(Solar ~ I(Year - input$sun_startdate), data = df)
                trend_ci_text(fit, 100)
        })
        
        output$solar_change <- renderText({
                df <- validate_range(solar_selection(), Year, input$sun_startdate, input$sun_enddate)
                paste0(loess_net_change(df$Year, df$Solar), " W/m²")
        })
        
        # -------- CO2 --------
        output$CO2_plot <- renderPlotly({
                df <- validate_range(CO2, Year, input$CO2_startdate, input$CO2_enddate)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, Year, mean.CO2,
                                  ylab = "Mean CO₂ level (ppm)",
                                  title = "Mean atmospheric CO₂ levels per year")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$CO2_sum <- renderPrint({
                df <- validate_range(CO2, Year, input$CO2_startdate, input$CO2_enddate)
                summary(lm(mean.CO2 ~ I(Year - input$CO2_startdate) + I((Year - input$CO2_startdate)^2), data = df))
        })
        
        output$CO2_trend <- renderText({
                df <- validate_range(CO2, Year, input$CO2_startdate, input$CO2_enddate)
                fit <- lm(mean.CO2 ~ I(Year - input$CO2_startdate) + I((Year - input$CO2_startdate)^2), data = df)
                paste0(round(10 * coef(fit)[2], 2), " ppmv")
        })
        
        output$CO2_confidence <- renderText({
                df <- validate_range(CO2, Year, input$CO2_startdate, input$CO2_enddate)
                fit <- lm(mean.CO2 ~ I(Year - input$CO2_startdate) + I((Year - input$CO2_startdate)^2), data = df)
                trend_ci_text(fit, 10)
        })
        
        output$CO2_change <- renderText({
                df <- validate_range(CO2, Year, input$CO2_startdate, input$CO2_enddate)
                paste0(loess_net_change(df$Year, df$mean.CO2), " ppmv")
        })
        
        # -------- ENSO --------
        output$ENSO_plot <- renderPlotly({
                df <- ENSO |> filter(YR >= input$ENSO_startdate, YR <= input$ENSO_enddate)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, Time, ANOM,
                                  ylab = "Sea surface temperature anomaly (ºC)",
                                  title = "ENSO 3.4 sea surface temperature",
                                  show_trend = TRUE, show_loess = TRUE) +
                        # Reference lines (no inherit.aes to avoid warnings in older ggplot2)
                        geom_hline(yintercept =  0.5, linetype = "dashed", linewidth = 0.25, colour = "red") +
                        geom_hline(yintercept = -0.5, linetype = "dashed", linewidth = 0.25, colour = "blue")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$ENSO_sum <- renderPrint({
                df <- ENSO |> filter(YR >= input$ENSO_startdate, YR <= input$ENSO_enddate)
                t <- decimal_date(df$Time)
                summary(lm(df$ANOM ~ t))
        })
        
        output$ENSO_trend <- renderText({
                df <- ENSO |> filter(YR >= input$ENSO_startdate, YR <= input$ENSO_enddate)
                t <- decimal_date(df$Time)
                fit <- lm(df$ANOM ~ t)
                paste0(round(10 * coef(fit)[2], 2))
        })
        
        output$ENSO_confidence <- renderText({
                df <- ENSO |> filter(YR >= input$ENSO_startdate, YR <= input$ENSO_enddate)
                t <- decimal_date(df$Time)
                fit <- lm(df$ANOM ~ t)
                trend_ci_text(fit, 10)
        })
        
        # -------- Sea Ice (area) --------
        sea_ice_data_set <- reactive(if (input$sea_ice_data == "Arctic") Arctic_sea_ice else Antarctic_sea_ice)
        sea_ice_month     <- reactive(if (input$sea_ice_month_choice == "All") 1:12 else match(input$sea_ice_month_choice, month.name))
        
        output$sea_ice_plot <- renderPlotly({
                df <- sea_ice_data_set() |>
                        filter(year >= input$sea_ice_startdate, year <= input$sea_ice_enddate, month %in% sea_ice_month()) |>
                        arrange(decimal.date)
                validate(need(nrow(df) > 1, "No data in selected range"))
                p <- make_ts_plot(df, decimal.date, area,
                                  ylab = "Sea ice area (million sq km)",
                                  title = "Sea ice area")
                p <- apply_palette(p, input$palette_choice, n = 3)
                ggplotly(p)
        })
        
        output$sea_ice_sum <- renderPrint({
                df <- sea_ice_data_set() |>
                        filter(year >= input$sea_ice_startdate, year <= input$sea_ice_enddate, month %in% sea_ice_month()) |>
                        arrange(decimal.date)
                summary(lm(area ~ decimal.date, data = df))
        })
        
        output$sea_ice_trend <- renderText({
                df <- sea_ice_data_set() |>
                        filter(year >= input$sea_ice_startdate, year <= input$sea_ice_enddate, month %in% sea_ice_month()) |>
                        arrange(decimal.date)
                fit <- lm(area ~ decimal.date, data = df)
                paste0(round(10 * coef(fit)[2], 2), " million square kilometers")
        })
        
        output$sea_ice_confidence <- renderText({
                df <- sea_ice_data_set() |>
                        filter(year >= input$sea_ice_startdate, year <= input$sea_ice_enddate, month %in% sea_ice_month()) |>
                        arrange(decimal.date)
                fit <- lm(area ~ decimal.date, data = df)
                trend_ci_text(fit, 10)
        })
        
        output$sea_ice_change <- renderText({
                df <- sea_ice_data_set() |>
                        filter(year >= input$sea_ice_startdate, year <= input$sea_ice_enddate, month %in% sea_ice_month()) |>
                        arrange(decimal.date)
                paste0(loess_net_change(df$decimal.date, df$area), " million square kilometers")
        })
})
