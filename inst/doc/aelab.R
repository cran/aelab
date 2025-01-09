## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
rmarkdown.html_vignette.check_title = FALSE
library(aelab)
library(readxl)
library(tibble)
library(lubridate)
library(stats)
library(dplyr)
library(openxlsx)

## -----------------------------------------------------------------------------
# The provided file is a raw data file downloaded from 
# the LI-COR Trace Gas Analyzer
ghg_data_path <- system.file("extdata", "ch4.xlsx", package = "aelab", mustWork = T)
ch4 <- tidy_ghg_analyzer(ghg_data_path, "ch4")
ch4[c(1:3), ]

## -----------------------------------------------------------------------------
# The analyzer's time was assumed to be 
# 15 minutes and 30 seconds faster than real time
ch4 <- convert_time(ch4, min = -15, sec = 30)
ch4[c(1:3), c(5:6)]

## -----------------------------------------------------------------------------
ref_data_path <- system.file("extdata", "reference.xlsx", package = "aelab", mustWork = T)
ref <- read_excel(ref_data_path)
ref

## -----------------------------------------------------------------------------
calculate_regression(ch4, ghg = "CH4", reference_time = ref$date_time,
                     duration_minutes = 7, num_rows = 300)

## -----------------------------------------------------------------------------
calculate_regression(ch4, ghg = "CH4", reference_time = as.POSIXct("2023-03-11 07:32:00", tz = "UTC"))

## -----------------------------------------------------------------------------
results_ch4 <- calculate_regression(ch4, ghg = "CH4", reference_time = as.POSIXct("2023-03-11 07:32:00", tz = "UTC"))
flux_ch4 <- data.frame(
    slope = results_ch4$slope,
    area = 1, # in square meter
    volume = 1, # in litre
    temp = 1) # in celcius
calculate_ghg_flux(flux_ch4)

## -----------------------------------------------------------------------------
hobo_data_path <- system.file("extdata", "ex_hobo.csv", package = "aelab")
do <- process_hobo(hobo_data_path, no_hobo = "code_for_logger")
do[c(1:3), ]

## -----------------------------------------------------------------------------
weather_data_path <- system.file("extdata", "ex_weather.csv", package = "aelab")
weather <- process_weather(weather_data_path, date = "2024-04-10", zone = "zone_A")
weather[c(1:5), ]

## -----------------------------------------------------------------------------
info_data_path <- system.file("extdata", "info.xlsx", package = "aelab")
info <- process_info(info_data_path)
info

## -----------------------------------------------------------------------------
data <- merge(weather, do, by = "date_time")
merged_df <- data %>% 
  inner_join(info, by = c("zone", "no_hobo"))
merged_df[c(1:3), ]
plot_hobo(merged_df)

## -----------------------------------------------------------------------------
calculate_do(merged_df)

