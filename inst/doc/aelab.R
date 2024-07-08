## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
rmarkdown.html_vignette.check_title = FALSE

## -----------------------------------------------------------------------------
library(aelab)

## -----------------------------------------------------------------------------
library(readxl)
library(tibble)
library(lubridate)
library(stats)
library(dplyr)
library(openxlsx)

## -----------------------------------------------------------------------------
ghg_data_path <- system.file("extdata", "ch4.xlsx", package = "aelab", mustWork = T)
ch4 <- tidy_licor(ghg_data_path, "ch4")
ch4[c(1:5), ]

## -----------------------------------------------------------------------------
ch4 <- convert_time(ch4, min = -15, sec = 30)
ch4[c(1:5), ]

## -----------------------------------------------------------------------------
ref_data_path <- system.file("extdata", "reference.xlsx", package = "aelab", mustWork = T)
ref <- read_excel(ref_data_path)
ref

## -----------------------------------------------------------------------------
calculate_regression(ch4, ghg = "CH4", reference_time = ref$date_time)

## -----------------------------------------------------------------------------
calculate_regression(ch4, ghg = "CH4", reference_time = ref$date_time,
                     duration_minutes = 5, num_rows = 300)

## -----------------------------------------------------------------------------
calculate_regression(ch4, ghg = "CH4", reference_time = as.POSIXct("2023-03-11 07:32:00", tz = "UTC"))

