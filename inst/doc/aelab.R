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
# Numeric input: 97 mg CH4 m-2 h-1 → µg m-2 h-1
convert_ghg_unit(97, ghg = "ch4", mass = "mg", area = "m2", time = "hr")

# Character string input: each number is converted individually
convert_ghg_unit("0.002 ± 0.003", ghg = "ch4", mass = "mmol", area = "m2", time = "hr")

## -----------------------------------------------------------------------------
calculate_MDF(
  precision_ppm     = 1,
  closure_time_s    = 300,
  data_point_n      = 300,
  chamber_volume_m3 = 0.0064,
  temperature_C     = 25,
  chamber_area_m2   = 0.07
)

## -----------------------------------------------------------------------------
calculate_total_co2e(co2 = 4.02, ch4 = 0.001, n2o = 0.003)

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

## -----------------------------------------------------------------------------
set.seed(42)
stat_df <- data.frame(
  group = rep(c("A", "B", "C"), each = 8),
  value = c(rnorm(8, 2, 0.4), rnorm(8, 3.5, 0.5), rnorm(8, 5, 0.6))
)

## -----------------------------------------------------------------------------
descriptive_statistic(stat_df, vars = value, groups = group)

## -----------------------------------------------------------------------------
find_outlier(stat_df, var = "value")

## -----------------------------------------------------------------------------
# Two-group context
normality_test_t(stat_df, "value", group, "A", "B")

# One-way ANOVA context (all three groups)
normality_test_aov(stat_df, "value", "group")

## -----------------------------------------------------------------------------
stat_df_t <- df_trans(stat_df, "value", "sqrt")
head(stat_df_t)

## -----------------------------------------------------------------------------
aov_test(stat_df, "value", "group")

## -----------------------------------------------------------------------------
ks_test(stat_df, "value", "group")

## ----eval=FALSE---------------------------------------------------------------
#  # Preview the "ghg" palette
#  aelab_palettes("ghg")
#  
#  # Box plot with aelab palette
#  df_vis <- data.frame(
#    site = rep(c("Mangrove", "Mudflat", "Seagrass"), each = 10),
#    ch4  = c(rnorm(10, 2, 0.5), rnorm(10, 1, 0.3), rnorm(10, 0.5, 0.2))
#  )
#  
#  plot_box(df_vis, site, ch4, site) +
#    scale_fill_aelab_d("ghg")

