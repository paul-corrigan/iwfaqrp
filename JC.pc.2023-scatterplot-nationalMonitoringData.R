# Create a daily average time series plot and a map for 2023

# ----- Load 2023 monitor data -------------------------------------------------

library(AirMonitor)
library(dplyr)
library(ggplot2)
all_monitors <-

  # 1) load all monitors
  #    - negative values are lifted up to zero
  #    - use AirNow data rather than EPA AQS
  monitor_loadAnnual(
    year = 2023,
    QC_negativeValues = "zero",
    epaPreference = "airnow"
  ) %>%

  # 2) US only
  monitor_filter(countryCode == "US") %>%

  # 3) drop any monitors with no data
  monitor_dropEmpty()

# ----- Calculate daily means --------------------------------------------------

# NOTE:  Calculating daily means will depend upon the timezone so, from here on,
# NOTE:  we do things on a per-timezone basis.

dailyMeanList <- list()

for ( tz in unique(all_monitors$meta$timezone) ) {

  dailyMeanList[[tz]] <-

    all_monitors %>%
    monitor_filter(timezone == tz) %>%
    monitor_filterDate(20230415, 20231111) %>%
    monitor_dailyStatistic(
      FUN = mean,
      na.rm = TRUE,
      minHours = 18,
      dayBoundary = "LST"
    )

}

# ----- Timeseries plot --------------------------------------------------------

# Initialize the plot axes by plotting with 'transparent' color
monitor_timeseriesPlot(
  dailyMeanList[[1]],
  ylab = "24-hr Average PM2.5 (micrograms/m\u00b3)",
  xlab = "2023",
  ylim = c(0, 800),
  #main = '',
  addAQI = TRUE
)

# Then add each timezone in succession
for ( tz in unique(all_monitors$meta$timezone) ) {

  print(tz)
  monitor_timeseriesPlot(
    dailyMeanList[[tz]],
    opacity = 0.3,
    #main = '',
    add = TRUE,
    #col = aqiColors(dailyMeanList[[tz]])
  )

}

addAQILegend("topright", title = "Daily Average AQI")

title("2023 Timing of Smoke Impacts over entire US (all AQI levels), Apr 15-Nov 1")

