# convert fit to csv at: http://garmin.stevegordon.co.uk/

library(tidyverse)

workouts <- lapply(list.files("csv_files"), function(f) {
  read_csv(paste0("csv_files/", f),
           col_names = c("lat", "lon", "duration", "distance", "mph", "hr",
                         "cadence", "power", "elev")) %>%
    filter(!is.na(power)) %>%
    mutate(duration = (duration - min(duration)) / 60)
})
names(workouts) <- gsub(".csv", "", list.files("csv_files"))

# Hmmm I should maybe store smoothed power and hr separately from raw...sprints are nice to see
smoothed_workouts <- lapply(workouts, function(df) {
  smooth_span <- 100 / nrow(df) # adjusted to be nice for Zwift...
  df$power <- loess(power ~ duration, df, span = smooth_span)$fitted
  df$hr <- loess(hr ~ duration, df, span = smooth_span)$fitted
  df
})

# Power + HR
smoothed_workouts[[2]] %>%
  ggplot(aes(duration)) +
  geom_line(aes(y = power), color = "green") +
  geom_line(aes(y = hr), color = "red") +
  xlab("Duration (min)") +
  ylab("Power (W), HR (bpm)")

# Power by geo
smoothed_workouts[[1]] %>%
  ggplot(aes(lon, lat, color = power)) +
  geom_point() +
  scale_color_distiller(palette = "Spectral")
