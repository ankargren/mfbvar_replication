library(tidyverse)
library(lubridate)
library(mfbvar)
directory <- "D:/mfbvar/small_grid"

data(mf_sweden)
gdp <- mf_sweden %>%
  .["gdp"] %>%
  na.omit() %>%
  rownames_to_column("date") %>%
  as_tibble() %>%
  mutate(date = lubridate::ymd(date))
files <- list.files(directory, full.names = TRUE)

## PIT
PIT_fun_best <- function(x) {
  temp <- readRDS(x)
  max_mdd <- which.max(vapply(temp, function(x) x$log_mdd, numeric(1)))
  temp2 <- temp[[max_mdd]]$fcst %>%
    mutate(prior = temp[[max_mdd]][["prior"]],
           time = temp[[max_mdd]][["time"]],
           freq = temp[[max_mdd]][["freq"]])
  left_join(temp2, gdp, by = "date") %>%
    group_by(date, fcst_date, prior, freq, time) %>%
    summarize(z = mean(fcst < gdp))
}
PIT <- map_df(files, PIT_fun_best)
saveRDS(PIT, file = paste0(dirname(directory), "/fcst_PIT_", basename(directory), ".rds"), compress = "xz")

## Interval coverage
interval_coverage <- function(x) {
  temp <- readRDS(x)
  max_mdd <- which.max(vapply(temp, function(x) x$log_mdd, numeric(1)))
  temp2 <- temp[[max_mdd]]$fcst %>%
    mutate(prior = temp[[max_mdd]][["prior"]],
           time = temp[[max_mdd]][["time"]],
           freq = temp[[max_mdd]][["freq"]]) %>%
    filter(date < ymd("2015-12-31")) %>%
    mutate(intraquarter = month(fcst_date)-3*(quarter(fcst_date)-1),
           h = (year(date) - year(fcst_date))*4 + quarter(date) - quarter(fcst_date)) %>%
    filter(h %in% c(0, 1, 2, 4, 8)) %>%
    group_by(date, fcst_date, prior, time, freq, h) %>%
    do(tibble(coverage = seq(0.1, 0.9, by = 0.1),
              lower = quantile(.$fcst, seq(0.45, 0.05, by = -0.05)),
              upper = quantile(.$fcst, seq(0.55, 0.95, by = 0.05)))) %>%
    ungroup()

  left_join(temp2, gdp, by = "date")%>%
    mutate(covered = ifelse((gdp > lower)*(gdp < upper), 1, 0))
}

coverage <- map_df(files, interval_coverage)
saveRDS(coverage, file = paste0(dirname(directory), "/fcst_coverage_", basename(directory), ".rds"), compress = "xz")
