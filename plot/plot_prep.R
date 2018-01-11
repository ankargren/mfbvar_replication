suffix <- "small_grid"
fcst_mean <- readRDS(sprintf("fcst_mean_%s.rds", suffix))
## Data prep
data(mf_sweden)
temp <- mf_sweden %>%
  .["gdp"] %>%
  na.omit() %>%
  rownames_to_column("date") %>%
  as_tibble() %>%
  mutate(date = lubridate::ymd(date))

fcst <- left_join(fcst_mean, temp, by = "date")

plot_dat <- fcst %>%
  mutate(intraquarter = month(fcst_date)-3*(quarter(fcst_date)-1), h = (year(date) - year(fcst_date))*4 + quarter(date) - quarter(fcst_date)) %>%
  filter(h > -1, h < 9) %>%
  mutate(error = mean - gdp, squared_error = error^2) %>%
  filter(!is.na(gdp)) %>%
  group_by(fcst_date, prior, freq) %>%
  mutate(rank = min_rank(log_mdd), rank = ifelse(rank == max(rank, na.rm = TRUE), TRUE, FALSE)) %>%
  ungroup()

plot_mod_ave <- plot_dat %>%
  group_by(date, fcst_date, prior, freq, h) %>%
  mutate(w = exp(log_mdd-max(log_mdd))/sum(exp(log_mdd-max(log_mdd))),
         error_w = w*error)%>%
  summarize(error_ma = sum(error_w)) %>%
  mutate(squared_error = error_ma^2) %>%
  group_by(h, prior, freq) %>%
  summarize(rmse = sqrt(mean(squared_error)))

plot1 <- plot_dat %>%
  group_by(lambda1, lambda2, h, prior, freq) %>%
  summarize(rmse = sqrt(mean(squared_error))) %>%
  unite(hyper, lambda1, lambda2)

plot2 <- plot_dat %>%
  filter(rank == TRUE) %>%
  group_by(h, prior, freq) %>%
  summarize(rmse = sqrt(mean(squared_error))) %>%
  mutate(prior_freq = paste0(freq, "-", prior)) %>%
  mutate(prior_freq = factor(prior_freq, levels = c("MF-SS", "MF-Minn", "QF-SS", "QF-Minn")))

# To also consider intraquarter differences
plot3 <- plot_dat %>%
  group_by(lambda1, lambda2, h, prior, freq, intraquarter) %>%
  summarize(rmse = sqrt(mean(squared_error))) %>%
  unite(hyper, lambda1, lambda2)

plot4 <- plot_dat %>%
  filter(rank == TRUE) %>%
  group_by(h, prior, freq, intraquarter) %>%
  summarize(rmse = sqrt(mean(squared_error))) %>%
  mutate(prior_freq = paste0(freq, "-", prior)) %>%
  mutate(prior_freq = factor(prior_freq, levels = c("MF-SS", "MF-Minn", "QF-SS", "QF-Minn"))) %>%
  mutate(intraquarter = factor(intraquarter, levels = 1:3, labels = paste0("Month ", 1:3)))

test <- readRDS(sprintf("fcst_PIT_%s.rds", suffix))
plot_1 <- test %>%
  ungroup() %>%
  filter(date < ymd("2015-12-31")) %>%
  mutate(intraquarter = month(fcst_date)-3*(quarter(fcst_date)-1),
  h = (year(date) - year(fcst_date))*4 + quarter(date) - quarter(fcst_date))  %>%
  mutate(prior_freq = paste0(prior, "_", freq)) %>%
  filter(h %in% c(0, 1, 2, 4, 8), intraquarter==3) %>%
  mutate(h = factor(h, levels = c(0, 1, 2, 4, 8), labels = paste0(c(0, 1, 2, 4, 8), "-step"))) %>%
  mutate(prior = factor(prior, levels = c("SS", "Minn")))

# Interval coverage
coverage <- readRDS(sprintf("fcst_coverage_%s.rds", suffix))
  mean_coverage <- coverage %>%
  mutate(h = factor(h, levels = c(0, 1, 2, 4, 8), labels = paste0(c(0, 1, 2, 4, 8), "-step"))) %>%
  mutate(prior = factor(prior, levels = c("SS", "Minn"))) %>%
  group_by(prior, freq, h, coverage) %>%
  summarize(empirical = mean(covered))


