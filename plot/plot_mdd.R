library(tidyverse)
library(lubridate)
library(mfbvar)
source("plot_prep.R")
source("plot_settings.R")

fcst_mdd <- plot_dat %>%
  filter(rank == TRUE) %>%
  select(fcst_date, prior, freq, lambda1, lambda2) %>%
  distinct()

hyper_plot_n <- fcst_mdd %>%
  count(prior, freq, lambda1, lambda2) %>%
  ggplot(aes(x = lambda1, y = lambda2)) +
  geom_point(aes(size = n), shape = 21, fill = "white") +
  facet_wrap(freq ~ prior) +
  full_size +
  labs(x = "$\\lambda_1$", y = "$\\lambda_2$",
       size = "")

hyper_plot_ts <- fcst_mdd %>%
  gather(lambda1:lambda2, key = "hyper", value = "value") %>%
  mutate(hyper = replace(hyper, hyper == "lambda1", "$\\lambda_1$"),
         hyper = replace(hyper, hyper == "lambda2", "$\\lambda_2$"),
         prior = factor(prior, levels = c("SS", "Minn"))) %>%
  ggplot(aes(x = fcst_date, y = value)) +
  geom_line(aes(group = hyper), size = 0.2) +
  geom_point(aes(fill = hyper), size = 1.5, stroke = 0.1, shape = 21) +
  facet_wrap(freq ~ prior, scales = "free_y") +
  full_size +
  scale_fill_grey() +
  labs(x = "Forecast origin", y = "Value",
       fill = "Hyper-\nparameter")

full_data <- plot_dat %>%
  select(fcst_date, prior, freq, lambda1, lambda2, time) %>%
  distinct() %>%
  group_by(prior, freq) %>%
  count(lambda1, lambda2) %>%
  arrange(desc(n)) %>%
  filter(n >= 141) %>%
  select(-n) %>%
  mutate(always_included = TRUE) %>%
  left_join(plot_dat, ., by = c("prior", "freq", "lambda1", "lambda2")) %>%
  filter(always_included == TRUE) %>%
  mutate(l1l2 = paste0(lambda1, "_", lambda2)) %>%
  group_by(l1l2, prior, freq, h) %>%
  summarize(rmse = sqrt(mean(squared_error))) %>%
  bind_rows(plot2, .id = "type") %>%
  ungroup() %>%
  mutate(prior = factor(prior, levels = c("SS", "Minn")))

rmse_hyper <- ggplot(filter(full_data, type == "1"), aes(x = h, y = rmse)) +
  geom_line(aes(group = l1l2), size = 0.1) +
  geom_point(data = filter(full_data, type == "2"), shape = 21,
             fill = "white", size = 2.5) +
  facet_wrap(freq ~ prior) +
  labs(x = "Root mean squared forecast errors",
       y = "Forecast horizon") +
  full_size +
  facet_lines

tikz(file = "figure/hyper-n.tex", width = 6, height = 0.7*6)
hyper_plot_n
dev.off()

tikz(file = "figure/hyper-ts.tex", width = 6, height = 0.7*6)
hyper_plot_ts
dev.off()

tikz(file = "figure/rmse-hyper.tex", width = 6, height = 0.7*6)
rmse_hyper
dev.off()
