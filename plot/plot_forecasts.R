source("plot_prep.R")
source("plot_settings.R")

rmse <- ggplot(data = plot2, aes(x = h, y = rmse))+geom_line(aes(linetype=prior_freq))+
  geom_point(aes(shape = prior_freq), fill = "white", size = 2) +
  scale_color_grey(start=0, end=0.6) +
  labs(y = "Root mean squared forecast errors",
       x = "Forecast horizon (quarters)",
       shape = "Model",
       linetype = "Model") +
  scale_shape_manual(values = 21:24) +
  scale_linetype_manual() +
  full_size

rmse_intra <- ggplot(data = plot4, aes(x = h, y = rmse))+geom_line(aes(linetype=prior_freq))+
  geom_point(aes(shape = prior_freq), fill = "white", size = 2) +
  scale_color_grey(start=0, end=0.6) +
  labs(y = "Root mean squared forecast errors",
       x = "Forecast horizon (quarters)",
       shape = "Model",
       linetype = "Model") +
  scale_shape_manual(values = 21:24)+
  facet_grid(intraquarter~.) +
  scale_linetype_manual() +
  full_size +
  facet_lines

rmse_cumul <- plot_dat %>%
  filter(rank == TRUE, h == 0) %>%
  group_by(prior, freq) %>%
  arrange(fcst_date, date) %>%
  mutate(cum_error = sqrt(cummean(squared_error)), prior_freq = paste0(freq, "-", prior)) %>%
  mutate(prior_freq = factor(prior_freq, levels = c("MF-SS", "MF-Minn", "QF-SS", "QF-Minn"))) %>%
  ggplot(aes(x = fcst_date, y = cum_error))+geom_line(aes(linetype=prior_freq)) +
  labs(y = "Cumulative RMSFE",
       x = "Date",
       color = "Model",
       linetype = "Model") +
  scale_shape_manual(values = 21:24) +
  scale_linetype_manual() +
  full_size

pit <- ggplot(plot_1) +
  geom_histogram(aes(x=z, y=20*..count../sum(..count..)), bins = 5, size = 0.5, fill = "white", color = "black") +
  geom_hline(aes(yintercept=0.2))+
  facet_grid(h~freq+prior) +
  labs(y = "Proportion",
       x = "Probability integral transform") +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.2)) +
  full_size

cover <- ggplot(mean_coverage) +
  geom_abline(intercept = 0, slope = 1, linetype = 3)  +
  geom_line(aes(x=coverage, y=empirical))+
  facet_grid(h~freq+prior) +
  labs(x = "Nominal coverage", y = "Empirical coverage") +
  full_size +
  facet_lines

tikz(file = "figure/rmse.tex", width = 6, height = 0.5*6)
rmse
dev.off()

tikz(file = "figure/rmse-intra.tex", width = 6, height = 0.62*6)
rmse_intra
dev.off()

tikz(file = "figure/rmse-cumul.tex", width = 6, height = 0.5*6)
rmse_cumul
dev.off()

tikz(file = "figure/pit.tex", width = 6, height = 0.62*6)
pit
dev.off()

tikz(file = "figure/cover.tex", width = 6, height = 6)
cover
dev.off()
