source("plot_settings.R")
load("full_tbl.RData")

log_diff <- function(x) {
  100*(log(x) - lag(log(x), 1))
}
scale_diff <- function(x) {
  x <- (x-10)/10
  x <- x - dplyr::lag(x)
}

full_tbl2 <- unnest(full_tbl) %>%
  filter(fcst_date > "2000-01-01" & date > "1996-07-01" & fcst_date < "2016-01-01")

monthly_tbl <- full_tbl2 %>%
  group_by(fcst_date) %>%
  mutate(infl = log_diff(infl), ip = log_diff(ip), eti = scale_diff(eti)) %>%
  nest()

m_tbl <- full_tbl2 %>%
  group_by(fcst_date) %>%
  mutate(infl = log_diff(infl), ip = log_diff(ip), eti = scale_diff(eti)) %>%
  filter(date =="2000-03-31" | date == "2004-06-30" | date == "2008-09-30" | date== "2012-12-31") %>%
  group_by(date)

grey_col <- "grey70"
df <- m_tbl %>% select(fcst_date, date, unemp) %>% ungroup() %>%
  mutate(real_date = date, date = paste0(year(date), "M", month(date)),
         month = month(fcst_date))
rev_unemp <- ggplot(df) +
  geom_line(aes(x=fcst_date, y=unemp, linetype = factor(date)))+
  geom_point(data = filter(df, month %in% c(2, 5, 8, 11)), aes(x=fcst_date, y=unemp, shape = factor(date)), size = 2, fill = "grey50")+
  labs(y="", x = "Vintage", linetype ="Observation", shape = "Observation") +
  theme(legend.key.width = unit(0.5, "inch"),
        legend.key.height = unit(0.3, "inch")) +
  ggplot2::scale_linetype_manual(values = c("solid", "F3", "66", "22"))+
  ggplot2::scale_shape_manual(values = c(NA, NA, NA, NA))

df <- m_tbl %>% select(fcst_date, date, infl) %>% ungroup() %>%
  mutate(real_date = date, date = paste0(year(date), "M", month(date)),
         month = month(fcst_date))
rev_infl <- ggplot(df) +
  geom_line(aes(x=fcst_date, y=infl, linetype = factor(date)))+
  geom_point(data = filter(df, month %in% c(5, 11)), aes(x=fcst_date, y=infl, shape = factor(date)), size = 2, fill = "grey50")+
  labs(y="", x = "Vintage", linetype ="Observation", shape = "Observation") +
  theme(legend.key.width = unit(0.5, "inch"),
        legend.key.height = unit(0.3, "inch")) +
  ggplot2::scale_linetype_manual(values = c("solid", "F3", "66", "22"))+
  ggplot2::scale_shape_manual(values = c(NA, NA, NA, NA))

df <- m_tbl %>% select(fcst_date, date, ip) %>% ungroup() %>%
  mutate(real_date = date, date = paste0(year(date), "M", month(date)),
         month = month(fcst_date))
rev_ip <- ggplot(df) +
  geom_line(aes(x=fcst_date, y=ip, linetype = factor(date)))+
  geom_point(data = filter(df, month %in% c(2, 5, 8, 11)), aes(x=fcst_date, y=ip, shape = factor(date)), size = 2, fill = "grey50")+
  labs(y="", x = "Vintage", linetype ="Observation", shape = "Observation") +
  theme(legend.key.width = unit(0.5, "inch"),
        legend.key.height = unit(0.3, "inch")) +
  ggplot2::scale_linetype_manual(values = c("solid", "F3", "66", "22"))+
  ggplot2::scale_shape_manual(values = c(NA, NA, NA, NA))

df <- m_tbl %>% select(fcst_date, date, eti) %>% ungroup() %>%
  mutate(real_date = date, date = paste0(year(date), "M", month(date)),
         month = month(fcst_date))
rev_eti <- ggplot(df) +
  geom_line(aes(x=fcst_date, y=eti, linetype = factor(date)))+
  geom_point(data = filter(df, month %in% c(2, 5, 8, 11)), aes(x=fcst_date, y=eti, shape = factor(date)), size = 2, fill = "grey50")+
  labs(y="", x = "Vintage", linetype ="Observation", shape = "Observation") +
  theme(legend.key.width = unit(0.5, "inch"),
        legend.key.height = unit(0.3, "inch")) +
  ggplot2::scale_linetype_manual(values = c("solid", "F3", "66", "22"))+
  ggplot2::scale_shape_manual(values = c(NA, NA, NA, NA))

df <- m_tbl %>% select(fcst_date, date, gdp) %>% ungroup() %>%
  mutate(real_date = date, date = paste0(year(date), "M", month(date)),
         month = month(fcst_date))
rev_gdp <- ggplot(df) +
  geom_line(aes(x=fcst_date, y=gdp, linetype = factor(date)))+
  geom_point(data = filter(df, month %in% c(2, 5, 8, 11)), aes(x=fcst_date, y=gdp, shape = factor(date)), size = 2, fill = "grey50")+
  labs(y="", x = "Vintage", linetype ="Observation", shape = "Observation") +
  theme(legend.key.width = unit(0.5, "inch"),
        legend.key.height = unit(0.3, "inch")) +
  ggplot2::scale_linetype_manual(values = c("solid", "F3", "66", "22"))+
  ggplot2::scale_shape_manual(values = c(NA, NA, NA, NA))


abbrev <- c("unemp", "infl", "ip", "eti", "gdp")
tikzDevice::tikz(file = sprintf("figure/rev-%s.tex", abbrev[1]), width = 6, height = 0.62*6)
rev_unemp
dev.off()

tikzDevice::tikz(file = sprintf("figure/rev-%s.tex", abbrev[2]), width = 6, height = 0.62*6)
rev_infl
dev.off()

tikzDevice::tikz(file = sprintf("figure/rev-%s.tex", abbrev[3]), width = 6, height = 0.62*6)
rev_ip
dev.off()

tikzDevice::tikz(file = sprintf("figure/rev-%s.tex", abbrev[4]), width = 6, height = 0.62*6)
rev_eti
dev.off()

tikzDevice::tikz(file = sprintf("figure/rev-%s.tex", abbrev[5]), width = 6, height = 0.62*6)
rev_gdp
dev.off()

