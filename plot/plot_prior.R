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

prior <- matrix(c(6.5, 7.5,
                  0.4/3, 0.6/3,
                  0, 1,
                  -0.1, 0.1,
                  0.5, 0.65), ncol = 2, byrow = TRUE)

grey_col <- "grey70"
df <- monthly_tbl[192,2] %>% unnest %>% transmute(date = date, unemp = unemp, lower = prior[1,1], upper = prior[1,2])
prior_unemp <- ggplot(df) +
  geom_ribbon(aes(x=date, ymin=lower,ymax=upper), fill = grey_col) +
  geom_line(aes(x=date, y=unemp)) +
  geom_hline(yintercept = rowMeans(prior)[1], linetype = "dashed") +
  labs(y="", x = "Year") +
  scale_x_date(expand=c(0,0))

df <- monthly_tbl[192,2] %>% unnest %>% transmute(date = date, infl = infl, lower = prior[2,1], upper = prior[2,2])
prior_infl <- ggplot(df) +
  geom_ribbon(aes(x=date, ymin=lower,ymax=upper), fill = grey_col) +
  geom_line(aes(x=date, y=infl)) +
  geom_hline(yintercept = rowMeans(prior)[2], linetype = "dashed") +
  labs(y="", x = "Year") +
  scale_x_date(expand=c(0,0))

df <- monthly_tbl[192,2] %>% unnest %>% transmute(date = date, ip = ip, lower = prior[3,1], upper = prior[3,2])
prior_ip <- ggplot(df) +
  geom_ribbon(aes(x=date, ymin=lower,ymax=upper), fill = grey_col) +
  geom_line(aes(x=date, y=ip)) +
  geom_hline(yintercept = rowMeans(prior)[3], linetype = "dashed") +
  labs(y="", x = "Year") +
  scale_x_date(expand=c(0,0))

df <- monthly_tbl[192,2] %>% unnest %>% transmute(date = date, eti = eti, lower = prior[4,1], upper = prior[4,2])
prior_eti <- ggplot(df) +
  geom_ribbon(aes(x=date, ymin=lower,ymax=upper), fill = grey_col) +
  geom_line(aes(x=date, y=eti)) +
  geom_hline(yintercept = rowMeans(prior)[4], linetype = "dashed") +
  labs(y="", x = "Year") +
  scale_x_date(expand=c(0,0))

df <- monthly_tbl[192,2] %>% unnest %>% transmute(date = date, gdp = gdp, lower = prior[5,1], upper = prior[5,2]) %>% na.omit()
prior_gdp <- ggplot(df) +
  geom_ribbon(aes(x=date, ymin=lower,ymax=upper), fill = grey_col) +
  geom_line(aes(x=date, y=gdp)) +
  geom_hline(yintercept = rowMeans(prior)[5], linetype = "dashed") +
  labs(y="", x = "Year") +
  scale_x_date(expand=c(0,0))

abbrev <- c("unemp", "infl", "ip", "eti", "gdp")
tikzDevice::tikz(file = sprintf("figure/prior-%s.tex", abbrev[1]), width = 6, height = 0.62*6)
prior_unemp
dev.off()

tikzDevice::tikz(file = sprintf("figure/prior-%s.tex", abbrev[2]), width = 6, height = 0.62*6)
prior_infl
dev.off()

tikzDevice::tikz(file = sprintf("figure/prior-%s.tex", abbrev[3]), width = 6, height = 0.62*6)
prior_ip
dev.off()

tikzDevice::tikz(file = sprintf("figure/prior-%s.tex", abbrev[4]), width = 6, height = 0.62*6)
prior_eti
dev.off()

tikzDevice::tikz(file = sprintf("figure/prior-%s.tex", abbrev[5]), width = 6, height = 0.62*6)
prior_gdp
dev.off()
