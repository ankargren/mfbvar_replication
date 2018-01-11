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


m_tbl <- full_tbl2 %>%
  group_by(fcst_date) %>%
  mutate(infl = log_diff(infl), ip = log_diff(ip), eti = scale_diff(eti)) %>%
  filter(date =="2000-03-31" | date == "2004-06-30" | date == "2008-09-30" | date== "2012-12-31") %>%
  group_by(date)

monthly_tbl <- full_tbl2 %>%
  group_by(fcst_date) %>%
  mutate(infl = log_diff(infl), ip = log_diff(ip), eti = scale_diff(eti)) %>%
  nest()

missing_end <- function(x) {
  length(x) - max(which(!is.na(x)))
}

end_NA <- monthly_tbl %>%
  unnest() %>%
  group_by(fcst_date) %>%
  summarize_all(.funs=funs(missing_end)) %>%
  mutate(month= month(fcst_date)%%3) %>%
  mutate(month=replace(month, month==0, 3)) %>% select(-date, -interest)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


library("RColorBrewer")
pattern_fun <- function(var_name, title_name) {
  Sys.setlocale("LC_TIME", "C")
  temp <- end_NA %>% filter(fcst_date > "2002-03-30") %>%
    mutate(year=year(fcst_date), month=firstup(months(fcst_date, abbreviate=TRUE)))
  temp <- temp %>% select(year, month, which(names(temp) == var_name)) %>%
    mutate(month = factor(month, levels = end_NA$fcst_date%>%months(abbreviate=TRUE) %>% unique))
  names(temp)[3] <- "variable"
  temp <- temp %>%  mutate(variable = factor(variable, levels = as.character(0:5)))

  if (var_name == "unemp") {
    temp$variable[40:42] <- NA
  }
  ggplot(temp, aes(x=year, y=month)) +
    geom_tile(aes(fill=factor(variable)), height = 0.8, width = 0.9) +
    labs(y="Month", x = "Year") +
    theme(axis.text.y = element_text(size = 18, color = "black"),
          axis.text.x = element_text(size = 18),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.text=element_text(size=18),
          legend.title=element_text(size=20),
          plot.title = element_text(size=18)) +
    scale_y_discrete(labels=c("Feb" = "", "Mar" = "", "May" = "", "Jun" = "",
                              "Aug" = "", "Sep" = "", "Nov" = "", "Dec" = "")) +
    scale_fill_brewer(name = "Delay", palette = "Reds", na.value = "white", limits = as.character(0:5))+
    #scale_fill_grey(name = "Delay", limits = as.character(0:5), start = 0.8, end = 0.2, na.value = "white") +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank())

}
abbrev <- c("unemp", "infl", "ip", "eti", "gdp")
fullname <- c("Unemployment rate", "Inflation rate", "Industrial production",
              "Economic tendency indicator", "GDP growth")

tikzDevice::tikz(file = sprintf("figure/rel-%s.tex", abbrev[1]), width = 6, height = 0.62*6)
pattern_fun(abbrev[1], fullname[1])
dev.off()

tikzDevice::tikz(file = sprintf("figure/rel-%s.tex", abbrev[2]), width = 6, height = 0.62*6)
pattern_fun(abbrev[2], fullname[2])
dev.off()

tikzDevice::tikz(file = sprintf("figure/rel-%s.tex", abbrev[3]), width = 6, height = 0.62*6)
pattern_fun(abbrev[3], fullname[3])
dev.off()

tikzDevice::tikz(file = sprintf("figure/rel-%s.tex", abbrev[4]), width = 6, height = 0.62*6)
pattern_fun(abbrev[4], fullname[4])
dev.off()

tikzDevice::tikz(file = sprintf("figure/rel-%s.tex", abbrev[5]), width = 6, height = 0.62*6)
pattern_fun(abbrev[5], fullname[5])
dev.off()
