# Prepare data
cat("Preparing data\n")
load("mf_list.RData")
# Some vintages lack a lot of data, fill them in
for (i in 66:87) {
  na_ind <- which(is.na(mf_list$data[[i]][, 1]))
  na_ind <- na_ind[-((length(na_ind)-1):length(na_ind))]
  mf_list$data[[i]][na_ind, 1] <- mf_list$data[[88]][na_ind, 1]
}

# Create quarterly data set
qf_data <- lapply(mf_list$data, FUN = function(x) {
  dat <- x %>%
    mutate(date = rownames(x), year = year(date), quarter = quarter(date)) %>%
    group_by(year, quarter) %>%
    summarize(unemp = mean(unemp), infl = mean(infl), ip = mean(ip), eti = mean(eti), gdp = mean(gdp, na.rm = TRUE)) %>%
    mutate(date = ymd(paste(year, quarter*3, "01", sep = "-")) + months(1)-days(1)) %>%
    ungroup() %>%
    select(-year, -quarter) %>%
    as.data.frame()
  rownames(dat) <- dat$date
  dat %>% select(-date)})
qf_list <- list(fcst_date = mf_list$fcst_date, data = lapply(qf_data, na.omit))

d_list <- lapply(mf_list$data, function(x) matrix(1, nrow = nrow(x), ncol = 1, dimnames = list(time = rownames(x), const = "const")))
d_fcst_list <- lapply(as.Date(sapply(mf_list$data, function(x) rownames(x)[nrow(x)]), origin = "1970-01-01"),
                      function(x) matrix(1, nrow = 3*n_fcst, ncol = 1, dimnames =
                                           list(time = as.character(x + lubridate::days(1) + months(1:(3*n_fcst), abbreviate = TRUE) - lubridate::days(1)), const = "const")))

d_list_qf <- lapply(qf_list$data, function(x) matrix(1, nrow = nrow(x), ncol = 1, dimnames = list(time = rownames(x), const = "const")))
d_fcst_list_qf <- lapply(as.Date(sapply(qf_list$data, function(x) rownames(x)[nrow(x)]), origin = "1970-01-01"),
                         function(x) matrix(1, nrow = n_fcst, ncol = 1, dimnames = list(time = as.character(x + lubridate::days(1) + months(1:n_fcst, abbreviate = TRUE) - lubridate::days(1)), const = "const")))










