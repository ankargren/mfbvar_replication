directory <- "D:/mfbvar/small_grid"

library(tidyverse)
map_fun <- function(x) {
  x$fcst %>%
    group_by(date, fcst_date) %>%
    summarize(mean = mean(fcst)) %>%
    ungroup() %>%
    mutate(prior = x[["prior"]], time = x[["time"]], freq = x[["freq"]], lambda1 = x[["lambda1"]],
           lambda2 = x[["lambda2"]], log_mdd = x[["log_mdd"]])
}


files <- list.files(directory, full.names = TRUE)

temp <- readRDS(files[1])
fcst <- map_df(temp, map_fun)
for (i in 2:length(files)) {
  temp <- readRDS(files[i])
  temp2 <- map_df(temp[sapply(temp, function(x) !is.null(x$fcst))], map_fun)
  fcst <- bind_rows(fcst, temp2)
}

saveRDS(fcst, file = paste0(dirname(directory), "/fcst_mean_", basename(directory), ".rds"), compress = "xz")
