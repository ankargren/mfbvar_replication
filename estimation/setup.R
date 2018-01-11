cat("Checking if packages are installed\n")
list.of.packages <- c("parallel", "tidyverse", "lubridate", "Rcpp", "RcppArmadillo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages, repos = "http://cran.case.edu" )

for (i in seq_along(list.of.packages)) {
  library(list.of.packages[i], character.only = TRUE)
}

if (!("devtools" %in% installed.packages()[,"Package"])) {
  install.packages("devtools", repos = "http://cran.case.edu" )
}
devtools::install_github("ankargren/mfbvar")

library(mfbvar)

source("quarterly_models.R")

start_period <- 49
end_period <- 192
n_cores <- 20
l_out <- 7
prior_Pi_AR1 <- rep(0, 5)
n_fcst <- 10
n_lags <- 4
lambda3 <- 100
n_burnin <- 5000
n_reps <- 15000
intervals <- matrix(c(6.5, 7.5,
                      0.4/3, 0.6/3,
                      0, 1,
                      -0.1, 0.1,
                      0.5, 0.65), ncol = 2, byrow = TRUE)
prior_psi_mean <- interval_to_moments(intervals)$prior_psi_mean
prior_psi_Omega <- interval_to_moments(intervals)$prior_psi_Omega
verbose <- FALSE

source("prepare_data.R")

zoom_fun <- function(lambda1, lambda2, prior, freq, time, fcst_vec, ...) {
  cat(lambda1, lambda2, "\n")
  arg_list <- list(...)
  Y <- arg_list$Y
  prior_Pi_AR1 <- arg_list$prior_Pi_AR1
  n_lags <- arg_list$n_lags
  n_fcst <- arg_list$n_fcst
  n_burnin <- arg_list$n_burnin
  n_reps <- arg_list$n_reps
  verbose <- FALSE
  data_freq <- c("m", "m", "m", "m", "q")

  prior_nu <- ncol(Y) + 2
  prior_psi_mean <- arg_list$prior_psi_mean
  prior_psi_Omega <- arg_list$prior_psi_Omega
  d <- arg_list$d
  d_fcst <- arg_list$d_fcst
  lambda3 <- arg_list$lambda3

  if (freq == "MF") {
    prior_obj <- set_prior(Y = Y, freq = data_freq, prior_Pi_AR1 = prior_Pi_AR1, lambda1 = lambda1, lambda2 = lambda2,
                           n_lags = n_lags, n_fcst = 3*n_fcst, n_burnin = n_burnin, n_reps = n_reps, d = d, d_fcst = d_fcst,
                           prior_psi_mean = prior_psi_mean, prior_psi_Omega = prior_psi_Omega, lambda3 = lambda3, verbose = verbose)
  }


  set.seed(time+3)
  if (prior == "SS") {
    if (freq == "MF") {
      mfbvar_obj <- #tryCatch({
        estimate_mfbvar(prior_obj, prior_type = "ss")
      #}, error = function(cond) {NULL})
    }
    if (freq == "QF") {
      mfbvar_obj <- tryCatch({
        qfbvar(Y = Y, d = d, d_fcst = d_fcst, prior_Pi_AR1 = prior_Pi_AR1, lambda1 = lambda1, lambda2 = lambda2,
               prior_nu = prior_nu, prior_psi_mean = prior_psi_mean, prior_psi_Omega = prior_psi_Omega, n_lags = n_lags, n_fcst = n_fcst,
               n_burnin = n_burnin, n_reps = n_reps, verbose = verbose)
      }, error = function(cond) {NULL})
    }
  } else {
    if (freq == "MF") {
      mfbvar_obj <- tryCatch({
        estimate_mfbvar(prior_obj, prior_type = "minn")
      }, error = function(cond) {NULL})
    }
    if (freq == "QF") {
      priors_extracted <- mfbvar:::prior_Pi_Sigma(lambda1, lambda2, prior_Pi_AR1, Y, n_lags, prior_nu)
      mfbvar_obj <- #tryCatch({
        qfbvar_minn(Y = Y, prior_Pi_mean = priors_extracted$prior_Pi_mean, prior_Pi_Omega = priors_extracted$prior_Pi_Omega, prior_S = priors_extracted$prior_S,
                    lambda3 = lambda3, n_fcst = n_fcst, n_burnin = n_burnin, n_reps = n_reps, verbose = verbose)
      #}, error = function(cond) {NULL})
    }
  }

  if (!is.null(mfbvar_obj$Z_fcst)) {
    fcst <- t(mfbvar_obj$Z_fcst[,5,-1])
    if (prior == "SS") {
      if (freq == "MF") {
        mdd_est <- tryCatch({
          mdd(mfbvar_obj)
        }, error = function(cond) {
          NA
        })
      }
      if (freq == "QF") {
        mdd_est <- tryCatch({
          mdd1_qf(mfbvar_obj)$log_mdd
        }, error = function(cond) {
          NA
        })
      }

    }
    if (prior == "Minn") {
      if (freq == "MF") {
        mdd_est <- tryCatch({
          mdd(mfbvar_obj, p_trunc = 0.5)
        }, error = function(cond) {
          NA
        })
      }
      if (freq == "QF") {
        mdd_est <- mfbvar_obj$lnpYY
      }
    }
    if (freq == "MF") {
      colnames(fcst)[-(1:4)] <- as.character(floor_date(ymd(colnames(fcst)[4]), unit = "month") + months(1:(ncol(fcst)-4)) + months(1) - days(1))
      fcst2 <- as_tibble(fcst) %>%
        mutate(iteration = 1:n()) %>%
        gather(1:ncol(fcst), key = "date", value = "fcst") %>%
        mutate(date = ymd(date),
               year = year(date),
               quarter = quarter(date)) %>%
        group_by(iteration, year, quarter) %>%
        summarize(fcst = mean(fcst),
                  date = last(date),
                  n = n()) %>%
        filter(n == 3) %>%
        mutate(fcst_date = fcst_vec) %>%
        ungroup() %>%
        select(iteration, fcst, date, fcst_date)
    } else {
      colnames(fcst)[-(1:4)] <- as.character(floor_date(ymd(colnames(fcst)[4]), unit = "month") + months(3*(1:(ncol(fcst)-4))) + months(1) - days(1))
      fcst2 <- as_tibble(fcst) %>%
        mutate(iteration = 1:n()) %>%
        gather(1:ncol(fcst), key = "date", value = "fcst") %>%
        mutate(fcst_date = fcst_vec, date = ymd(date)) %>%
        select(iteration, fcst, date, fcst_date)
    }
  }  else {
    mdd_est <- NA
    fcst2 <- NULL
  }

  #res_i <- list(prior = as.character(prior), time = time, freq = as.character(freq), lambda1 = lambda1, lambda2 = lambda2, log_mdd = c(mdd_est), fcst = fcst2)
  #save(res_i, file = paste0(Sys.getenv("SNIC_TMP"), freq, "_", prior, "_", time, "_lambda1-", gsub("\\.", "", as.character(lambda1)), "_lambda2-", lambda2, ".RData"))
  return(list(prior = as.character(prior), time = time, freq = as.character(freq), lambda1 = lambda1, lambda2 = lambda2, log_mdd = c(mdd_est), fcst = fcst2))
}

par_fun <- function(x, temp, l1, l2, ...) {
  time <- temp[x, "time"]
  prior <- temp[x, "prior"]
  freq <- temp[x, "freq"]
  if (freq == "QF") {
    Y <- qf_list$data[[time]]
    d <- d_list_qf[[time]]
    d_fcst <- d_fcst_list_qf[[time]]
    fcst_vec <- qf_list$fcst_date[time]
  } else {
    Y <- mf_list$data[[time]]
    d <- d_list[[time]]
    d_fcst <- d_fcst_list[[time]]
    fcst_vec <- mf_list$fcst_date[time]
  }

  l_out <- length(l1)

  lam_1 <- expand.grid(l1 = l1, l2 = l2, mdd = NA)
  outlist <- vector("list", l_out^2)
  cat("\n ======================= \n STEP 1 \n ======================= \n")
  for (i in 1:nrow(lam_1)) {
    outlist[[i]] <- zoom_fun(lambda1 = lam_1[i, 1], lambda2 = lam_1[i, 2], prior, freq, Y = Y, d = d, d_fcst = d_fcst, time = time, fcst_vec = fcst_vec, ...)
  }

  # Step 2
  step_1_mdd <- as_tibble(t(sapply(outlist[1:nrow(lam_1)], function(x) c(l1 = x$lambda1, l2 = x$lambda2, mdd = x$log_mdd))))
  l1_pos <- which(dplyr::near(round(step_1_mdd$l1[which.max(step_1_mdd$mdd)], digits = 4), round(l1, digits = 4)))
  l2_pos <- which(dplyr::near(round(step_1_mdd$l2[which.max(step_1_mdd$mdd)], digits = 4), round(l2, digits = 4)))
  l1_2 <- seq(l1[ifelse(l1_pos == 1, 1, l1_pos - 1)], l1[ifelse(l1_pos == l_out, l_out, l1_pos + 1)], length.out = l_out)[-c(1, l_out)]
  l2_2 <- seq(l2[ifelse(l2_pos == 1, 1, l2_pos - 1)], l2[ifelse(l2_pos == l_out, l_out, l2_pos + 1)], length.out = l_out)[-c(1, l_out)]
  lam_2 <- expand.grid(l1 = l1_2,
                       l2 = l2_2)
  print(as_tibble(round(lam_2, digits = 4)))
  print(as_tibble(round(step_1_mdd, digits = 4)))
  step_2_mdd <- left_join(as_tibble(round(lam_2, digits = 4)), as_tibble(round(step_1_mdd, digits = 4)), by = c("l1", "l2"), copy = TRUE)
  cat("x\n")
  x <- which(is.na(step_2_mdd$mdd))
  lam_2 <- lam_2[x, ]

  step_2 <- vector("list", length(x))
  cat("\n ======================= \n STEP 2 \n ======================= \n")
  for (i in 1:length(x)) {
    step_2[[i]] <- zoom_fun(lambda1 = lam_2[i, 1], lambda2 = lam_2[i, 2], prior, freq, Y = Y, d = d, d_fcst = d_fcst, time = time, fcst_vec = fcst_vec, ...)
  }
  #step_2 <- lapply(x, zoom_fun, lam_mat = lam_2, freq = freq, prior = prior)

  mdd_temp <- t(sapply(step_2, function(x) c(l1 = x$lambda1, l2 = x$lambda2, mdd = x$log_mdd)))
  outlist <- append(outlist, step_2)
  rm(step_2)

  step_2_mdd <- left_join(as_tibble(round(step_2_mdd, digits = 4)), as_tibble(round(mdd_temp, digits = 4)), by = c("l1", "l2"), copy = TRUE) %>%
    transmute(l1, l2, mdd = ifelse(is.na(mdd.x), mdd.y, mdd.x))

  # Step 3
  l1_pos <- which(dplyr::near(round(step_2_mdd$l1[which.max(step_2_mdd$mdd)], digits = 4), round(l1_2, digits = 4)))
  l2_pos <- which(dplyr::near(round(step_2_mdd$l2[which.max(step_2_mdd$mdd)], digits = 4), round(l2_2, digits = 4)))

  l1_3 <- seq(l1_2[ifelse(l1_pos == 1, 1, l1_pos - 1)], l1_2[ifelse(l1_pos == l_out - 2, l_out - 2, l1_pos + 1)], length.out = l_out - 2)[-c(1, l_out - 2)]
  l2_3 <- seq(l2_2[ifelse(l2_pos == 1, 1, l2_pos - 1)], l2_2[ifelse(l2_pos == l_out - 2, l_out - 2, l2_pos + 1)], length.out = l_out - 2)[-c(1, l_out - 2)]
  lam_3 <- expand.grid(l1 = l1_3,
                       l2 = l2_3)

  step_3_mdd <- left_join(round(lam_3, digits = 4), round(step_2_mdd, digits = 4), by = c("l1", "l2"), copy = TRUE)
  x <- which(is.na(step_3_mdd$mdd))
  lam_3 <- lam_3[x, ]
  step_3 <- vector("list", length(x))
  cat("\n ======================= \n STEP 3 \n ======================= \n")
  for (i in 1:length(x)) {
    step_3[[i]] <- zoom_fun(lambda1 = lam_3[i, 1], lambda2 = lam_3[i, 2], prior, freq, Y = Y, d = d, d_fcst = d_fcst, time = time, fcst_vec = fcst_vec, ...)
  }
  #step_3 <- lapply(x, zoom_fun, lam_mat = lam_3, freq = freq, prior = prior)
  outlist <- append(outlist, step_3)
  mdd_temp <- t(sapply(step_3, function(x) c(l1 = x$lambda1, l2 = x$lambda2, mdd = x$log_mdd)))

  step_3_mdd <- left_join(as_tibble(round(step_3_mdd, digits = 4)), as_tibble(round(mdd_temp, digits = 4)), by = c("l1", "l2"), copy = TRUE) %>%
    transmute(l1, l2, mdd = ifelse(is.na(mdd.x), mdd.y, mdd.x))

  # save
  saveRDS(outlist, file = paste0("/proj/snic2015-6-117/nobackup/", freq, "_", prior, "_", time, ".rds"), compress = "xz")
  return(1)

}

fixed_fun <- function(x, temp, lambda1, lambda2, ...) {
  time <- temp[x, "time"]
  prior <- temp[x, "prior"]
  freq <- temp[x, "freq"]
  if (freq == "QF") {
    Y <- qf_list$data[[time]]
    d <- d_list_qf[[time]]
    d_fcst <- d_fcst_list_qf[[time]]
    fcst_vec <- qf_list$fcst_date[time]
  } else {
    Y <- mf_list$data[[time]]
    d <- d_list[[time]]
    d_fcst <- d_fcst_list[[time]]
    fcst_vec <- mf_list$fcst_date[time]
  }

  out <- zoom_fun(lambda1 = lambda1, lambda2 = lambda2, prior, freq, Y = Y, d = d, d_fcst = d_fcst, time = time, fcst_vec = fcst_vec, ...)
  saveRDS(out, file = paste0("/proj/snic2015-6-117/nobackup/", freq, "_", prior, "_fixed_", time, ".rds"), compress = "xz")
  return(1)
}
