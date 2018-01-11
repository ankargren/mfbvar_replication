freq <- "QF"
prior <- "Minn"
source("setup.R")
temp <- expand.grid(time = start_period:end_period,
                    prior = prior,
                    freq = freq)

l1 <- seq(0.01, 1, length.out = l_out)
l2 <- seq(0.01, 4, length.out = l_out)

cat("Initializing cluster\n")
cl <- parallel::makeCluster(n_cores)

parallel::clusterExport(cl, varlist = c("mf_list", "qf_list", "d_list", "d_fcst_list", "d_list_qf", "d_fcst_list_qf", "n_fcst",
                                        "n_lags", "prior_Pi_AR1", "lambda3", "n_burnin", "n_reps", "prior_psi_mean", "prior_psi_Omega","zoom_fun",
                                        "verbose", "gibbs_sampler_qf", "qfbvar", "gibbs_sampler_minn_qf", "qfbvar_minn",
                                        "mdd1_qf", "lambda3"))

parallel::clusterEvalQ(cl, {
  library(mfbvar)
  library(tibble)
  library(tidyr)
  library(dplyr)
  library(lubridate)
})
start_time <- Sys.time()
cat("Starting at", as.character(start_time), "\n")

res <- parallel::parLapply(cl = cl, 1:nrow(temp), fun = par_fun, temp = temp, l1 = l1, l2 = l2, n_fcst = n_fcst, n_lags = n_lags, prior_Pi_AR1 = prior_Pi_AR1,
                           prior_psi_mean = prior_psi_mean, prior_psi_Omega = prior_psi_Omega, n_burnin = n_burnin, n_reps = n_reps, verbose = FALSE, lambda3 = lambda3)
parallel::stopCluster(cl)
end_time <- Sys.time()
cat("Ending at", as.character(end_time), "\n")
