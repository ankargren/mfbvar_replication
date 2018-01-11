gibbs_sampler_qf <- function(Y, d, d_fcst = NULL, prior_Pi_mean, prior_Pi_Omega, prior_S, prior_nu, prior_psi_mean, prior_psi_Omega,
                             n_fcst = NULL, n_reps, init_Pi = NULL, init_Sigma = NULL, init_psi = NULL, check_roots = TRUE, verbose = TRUE) {

  # n_vars: number of variables
  # n_lags: number of lags
  # n_determ: number of deterministic variables
  # n_T: sample size (full sample)
  # n_T_: sample size (reduced sample)

  n_vars <- dim(Y)[2]
  n_lags <- prod(dim(as.matrix(prior_Pi_mean)))/n_vars^2
  n_determ <- dim(d)[2]
  n_T <- dim(Y)[1]# - n_lags
  n_T_ <- n_T - n_lags

  ################################################################
  ### Preallocation
  # Pi and Sigma store their i-th draws in the third dimension, psi
  # is vectorized so it has its i-th draw stored in the i-th row
  # Pi:    p * pk * n_reps, each [,,i] stores Pi'
  # Sigma: p * p  * n_reps
  # psi:   n_reps * p
  # Z:     T * p * n_reps
  ### If forecasting (h is horizon):
  # Z_fcst: hk * p * n_reps
  # d_fcst_lags: hk * m
  ### If root checking:
  # roots: n_reps vector
  # num_tries: n_reps vector
  ### If smoothing of the state vector:
  # smoothed_Z: T * p * n_reps

  Pi    <- array(NA, dim = c(n_vars, n_vars * n_lags, n_reps))
  Sigma <- array(NA, dim = c(n_vars, n_vars, n_reps))
  psi   <- array(NA, dim = c(n_reps, n_vars * n_determ))
  if (!is.null(n_fcst)) {
    Z_fcst<- array(NA, dim = c(n_fcst+n_lags, n_vars, n_reps),
                   dimnames = list(c((n_T-n_lags+1):n_T, paste0("fcst_", 1:n_fcst)), NULL, NULL))
    d_fcst_lags <- matrix(rbind(d[(n_T-n_lags+1):n_T, , drop = FALSE], d_fcst), nrow = n_fcst + n_lags)
  }
  if (check_roots == TRUE) {
    roots <- vector("numeric", n_reps)
    num_tries <- roots
  }



  ################################################################
  ### Gibbs sampling initialization

  # If the initial values are not provided, the missing values in
  # Z are filled with the next observed value and Pi, Sigma and
  # psi are then computed using maximum likelihood

  # This allows the user to run the Gibbs sampler for a burn-in
  # period, then use the final draw of that as initialization
  # for multiple chains

  ols_results <- tryCatch(mfbvar:::ols_initialization(z = Y, d = d, n_lags = n_lags, n_T = n_T, n_vars = n_vars, n_determ = n_determ),
                          error = function(cond) NULL)
  if (is.null(ols_results)) {
    ols_results <- list()
    ols_results$Pi <- prior_Pi_mean
    ols_results$S <- prior_S
    ols_results$psi <- prior_psi_mean
  }


  if (is.null(init_Pi)) {
    Pi[,, 1]    <- ols_results$Pi
  } else {
    if (all(dim(Pi[,, 1]) == dim(init_Pi))) {
      Pi[,, 1] <- init_Pi
    } else {
      stop(paste0("The dimension of init_Pi is ", paste(dim(init_Pi), collapse = " x "), ", but should be ", paste(dim(Pi[,, 1]), collapse = " x ")))
    }
  }

  # Compute the maximum eigenvalue of the initial Pi
  if (check_roots == TRUE) {
    Pi_comp    <- mfbvar:::build_companion(Pi = Pi[,, 1], n_vars = n_vars, n_lags = n_lags)
    roots[1]   <- mfbvar:::max_eig_cpp(Pi_comp)
  }

  if (is.null(init_Sigma)) {
    Sigma[,, 1] <- ols_results$S
  } else {
    if (all(dim(Sigma[,,1]) == dim(init_Sigma))) {
      Sigma[,, 1] <- init_Sigma
    } else {
      stop(paste0("The dimension of init_Sigma is ", paste(dim(init_Sigma), collapse = " x "), ", but should be ", paste(dim(Sigma[,,1]), collapse = " x ")))
    }
  }

  if (is.null(init_psi)) {
    if (roots[1] < 1) {
      psi[1, ] <- ols_results$psi
    } else {
      psi[1, ] <- prior_psi_mean
    }
  } else {
    if (length(psi[1, ]) == length(init_psi)) {
      psi[1,] <- init_psi
    } else {
      stop(paste0("The length of init_psi is ", paste(length(init_psi), collapse = " x "), ", but should be ", paste(length(psi[1,]), collapse = " x ")))
    }
  }

  ################################################################
  ### Compute terms which do not vary in the sampler

  # Create D (does not vary in the sampler), and find roots of Pi
  # if requested
  D_mat <- mfbvar:::build_DD(d = d, n_lags = n_lags)

  # For the posterior of Pi
  inv_prior_Pi_Omega <- chol2inv(chol(prior_Pi_Omega))
  Omega_Pi <- inv_prior_Pi_Omega %*% prior_Pi_mean



  for (r in 2:(n_reps)) {
    ################################################################
    ### Pi and Sigma step
    #(Z_r1,             d,     psi_r1,                            prior_Pi_mean, inv_prior_Pi_Omega, Omega_Pi, prior_S, prior_nu, check_roots, n_vars, n_lags, n_T)
    Pi_Sigma <- mfbvar:::posterior_Pi_Sigma(Z_r1 = Y, d = d, psi_r1 = psi[r-1, , drop = FALSE], prior_Pi_mean, prior_Pi_Omega, inv_prior_Pi_Omega, Omega_Pi, prior_S, prior_nu, check_roots, n_vars, n_lags, n_T)
    Pi[,,r]      <- Pi_Sigma$Pi_r
    Sigma[,,r]   <- Pi_Sigma$Sigma_r
    num_tries[r] <- Pi_Sigma$num_try
    roots[r]     <- Pi_Sigma$root

    ################################################################
    ### Steady-state step
    #(Pi_r,            Sigma_r,               Z_r1,             prior_psi_mean, prior_psi_Omega, D, n_vars, n_lags, n_determ)
    psi[r, ] <- mfbvar:::posterior_psi(Pi_r = Pi[,, r], Sigma_r = Sigma[,, r], Z_r1 = Y, prior_psi_mean, prior_psi_Omega, D_mat, n_vars, n_lags, n_determ)

    ################################################################
    ### Forecasting step
    if (!is.null(n_fcst)) {

      # Forecast the process with mean subtracted
      Z_fcst[1:n_lags, , r] <- Y[(n_T - n_lags+1):n_T,] - d[(n_T - n_lags+1):n_T, ] %*% t(matrix(psi[r, ], nrow = n_vars))
      for (h in 1:n_fcst) {
        Z_fcst[n_lags + h, , r] <- Pi[,, r] %*% matrix(c(t(Z_fcst[(n_lags+h-1):h,, r])), ncol = 1) +
          mfbvar:::rmultn(m = matrix(0, nrow = n_vars), Sigma = Sigma[,,r])
      }

      # Add the mean
      Z_fcst[, , r] <- Z_fcst[, , r] + d_fcst_lags %*% t(matrix(psi[r, ], nrow = n_vars))
    }

  }

  ################################################################
  ### Prepare the return object
  return_obj <- list(Pi = Pi, Sigma = Sigma, psi = psi, Z = Y, roots = NULL, num_tries = NULL,
                     Z_fcst = NULL, mdd = NULL, n_determ = n_determ,
                     n_lags = n_lags, n_vars = n_vars, n_fcst = n_fcst, prior_Pi_Omega = prior_Pi_Omega, prior_Pi_mean = prior_Pi_mean,
                     prior_S = prior_S, prior_nu = prior_nu, post_nu = n_T_ + prior_nu, d = d, Y = Y, n_T = n_T, n_T_ = n_T_,
                     prior_psi_Omega = prior_psi_Omega, prior_psi_mean = prior_psi_mean, n_reps = n_reps)

  if (check_roots == TRUE) {
    return_obj$roots <- roots
    return_obj$num_tries <- num_tries
  }
  if (!is.null(n_fcst)) {
    return_obj$Z_fcst <- Z_fcst
  }

  return(return_obj)

}

qfbvar <- function(Y, d, d_fcst, prior_Pi_AR1, lambda1, lambda2, prior_nu = NULL, prior_psi_mean, prior_psi_Omega, n_lags, n_fcst, n_burnin, n_reps, verbose, ...) {

  stopifnot(is.null(n_fcst) || is.vector(n_fcst))
  stopifnot(is.matrix(d), is.matrix(prior_psi_Omega))
  stopifnot(is.data.frame(Y) || is.matrix(Y), all(apply(Y, 2, is.numeric)))
  stopifnot(is.vector(n_lags), is.vector(n_burnin), is.vector(n_reps), is.vector(lambda1), is.vector(lambda2))
  stopifnot(nrow(Y) == nrow(d), ncol(Y) == length(prior_Pi_AR1), ncol(Y) == length(prior_psi_mean),
            ncol(Y) == sqrt(prod(dim(prior_psi_Omega))))

  if (!is.null(n_fcst)) {
    if (nrow(d_fcst) != n_fcst) {
      stop(paste0("d_fcst has ", nrow(d_fcst), " rows, but n_fcst is ", n_fcst, "."))
    } else {
      if (is.null(rownames(d_fcst))) {
        names_fcst <- paste0("fcst_", 1:n_fcst)
      } else {
        names_fcst <- rownames(d_fcst)
      }

    }
  } else {
    names_fcst <- NULL
  }

  fun_call <- match.call()
  if (is.null(rownames(Y))) {
    names_row <- 1:nrow(Y)
  } else {
    names_row <- rownames(Y)
  }

  if (is.null(colnames(Y))) {
    names_col <- 1:col(Y)
  } else {
    names_col <- colnames(Y)
  }

  if (is.null(colnames(d))) {
    names_determ <- paste0("d", 1:ncol(d))
  } else {
    names_determ <- colnames(d)
  }

  original_Y <- Y
  Y <- as.matrix(Y)
  n_vars <- ncol(Y)
  n_T <- nrow(Y)

  # Set priors
  if (is.null(prior_nu)) {
    prior_nu <- n_vars + 2
  }
  priors <- mfbvar:::prior_Pi_Sigma(lambda1 = lambda1, lambda2 = lambda2, prior_Pi_AR1 = prior_Pi_AR1, Y = Y,
                           n_lags = n_lags, prior_nu = prior_nu)
  prior_Pi_mean <- priors$prior_Pi_mean
  prior_Pi_Omega <- priors$prior_Pi_Omega
  prior_S <- priors$prior_S

  # For the smoothing
  if (verbose) {
    cat(paste0("############################################\n   Running the burn-in sampler with ", n_burnin, " draws\n\n"))
    start_burnin <- Sys.time()
  }
  burn_in <-  gibbs_sampler_qf(Y = Y, d = d, d_fcst = NULL, prior_Pi_mean = prior_Pi_mean, prior_Pi_Omega = prior_Pi_Omega,
                               prior_S = prior_S, prior_nu = prior_nu, prior_psi_mean = prior_psi_mean, prior_psi_Omega = prior_psi_Omega,
                               n_fcst = NULL, n_reps = n_burnin, check_roots = TRUE, verbose = verbose)
  if (verbose) {
    end_burnin <- Sys.time()
    time_diff <- end_burnin - start_burnin
    cat(paste0("\n   Time elapsed for drawing ", n_burnin, " times for burn-in: ", signif(time_diff, digits = 1), " ",
               attr(time_diff, "units"), "\n"))
    cat(paste0("\n   Moving on to ",
               n_reps, " replications in the main chain\n", ifelse(!is.null(n_fcst), paste0("   Making forecasts ", n_fcst, " steps ahead"), " "), "\n\n"))
  }

  main_run <- gibbs_sampler_qf(Y = Y, d = d, d_fcst = d_fcst, prior_Pi_mean = prior_Pi_mean, prior_Pi_Omega = prior_Pi_Omega,
                               prior_S = prior_S, prior_nu = prior_nu, prior_psi_mean = prior_psi_mean, prior_psi_Omega = prior_psi_Omega,
                               n_fcst = n_fcst, n_reps = n_reps, init_Pi  = burn_in$Pi[,,dim(burn_in$Pi)[3]], init_Sigma = burn_in$Sigma[,,dim(burn_in$Sigma)[3]],
                               init_psi = burn_in$psi[dim(burn_in$psi)[1],], check_roots = TRUE, verbose = verbose)
  main_run$call <- fun_call
  if (verbose) {
    time_diff <- Sys.time() - start_burnin
    cat(paste0("\n   Total time elapsed: ", signif(time_diff, digits = 1), " ",
               attr(time_diff, "units"), "\n"))
  }

  if (!is.null(n_fcst)) {
    rownames(main_run$Z_fcst)[1:main_run$n_lags] <- names_row[(main_run$n_T-main_run$n_lags+1):main_run$n_T]
    rownames(main_run$Z_fcst)[(main_run$n_lags+1):(main_run$n_fcst+main_run$n_lags)] <- names_fcst
    colnames(main_run$Z_fcst) <- names_col

  }


  main_run$names_row <- names_row
  main_run$names_col <- names_col
  main_run$names_fcst <- names_fcst
  main_run$names_determ <- names_determ
  main_run$n_burnin <- n_burnin
  main_run$prior_Pi_AR1 <- prior_Pi_AR1

  dimnames(main_run$Z) <- list(time = names_row,
                               variable = names_col)
  dimnames(main_run$Pi) <- list(dep = names_col,
                                indep = paste0(rep(names_col, n_lags), ".l", rep(1:n_lags, each = n_vars)),
                                iteration = 1:n_reps)
  dimnames(main_run$Sigma) <- list(names_col,
                                   names_col,
                                   iteration = 1:n_reps)
  n_determ <- dim(d)[2]
  dimnames(main_run$psi) <- list(iteration = 1:n_reps,
                                 param = paste0(rep(names_col, n_determ), ".", rep(names_determ, each = n_vars)))
  class(main_run) <- "qfbvar"
  return(main_run)

}

gibbs_sampler_minn_qf <- function(Y, prior_Pi_mean, prior_Pi_Omega, prior_S, lambda3, n_fcst = NULL, n_reps,
                                  init_Pi = NULL, init_Sigma = NULL, check_roots = TRUE, verbose = TRUE){

  # n_vars: number of variables
  # n_lags: number of lags
  # n_determ: number of deterministic variables
  # n_T: sample size (full sample)
  # n_T_: sample size (reduced sample)


  n_vars <- dim(Y)[2]
  n_lags <- nrow(prior_Pi_Omega)/n_vars
  n_T <- dim(Y)[1]# - n_lags
  n_T_ <- n_T - n_lags
  d <- matrix(1, nrow = nrow(Y), ncol = 1)
  prior_nu <- n_vars + 2
  prior_Pi_Omega <- diag(c(diag(prior_Pi_Omega), lambda3^2))
  prior_Pi_mean <- rbind(prior_Pi_mean, 0)

  ################################################################
  ### Preallocation
  # Pi and Sigma store their i-th draws in the third dimension, psi
  # is vectorized so it has its i-th draw stored in the i-th row
  # Pi:    p * pk * n_reps, each [,,i] stores Pi'
  # Sigma: p * p  * n_reps
  # psi:   n_reps * p
  # Z:     T * p * n_reps
  ### If forecasting (h is horizon):
  # Z_fcst: hk * p * n_reps
  # d_fcst_lags: hk * m
  ### If root checking:
  # roots: n_reps vector
  # num_tries: n_reps vector
  ### If smoothing of the state vector:
  # smoothed_Z: T * p * n_reps

  Pi    <- array(NA, dim = c(n_vars, n_vars * n_lags + 1, n_reps))
  Sigma <- array(NA, dim = c(n_vars, n_vars, n_reps))
  if (!is.null(n_fcst)) {
    Z_fcst<- array(NA, dim = c(n_fcst+n_lags, n_vars, n_reps),
                   dimnames = list(c((n_T-n_lags+1):n_T, paste0("fcst_", 1:n_fcst)), NULL, NULL))
  }
  if (check_roots == TRUE) {
    roots <- vector("numeric", n_reps)
    num_tries <- roots
  }


  ################################################################
  ### Gibbs sampling initialization

  # If the initial values are not provided, the missing values in
  # Z are filled with the next observed value and Pi, Sigma and
  # psi are then computed using maximum likelihood

  # This allows the user to run the Gibbs sampler for a burn-in
  # period, then use the final draw of that as initialization
  # for multiple chains


  ols_results <- mfbvar:::ols_initialization(z = Y, d = d, n_lags = n_lags, n_T = n_T, n_vars = n_vars, n_determ = 1)

  if (is.null(init_Pi)) {
    Pi[,, 1]    <- cbind(ols_results$Pi, ols_results$const)
  } else {
    if (all(dim(Pi[,, 1]) == dim(init_Pi))) {
      Pi[,, 1] <- init_Pi
    } else {
      stop(paste0("The dimension of init_Pi is ", paste(dim(init_Pi), collapse = " x "), ", but should be ", paste(dim(Pi[,, 1]), collapse = " x ")))
    }
  }

  # Compute the maximum eigenvalue of the initial Pi
  if (check_roots == TRUE) {
    Pi_comp    <- mfbvar:::build_companion(Pi = Pi[,-ncol(Pi[,,1]), 1], n_vars = n_vars, n_lags = n_lags)
    roots[1]   <- mfbvar:::max_eig_cpp(Pi_comp)
  }

  if (is.null(init_Sigma)) {
    Sigma[,, 1] <- ols_results$S
  } else {
    if (all(dim(Sigma[,,1]) == dim(init_Sigma))) {
      Sigma[,, 1] <- init_Sigma
    } else {
      stop(paste0("The dimension of init_Sigma is ", paste(dim(init_Sigma), collapse = " x "), ", but should be ", paste(dim(Sigma[,,1]), collapse = " x ")))
    }
  }

  ################################################################
  ### Compute terms which do not vary in the sampler

  inv_prior_Pi_Omega <- chol2inv(chol(prior_Pi_Omega))
  Omega_Pi <- inv_prior_Pi_Omega %*% prior_Pi_mean

  Z_comp <- mfbvar:::build_Z(z = Y, n_lags = n_lags)
  XX <- Z_comp[-nrow(Z_comp), ]
  XX <- cbind(XX, 1)
  YY <- Z_comp[-1, 1:n_vars]
  XXt.XX <- crossprod(XX)
  XXt.XX.inv <- chol2inv(chol(XXt.XX))
  Pi_sample <- XXt.XX.inv %*% crossprod(XX, YY)
  post_Pi_Omega <- chol2inv(chol(inv_prior_Pi_Omega + XXt.XX))
  post_Pi       <- post_Pi_Omega %*% (Omega_Pi + crossprod(XX, YY))
  S <- crossprod(YY - XX %*% Pi_sample)
  Pi_diff <- prior_Pi_mean - Pi_sample
  post_S <- prior_S + S + t(Pi_diff) %*% chol2inv(chol(prior_Pi_Omega + XXt.XX.inv)) %*% Pi_diff
  post_nu <- nrow(YY) + prior_nu

  for (r in 2:(n_reps)) {
    Sigma_r <- mfbvar:::rinvwish(v = post_nu, S = post_S)
    Sigma[,, r]   <- Sigma_r

    Pi[,, r] <- mfbvar:::rmatn(M = t(post_Pi), Q = post_Pi_Omega, P = Sigma_r)
    Pi_comp  <- mfbvar:::build_companion(Pi[,-(n_vars * n_lags + 1), r], n_vars = n_vars, n_lags = n_lags)

    Pi_r <- Pi[,,r]
    const_r <- Pi_r[, ncol(Pi_r)]
    Pi_r <- Pi_r[, -ncol(Pi_r)]


    ################################################################
    ### Forecasting step
    if (!is.null(n_fcst)) {

      # Forecast the process with mean subtracted
      Z_fcst[1:n_lags, , r] <- Y[(n_T - n_lags+1):n_T,]
      for (h in 1:n_fcst) {
        Z_fcst[n_lags + h, , r] <- const_r + Pi_r  %*% matrix(c(t(Z_fcst[(n_lags+h-1):h,, r])), ncol = 1) +
          mfbvar:::rmultn(m = matrix(0, nrow = n_vars), Sigma = Sigma[,,r])
      }

    }
  }

  lnpYY <- dmatt(YY, XX %*% prior_Pi_mean, chol2inv(chol(diag(nrow(YY)) + XX %*% prior_Pi_Omega %*% t(XX))), prior_S, prior_nu)

  ################################################################
  ### Prepare the return object
  return_obj <- list(Pi = Pi, Sigma = Sigma, psi = NULL, Z = Y, roots = NULL, num_tries = NULL,
                     Z_fcst = NULL, mdd = NULL, smoothed_Z = NULL, n_determ = 1,
                     n_lags = n_lags, n_vars = n_vars, n_fcst = n_fcst, prior_Pi_Omega = NULL, prior_Pi_mean = NULL,
                     prior_S = NULL, prior_nu = NULL, post_nu = NULL, d = d, Y = Y, n_T = n_T, n_T_ = n_T_,
                     prior_psi_Omega = NULL, prior_psi_mean = NULL, n_reps = n_reps,
                     lnpYY = lnpYY)

  if (check_roots == TRUE) {
    return_obj$roots <- roots
    return_obj$num_tries <- num_tries
  }
  if (!is.null(n_fcst)) {
    return_obj$Z_fcst <- Z_fcst
  }

  return(return_obj)

}
qfbvar_minn <- function(Y, prior_Pi_mean, prior_Pi_Omega, prior_S, lambda3, n_fcst, n_burnin, n_reps, verbose, ...) {

  stopifnot(is.null(n_fcst) || is.vector(n_fcst))
  stopifnot(is.data.frame(Y) || is.matrix(Y), all(apply(Y, 2, is.numeric)))
  stopifnot(is.vector(n_burnin), is.vector(n_reps))

  if (!is.null(n_fcst)) {
    names_fcst <- paste0("fcst_", 1:n_fcst)
  } else {
    names_fcst <- NULL
  }

  fun_call <- match.call()
  if (is.null(rownames(Y))) {
    names_row <- 1:nrow(Y)
  } else {
    names_row <- rownames(Y)
  }

  if (is.null(colnames(Y))) {
    names_col <- 1:col(Y)
  } else {
    names_col <- colnames(Y)
  }

  original_Y <- Y
  Y <- as.matrix(Y)
  n_vars <- ncol(Y)
  n_T <- nrow(Y)

  # For the smoothing
  if (verbose) {
    cat(paste0("############################################\n   Running the burn-in sampler with ", n_burnin, " draws\n\n"))
    start_burnin <- Sys.time()
  }
  burn_in <-  gibbs_sampler_minn_qf(Y, prior_Pi_mean, prior_Pi_Omega, prior_S, lambda3, n_fcst = NULL, n_burnin,
                                    check_roots = TRUE, verbose = verbose, ...)
  if (verbose) {
    end_burnin <- Sys.time()
    time_diff <- end_burnin - start_burnin
    cat(paste0("\n   Time elapsed for drawing ", n_burnin, " times for burn-in: ", signif(time_diff, digits = 1), " ",
               attr(time_diff, "units"), "\n"))
    cat(paste0("\n   Moving on to ",
               n_reps, " replications in the main chain\n", ifelse(!is.null(n_fcst), paste0("   Making forecasts ", n_fcst, " steps ahead"), " "), "\n\n"))
  }

  main_run <- gibbs_sampler_minn_qf(Y, prior_Pi_mean, prior_Pi_Omega, prior_S, lambda3, n_fcst, n_reps,
                                    init_Pi  = burn_in$Pi[,,dim(burn_in$Pi)[3]], init_Sigma = burn_in$Sigma[,,dim(burn_in$Sigma)[3]],
                                    check_roots = TRUE, verbose)
  main_run$call <- fun_call
  if (verbose) {
    time_diff <- Sys.time() - start_burnin
    cat(paste0("\n   Total time elapsed: ", signif(time_diff, digits = 1), " ",
               attr(time_diff, "units"), "\n"))
  }

  if (!is.null(n_fcst)) {
    rownames(main_run$Z_fcst)[1:main_run$n_lags] <- names_row[(main_run$n_T-main_run$n_lags+1):main_run$n_T]
    rownames(main_run$Z_fcst)[(main_run$n_lags+1):(main_run$n_fcst+main_run$n_lags)] <- names_fcst
    colnames(main_run$Z_fcst) <- names_col

  }


  main_run$names_row <- names_row
  main_run$names_col <- names_col
  main_run$names_fcst <- names_fcst
  main_run$names_determ <- "const"
  main_run$n_burnin <- n_burnin

  dimnames(main_run$Z) <- list(time = names_row,
                               variable = names_col)
  dimnames(main_run$Pi) <- list(dep = names_col,
                                indep = c(paste0(rep(names_col, n_lags), ".l", rep(1:n_lags, each = n_vars)), "const"),
                                iteration = 1:n_reps)
  dimnames(main_run$Sigma) <- list(names_col,
                                   names_col,
                                   iteration = 1:n_reps)
  class(main_run) <- "qfbvar"
  return(main_run)

}

mdd1_qf <- function(mfbvar_obj) {
  ################################################################
  ### Get things from the MFBVAR object
  n_determ <- mfbvar_obj$n_determ
  n_vars <- mfbvar_obj$n_vars
  n_lags <- mfbvar_obj$n_lags
  n_T <- mfbvar_obj$n_T
  n_T_ <- mfbvar_obj$n_T_
  n_reps <- mfbvar_obj$n_reps

  psi <- mfbvar_obj$psi
  prior_Pi_Omega <- mfbvar_obj$prior_Pi_Omega
  prior_Pi_mean <- mfbvar_obj$prior_Pi_mean
  prior_S <- mfbvar_obj$prior_S
  post_nu <- mfbvar_obj$post_nu

  Y     <- mfbvar_obj$Y
  d     <- mfbvar_obj$d
  Pi    <- mfbvar_obj$Pi
  Sigma <- mfbvar_obj$Sigma

  Lambda <- mfbvar_obj$Lambda

  post_Pi_mean <- apply(Pi, c(1, 2), mean)
  post_Sigma <- apply(Sigma, c(1, 2), mean)
  post_psi <- colMeans(psi)

  prior_S <- mfbvar_obj$prior_S
  prior_nu <- mfbvar_obj$prior_nu
  prior_Pi_Omega <- mfbvar_obj$prior_Pi_Omega
  prior_Pi_mean <- mfbvar_obj$prior_Pi_mean
  prior_psi_Omega <- mfbvar_obj$prior_psi_Omega
  prior_psi_mean <- mfbvar_obj$prior_psi_mean

  #(mZ,lH,mF,mQ,iT,ip,iq,h0,P0)
  Pi_comp <- mfbvar:::build_companion(post_Pi_mean, n_vars = n_vars, n_lags = n_lags)
  Q_comp  <- matrix(0, ncol = n_vars*n_lags, nrow = n_vars*n_lags)
  Q_comp[1:n_vars, 1:n_vars] <- t(chol(post_Sigma))
  P0      <- matrix(0, n_lags*n_vars, n_lags*n_vars)

  ################################################################
  ### Initialize
  Pi_red    <- array(NA, dim = c(n_vars, n_vars * n_lags, n_reps))
  Sigma_red <- array(NA, dim = c(n_vars, n_vars, n_reps))
  Pi_red[,, 1]    <- post_Pi_mean
  Sigma_red[,, 1] <- post_Sigma

  roots <- vector("numeric", n_reps)
  num_tries <- roots

  ################################################################
  ### Compute terms which do not vary in the sampler

  # Create D (does not vary in the sampler), and find roots of Pi
  D <- mfbvar:::build_DD(d = d, n_lags = n_lags)

  # For the posterior of Pi
  inv_prior_Pi_Omega <- solve(prior_Pi_Omega)
  Omega_Pi <- inv_prior_Pi_Omega %*% prior_Pi_mean

  ################################################################
  ### For the likelihood calculation
  mZ <- Y - d %*% t(matrix(post_psi, nrow = n_vars))
  mZ <- mZ[-(1:n_lags), ]
  demeaned_z0 <- Y[1:n_lags,] - d[1:n_lags, ] %*% t(matrix(post_psi, nrow = n_vars))
  h0 <- matrix(t(demeaned_z0), ncol = 1)
  h0 <- h0[(n_vars*n_lags):1,, drop = FALSE] # have to reverse the order

  ################################################################
  ### Final calculations
  # Eq (17) in Fuentes-Albero and Melosi

  ####
  # Likelihood
  Lambda <- matrix(0, nrow = n_vars, ncol = n_vars*n_lags)
  Lambda[1:n_vars, 1:n_vars] <- diag(n_vars)
  lklhd          <- sum(c(mfbvar:::loglike(Y = as.matrix(mZ), Lambda = Lambda, Pi_comp = Pi_comp, Q_comp = Q_comp, n_T = n_T_, n_vars = n_vars, n_comp = n_lags * n_vars, z0 = h0, P0 = P0)[-1]))

  ####
  # p(Pi, Sigma)
  log_dnorminvwish <- function(X, Sigma, M, P, S, v) {
    q <- dim(Sigma)[1]
    p <- dim(P)[1]
    det_Sigma <- det(Sigma)
    inv_Sigma <- chol2inv(chol(Sigma))
    dmultnorm <- (-p*q/2) * log(2 * pi) + (-p/2) * log(det_Sigma) + (-q/2)*log(det(P)) + (-1/2 * sum(diag(inv_Sigma %*% t(X - M) %*% chol2inv(chol(P)) %*% (X - M))))
    cc <- (v * q/2)*log(2) + (q*(q-1)/4)*log(pi) + sum(lgamma((v+1-1:q)/2))
    dinvwish <- -cc + (v/2) * log(det(S)) -(v+q+1)/2*log(det_Sigma) -1/2 * sum(diag(inv_Sigma %*% S))
    return(dmultnorm + dinvwish)
  }
  eval_prior_Pi_Sigma <- log_dnorminvwish(X = t(post_Pi_mean), Sigma = post_Sigma, M = prior_Pi_mean, P = prior_Pi_Omega, S = prior_S, v = prior_nu)

  ####
  # p(psi)
  log_dmultn <- function(x, m, Sigma) {
    log_d <- (-1/2)* log(det(2*pi*Sigma)) -1/2 * t(x-m) %*% chol2inv(chol(Sigma)) %*% (x-m)
    return(log_d)
  }
  eval_prior_psi      <- log_dmultn(x = post_psi, m = prior_psi_mean, Sigma = prior_psi_Omega)

  ####
  # p(Pi, Sigma|psi, Y)
  demeaned_z <- Y - d %*% post_psi
  demeaned_Z <- mfbvar:::build_Z(z = demeaned_z, n_lags = n_lags)
  XX <- demeaned_Z[-nrow(demeaned_Z), ]
  YY <- demeaned_Z[-1, 1:n_vars]
  XXt.XX <- crossprod(XX)
  XXt.XX.inv <- chol2inv(chol(XXt.XX))
  Pi_sample <- XXt.XX.inv %*% crossprod(XX, YY)

  # Posterior moments of Pi
  post_Pi_Omega_i <- chol2inv(chol(inv_prior_Pi_Omega + XXt.XX))
  post_Pi_i       <- post_Pi_Omega_i %*% (Omega_Pi + crossprod(XX, YY))

  # Then Sigma
  s_sample  <- crossprod(YY - XX %*% Pi_sample)
  Pi_diff <- prior_Pi_mean - Pi_sample
  post_s_i <- prior_S + s_sample + t(Pi_diff) %*% chol2inv(chol(prior_Pi_Omega + XXt.XX.inv)) %*% Pi_diff

  # Evaluate
  eval_RB_Pi_Sigma <- log_dnorminvwish(X = t(post_Pi_mean), Sigma = post_Sigma, M = post_Pi_i, P = post_Pi_Omega_i, S = post_s_i, v = post_nu)

  ####
  # p(psi|Y)
  Z_array <- array(Y, dim = c(dim(Y)[1], dim(Y)[2], dim(Pi)[3]))
  eval_marg_psi   <- log(mean(mfbvar:::eval_psi_MargPost(Pi_array = Pi, Sigma_array = Sigma, Z_array = Z_array, post_psi_center = post_psi, prior_psi_mean = prior_psi_mean,
                                                prior_psi_Omega = prior_psi_Omega, D_mat = D, n_determ = n_determ, n_vars = n_vars, n_lags = n_lags, n_reps = n_reps)))

  mdd_estimate <- lklhd + eval_prior_Pi_Sigma + eval_prior_psi - (eval_RB_Pi_Sigma + eval_marg_psi)

  return(list(lklhd = lklhd, eval_prior_Pi_Sigma = eval_prior_Pi_Sigma, eval_prior_psi = eval_prior_psi, eval_RB_Pi_Sigma = eval_RB_Pi_Sigma, eval_marg_psi = eval_marg_psi, log_mdd = mdd_estimate))
}
