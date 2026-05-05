validate_protomix_params <- function(data, params) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for method `protomix`.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`protomix` requires row-by-feature data.", call. = FALSE)
  }
}

fit_protomix <- function(data, params) {
  k <- params$k
  k_min <- if (is.null(params$k_min)) 2L else as.integer(params$k_min)
  k_max <- if (is.null(params$k_max)) k else as.integer(params$k_max)
  if (is.null(k_max)) {
    k_max <- max(k_min, min(6L, floor(sqrt(nrow(data)) + 1L)))
  }
  nstart <- if (is.null(params$nstart)) 3L else as.integer(params$nstart)
  max_iter <- if (is.null(params$max_iter)) 100L else as.integer(params$max_iter)
  tol <- if (is.null(params$tol)) 1e-8 else as.numeric(params$tol)
  seed <- if (is.null(params$seed)) 42L else as.integer(params$seed)
  fit <- protomix_auto_cluster(
    data,
    verbose = isTRUE(params$verbose),
    k_min = k_min,
    k_max = k_max,
    nstart = nstart,
    max_iter = max_iter,
    tol = tol,
    kappa0 = if (is.null(params$kappa0)) 0.5 else as.numeric(params$kappa0),
    alpha_cat = if (is.null(params$alpha_cat)) 1 else as.numeric(params$alpha_cat),
    q_lambda = if (is.null(params$q_lambda)) 0.90 else as.numeric(params$q_lambda),
    beta_lambda = if (is.null(params$beta_lambda)) 1.6 else as.numeric(params$beta_lambda),
    tuner_steps = if (is.null(params$tuner_steps)) 6L else as.integer(params$tuner_steps),
    seed = seed
  )
  prototypes <- protomix_prototypes(fit, names(data))
  list(
    clusters = fit$cluster,
    n_clusters = fit$k,
    membership = NULL,
    centers = fit$centers_num,
    prototypes = prototypes,
    distance_info = list(metric = "protomix", gamma = fit$gamma, lambda = fit$lambda),
    fitted_object = fit,
    extras = list(
      objective = fit$objective,
      objective_trace = fit$objective_trace,
      iterations = fit$iterations
    )
  )
}

predict_protomix <- function(object, new_data, ...) {
  cls <- predict_protomix_fit(object$fitted_object, new_data)
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = NULL,
    method = object$method,
    prediction_type = "native"
  )
}

protomix_auto_cluster <- function(
  X,
  verbose = FALSE,
  k_min = 2,
  k_max = NULL,
  nstart = 3,
  max_iter = 100,
  tol = 1e-8,
  kappa0 = 0.5,
  alpha_cat = 1,
  q_lambda = 0.90,
  beta_lambda = 1.6,
  tuner_steps = 6,
  seed = 42
) {
  set.seed(seed)
  X <- protomix_as_mixed_df(X)
  gamma <- protomix_suggest_gamma_balanced(X)
  k0 <- suppressWarnings(protomix_pick_k_fast_mixed(X, gamma = gamma, k_min = k_min, k_max = k_max))
  lambda0 <- protomix_lambda_farthest_first_mixed(
    X,
    k_target = k0,
    gamma = gamma,
    q = q_lambda,
    beta = beta_lambda,
    seed = seed
  )
  tuned <- protomix_tune_lambda_ministeps(
    X,
    gamma = gamma,
    lam_init = lambda0,
    k_target = k0,
    max_steps = tuner_steps,
    nstart = nstart,
    kappa0 = kappa0,
    alpha_cat = alpha_cat,
    tol = tol,
    max_iter = max_iter
  )
  fit <- tuned$fit
  fit$gamma <- gamma
  fit$lambda <- tuned$lambda
  if (verbose) {
    message(sprintf("protomix: k=%d, lambda=%.4f, gamma=%.4f", fit$k, fit$lambda, fit$gamma))
  }
  class(fit) <- "protomix_fit"
  fit
}

protomix_dp_means_kproto_bayes <- function(
  X,
  lambda,
  gamma = 1,
  max_iter = 100L,
  tol = 1e-8,
  order = NULL,
  standardize_num = TRUE,
  nstart = 1L,
  mu0 = NULL,
  kappa0 = 0,
  alpha_cat = 1
) {
  X <- protomix_as_mixed_df(X)
  n <- nrow(X)
  num_idx <- which(vapply(X, is.numeric, logical(1)))
  cat_idx <- which(!vapply(X, is.numeric, logical(1)))
  d_num <- length(num_idx)
  d_cat <- length(cat_idx)
  Xn_raw <- if (d_num) as.matrix(X[, num_idx, drop = FALSE]) else NULL
  Xc_fac <- if (d_cat) lapply(X[, cat_idx, drop = FALSE], factor) else NULL
  Xc_char <- if (d_cat) do.call(cbind, lapply(Xc_fac, as.character)) else NULL
  lvls_list <- if (d_cat) lapply(Xc_fac, levels) else NULL
  num_center <- if (d_num) colMeans(Xn_raw, na.rm = TRUE) else NULL
  num_scale <- if (d_num) apply(Xn_raw, 2, stats::sd, na.rm = TRUE) else NULL
  Xn <- Xn_raw
  if (d_num && standardize_num) {
    num_scale[num_scale == 0] <- 1
    Xn <- scale(Xn_raw, center = num_center, scale = num_scale)
  }
  if (is.null(mu0) && d_num) {
    mu0 <- rep(0, d_num)
  }

  dist2_to_all <- function(i, mu_num, mu_cat) {
    n_proto <- if (!is.null(mu_num)) nrow(mu_num) else nrow(mu_cat)
    out <- numeric(n_proto)
    for (cluster_id in seq_len(n_proto)) {
      dn <- if (d_num) protomix_l2_sq_na(Xn[i, ], mu_num[cluster_id, ]) else 0
      dc <- if (d_cat) gamma * protomix_mismatch_count(Xc_char[i, ], mu_cat[cluster_id, ]) / max(1, d_cat) else 0
      out[cluster_id] <- dn + dc
    }
    out
  }
  compute_obj <- function(assign, mu_num, mu_cat) {
    s <- 0
    for (i in seq_len(n)) {
      s <- s + min(dist2_to_all(i, mu_num, mu_cat))
    }
    n_proto <- if (!is.null(mu_num)) nrow(mu_num) else nrow(mu_cat)
    s + lambda * n_proto
  }
  run_once <- function(idx) {
    mu_num <- if (d_num) matrix(protomix_shrunk_mean(Xn, mu0, kappa0), nrow = 1) else NULL
    mu_cat <- if (d_cat) matrix(protomix_dirichlet_mode(Xc_char, lvls_list, alpha_cat), nrow = 1) else NULL
    assign <- rep(1L, n)
    last_obj <- Inf
    obj_hist <- numeric(0)
    for (it in seq_len(max_iter)) {
      for (i in idx) {
        d2 <- dist2_to_all(i, mu_num, mu_cat)
        d2[!is.finite(d2)] <- Inf
        m <- min(d2, na.rm = TRUE)
        if (!is.finite(m)) {
          m <- lambda + 1
        }
        if (m > lambda) {
          if (d_num) mu_num <- rbind(mu_num, Xn[i, , drop = FALSE])
          if (d_cat) mu_cat <- rbind(mu_cat, Xc_char[i, ])
          assign[i] <- if (d_num) nrow(mu_num) else nrow(mu_cat)
        } else {
          assign[i] <- which.min(d2)
        }
      }
      n_proto <- if (d_num) nrow(mu_num) else nrow(mu_cat)
      counts <- tabulate(assign, nbins = n_proto)
      if (any(counts == 0L)) {
        keep <- which(counts > 0L)
        remap <- integer(n_proto)
        remap[keep] <- seq_along(keep)
        assign <- remap[assign]
        if (d_num) mu_num <- mu_num[keep, , drop = FALSE]
        if (d_cat) mu_cat <- mu_cat[keep, , drop = FALSE]
        n_proto <- length(keep)
      }
      if (d_num) {
        mu_num_values <- vapply(seq_len(n_proto), function(cluster_id) {
          rows <- which(assign == cluster_id)
          protomix_shrunk_mean(Xn[rows, , drop = FALSE], mu0, kappa0)
        }, numeric(d_num))
        mu_num <- matrix(mu_num_values, nrow = n_proto, ncol = d_num, byrow = TRUE)
      }
      if (d_cat) {
        mu_cat_values <- vapply(seq_len(n_proto), function(cluster_id) {
          rows <- which(assign == cluster_id)
          protomix_dirichlet_mode(Xc_char[rows, , drop = FALSE], lvls_list, alpha_cat)
        }, character(d_cat))
        mu_cat <- matrix(mu_cat_values, nrow = n_proto, ncol = d_cat, byrow = TRUE)
      }
      cur_obj <- compute_obj(assign, mu_num, mu_cat)
      if (!is.finite(cur_obj)) {
        cur_obj <- .Machine$double.xmax
      }
      obj_hist <- c(obj_hist, cur_obj)
      if (abs(cur_obj - last_obj) < tol) break
      last_obj <- cur_obj
    }
    list(
      assign = assign,
      mu_num = mu_num,
      mu_cat = mu_cat,
      k = if (d_num) nrow(mu_num) else nrow(mu_cat),
      iterations = it,
      objective = compute_obj(assign, mu_num, mu_cat),
      objective_trace = obj_hist
    )
  }
  best <- NULL
  for (s in seq_len(nstart)) {
    idx <- if (!is.null(order)) as.integer(order) else sample.int(n)
    res <- run_once(idx)
    if (is.null(best) || res$objective < best$objective) {
      best <- res
    }
  }
  structure(list(
    centers_num = best$mu_num,
    centers_cat = best$mu_cat,
    cluster = best$assign,
    k = best$k,
    lambda = lambda,
    gamma = gamma,
    iterations = best$iterations,
    objective = best$objective,
    objective_trace = if (!is.null(best$objective_trace)) best$objective_trace else as.numeric(best$objective),
    standardize_num = standardize_num,
    num_center = num_center,
    num_scale = num_scale,
    num_cols = num_idx,
    cat_cols = cat_idx,
    levels_cat = lvls_list
  ), class = "protomix_fit")
}

predict_protomix_fit <- function(object, newdata) {
  X <- protomix_as_mixed_df(newdata)
  num_idx <- object$num_cols
  cat_idx <- object$cat_cols
  d_num <- length(num_idx)
  d_cat <- length(cat_idx)
  Xn <- if (d_num) as.matrix(X[, num_idx, drop = FALSE]) else NULL
  if (d_num && object$standardize_num) {
    sc <- object$num_scale
    sc[sc == 0] <- 1
    Xn <- scale(Xn, center = object$num_center, scale = sc)
  }
  Xc <- if (d_cat) {
    out <- vector("list", d_cat)
    for (j in seq_len(d_cat)) {
      out[[j]] <- factor(X[[cat_idx[j]]], levels = object$levels_cat[[j]])
    }
    out
  } else {
    NULL
  }
  Xc_char <- if (d_cat) do.call(cbind, lapply(Xc, as.character)) else NULL
  out <- integer(nrow(X))
  for (i in seq_len(nrow(X))) {
    best <- Inf
    best_cluster <- 1L
    for (cluster_id in seq_len(object$k)) {
      dn <- if (d_num) protomix_l2_sq_na(Xn[i, ], object$centers_num[cluster_id, ]) else 0
      dc <- if (d_cat) object$gamma * protomix_mismatch_count(Xc_char[i, ], object$centers_cat[cluster_id, ]) / max(1, d_cat) else 0
      d2 <- dn + dc
      if (d2 < best) {
        best <- d2
        best_cluster <- cluster_id
      }
    }
    out[i] <- best_cluster
  }
  out
}

protomix_l2_sq_na <- function(x, y) {
  ok <- !(is.na(x) | is.na(y))
  if (!any(ok)) 0 else sum((x[ok] - y[ok])^2)
}

protomix_mismatch_count <- function(x_char, proto_char) {
  ok <- !(is.na(x_char) | is.na(proto_char))
  if (!any(ok)) 0L else sum(x_char[ok] != proto_char[ok])
}

protomix_shrunk_mean <- function(Xc, mu0, kappa0) {
  if (is.null(Xc) || ncol(Xc) == 0) return(numeric(0))
  d <- ncol(Xc)
  out <- numeric(d)
  for (j in seq_len(d)) {
    col <- Xc[, j]
    col <- col[!is.na(col)]
    nobs <- length(col)
    out[j] <- if (nobs == 0) mu0[j] else (kappa0 * mu0[j] + nobs * mean(col)) / (kappa0 + nobs)
  }
  out
}

protomix_dirichlet_mode <- function(cols_char, levels_list, alpha) {
  if (is.null(cols_char) || ncol(cols_char) == 0) return(character(0))
  d <- ncol(cols_char)
  res <- character(d)
  if (length(alpha) == 1L) alpha <- rep(alpha, d)
  for (j in seq_len(d)) {
    lv <- levels_list[[j]]
    fac <- factor(cols_char[, j], levels = lv)
    tab <- table(fac, useNA = "no")
    tab <- as.numeric(tab) + alpha[j] / length(lv)
    res[j] <- lv[which.max(tab)]
  }
  res
}

protomix_as_mixed_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (j in seq_along(df)) {
    if (!is.numeric(df[[j]]) && !is.factor(df[[j]])) {
      df[[j]] <- factor(df[[j]])
    }
  }
  df
}

protomix_suggest_gamma_balanced <- function(X) {
  X <- protomix_as_mixed_df(X)
  num_idx <- which(vapply(X, is.numeric, logical(1)))
  cat_idx <- setdiff(seq_along(X), num_idx)
  if (!length(cat_idx)) return(0)
  if (!length(num_idx)) return(1)
  Xn <- scale(as.matrix(X[, num_idx, drop = FALSE]))
  d2 <- as.numeric(stats::dist(Xn)^2)
  stats::median(d2, na.rm = TRUE) * (length(cat_idx) / length(num_idx))
}

protomix_build_mixed_dist <- function(X, gamma) {
  X <- as.data.frame(X, stringsAsFactors = FALSE)
  num_idx <- which(vapply(X, is.numeric, logical(1)))
  cat_idx <- setdiff(seq_along(X), num_idx)
  Xn <- if (length(num_idx)) scale(as.matrix(X[, num_idx, drop = FALSE])) else NULL
  Xc <- if (length(cat_idx)) lapply(X[, cat_idx, drop = FALSE], factor) else NULL
  Xc_char <- if (length(cat_idx)) do.call(cbind, lapply(Xc, as.character)) else NULL
  n <- nrow(X)
  D <- matrix(0, n, n)
  denom <- if (is.null(Xc_char)) 1 else ncol(Xc_char)
  for (i in seq_len(n)) {
    dn <- if (!is.null(Xn)) {
      diff_mat <- sweep(Xn, 2, Xn[i, ], FUN = "-")
      rowSums(diff_mat^2)
    } else {
      rep(0, n)
    }
    dc <- if (!is.null(Xc_char)) {
      rowSums(Xc_char != matrix(Xc_char[i, ], n, ncol(Xc_char), byrow = TRUE), na.rm = TRUE)
    } else {
      0
    }
    D[i, ] <- dn + gamma * dc / denom
  }
  stats::as.dist((D + t(D)) / 2)
}

protomix_pick_k_fast_mixed <- function(X, gamma, k_min = 2, k_max = NULL) {
  X <- protomix_as_mixed_df(X)
  n <- nrow(X)
  if (is.null(k_max)) k_max <- max(k_min, min(4, floor(sqrt(n) + 1)))
  if (n < 4 || k_min >= k_max) return(max(2, min(3, n - 1)))
  D <- protomix_build_mixed_dist(X, gamma)
  best_k <- k_min
  best_s <- -Inf
  for (k in k_min:k_max) {
    lab <- tryCatch(cluster::pam(D, k = k, diss = TRUE, cluster.only = TRUE), error = function(e) NULL)
    if (is.null(lab)) next
    sil <- tryCatch(mean(cluster::silhouette(lab, D)[, "sil_width"]), error = function(e) NA_real_)
    if (is.finite(sil) && sil > best_s) {
      best_s <- sil
      best_k <- k
    }
  }
  best_k
}

protomix_lambda_farthest_first_mixed <- function(
  X,
  k_target,
  gamma = 1,
  seed = NULL,
  standardize_num = TRUE,
  mu0 = NULL,
  kappa0 = 0,
  alpha_cat = 1,
  beta = 1.6,
  q = 0.90,
  use_quantile = TRUE
) {
  if (!is.null(seed)) set.seed(seed)
  X <- protomix_as_mixed_df(X)
  num_idx <- which(vapply(X, is.numeric, logical(1)))
  cat_idx <- which(!vapply(X, is.numeric, logical(1)))
  Xn <- if (length(num_idx)) as.matrix(X[, num_idx, drop = FALSE]) else NULL
  Xc <- if (length(cat_idx)) lapply(X[, cat_idx, drop = FALSE], factor) else NULL
  d_num <- if (is.null(Xn)) 0L else ncol(Xn)
  d_cat <- if (is.null(Xc)) 0L else length(Xc)
  if (d_num) {
    num_center <- colMeans(Xn, na.rm = TRUE)
    num_scale <- apply(Xn, 2, stats::sd, na.rm = TRUE)
    num_scale[num_scale == 0] <- 1
    if (standardize_num) Xn <- scale(Xn, center = num_center, scale = num_scale)
  }
  Xc_char <- if (d_cat) do.call(cbind, lapply(Xc, as.character)) else NULL
  lvls_list <- if (d_cat) lapply(Xc, levels) else NULL
  mu0 <- if (is.null(mu0) && d_num) rep(0, d_num) else mu0
  Cn <- if (d_num) matrix(protomix_shrunk_mean(Xn, mu0, kappa0), 1) else NULL
  Cc <- if (d_cat) matrix(protomix_dirichlet_mode(Xc_char, lvls_list, alpha_cat), 1) else NULL
  dist2_to_T <- function(i, Cn, Cc) {
    if (is.null(Cn) && is.null(Cc)) return(Inf)
    n_proto <- if (!is.null(Cn)) nrow(Cn) else nrow(Cc)
    best <- Inf
    for (cluster_id in seq_len(n_proto)) {
      dn <- if (d_num) sum((Xn[i, ] - Cn[cluster_id, ])^2) else 0
      dc <- if (d_cat) gamma * sum(Xc_char[i, ] != Cc[cluster_id, ], na.rm = TRUE) / max(1, d_cat) else 0
      best <- min(best, dn + dc)
    }
    best
  }
  n_add <- max(0L, k_target - 1L)
  for (step in seq_len(n_add)) {
    d2 <- vapply(seq_len(nrow(X)), dist2_to_T, numeric(1), Cn, Cc)
    j <- which.max(d2)
    if (d_num) Cn <- rbind(Cn, Xn[j, , drop = FALSE])
    if (d_cat) Cc <- rbind(Cc, Xc_char[j, ])
  }
  dmin <- vapply(seq_len(nrow(X)), dist2_to_T, numeric(1), Cn, Cc)
  lambda <- if (use_quantile) {
    stats::quantile(dmin, probs = q, names = FALSE, na.rm = TRUE)
  } else {
    stats::median(dmin, na.rm = TRUE)
  }
  if (!is.finite(lambda) || lambda <= 0) {
    lambda <- 1
  }
  as.numeric(beta * lambda)
}

protomix_tune_lambda_ministeps <- function(
  X,
  gamma,
  lam_init,
  k_target,
  scale_up = 1.25,
  scale_down = 0.85,
  max_steps = 6,
  nstart = 3,
  kappa0 = 0.5,
  alpha_cat = 1,
  tol = 1e-8,
  max_iter = 100
) {
  lambda <- lam_init
  best <- NULL
  for (s in seq_len(max_steps)) {
    fit <- protomix_dp_means_kproto_bayes(
      X,
      lambda = lambda,
      gamma = gamma,
      nstart = nstart,
      max_iter = max_iter,
      tol = tol,
      kappa0 = kappa0,
      alpha_cat = alpha_cat
    )
    k <- fit$k
    if (is.null(best) || abs(k - k_target) < abs(best$k - k_target)) {
      best <- list(fit = fit, lambda = lambda, k = k)
    }
    if (k == k_target || k %in% c(k_target - 1L, k_target + 1L)) break
    lambda <- if (k > k_target) lambda * scale_up else lambda * scale_down
  }
  best
}

protomix_prototypes <- function(fit, feature_names) {
  out <- data.frame(cluster = seq_len(fit$k))
  if (length(fit$num_cols)) {
    num <- as.data.frame(fit$centers_num)
    names(num) <- feature_names[fit$num_cols]
    out <- cbind(out, num)
  }
  if (length(fit$cat_cols)) {
    cat <- as.data.frame(fit$centers_cat, stringsAsFactors = FALSE)
    names(cat) <- feature_names[fit$cat_cols]
    out <- cbind(out, cat)
  }
  rownames(out) <- paste0("cluster_", seq_len(fit$k))
  out
}
