.kmm_check_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("`data` must contain at least one row and one column.", call. = FALSE)
  }
  data
}

.kmm_split_vars <- function(data) {
  num_vars <- names(data)[vapply(data, is.numeric, logical(1))]
  cat_vars <- setdiff(names(data), num_vars)
  list(num = num_vars, cat = cat_vars)
}

.kmm_prepare <- function(data, scale_numeric = TRUE, num_center = NULL,
                         num_scale = NULL, factor_levels = NULL) {
  data <- .kmm_check_data(data)
  vars <- .kmm_split_vars(data)
  for (v in vars$cat) {
    data[[v]] <- as.factor(data[[v]])
    if (!is.null(factor_levels) && !is.null(factor_levels[[v]])) {
      data[[v]] <- factor(as.character(data[[v]]), levels = factor_levels[[v]])
    }
  }
  x_num <- data[vars$num]
  x_cat <- data[vars$cat]
  if (length(vars$num) > 0L && scale_numeric) {
    if (is.null(num_center)) num_center <- vapply(x_num, mean, numeric(1), na.rm = TRUE)
    if (is.null(num_scale)) {
      num_scale <- vapply(x_num, stats::sd, numeric(1), na.rm = TRUE)
      num_scale[is.na(num_scale) | num_scale == 0] <- 1
    }
    x_num <- as.data.frame(scale(x_num, center = num_center, scale = num_scale))
  }
  list(x_num = x_num, x_cat = x_cat, num_vars = vars$num, cat_vars = vars$cat,
       num_center = num_center, num_scale = num_scale)
}

.kmm_mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_character_)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

.kmm_center_value <- function(x, center_num) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  switch(center_num, mean = mean(x), median = stats::median(x), trimmed = mean(x, trim = 0.1))
}

.kmm_n <- function(x_num, x_cat) {
  if (ncol(x_num) > 0L) nrow(x_num) else nrow(x_cat)
}

.kmm_data_matrices <- function(x_num, x_cat, n = .kmm_n(x_num, x_cat)) {
  x_num_m <- if (ncol(x_num) > 0L) as.matrix(x_num) else matrix(numeric(0), nrow = n, ncol = 0L)
  x_cat_m <- if (ncol(x_cat) > 0L) {
    matrix(as.character(as.matrix(x_cat)), nrow = nrow(x_cat), ncol = ncol(x_cat),
           dimnames = dimnames(as.matrix(x_cat)))
  } else {
    matrix(character(0), nrow = n, ncol = 0L)
  }
  list(x_num = x_num_m, x_cat = x_cat_m)
}

.kmm_proto_matrices <- function(proto, k) {
  proto_num <- if (!is.null(proto$num) && ncol(proto$num) > 0L) {
    as.matrix(proto$num)
  } else {
    matrix(numeric(0), nrow = k, ncol = 0L)
  }
  proto_cat <- if (!is.null(proto$cat) && ncol(proto$cat) > 0L) {
    matrix(as.character(proto$cat), nrow = nrow(proto$cat), ncol = ncol(proto$cat),
           dimnames = dimnames(proto$cat))
  } else {
    matrix(character(0), nrow = k, ncol = 0L)
  }
  list(num = proto_num, cat = proto_cat)
}

update_prototypes <- function(x_num, x_cat, cluster, k, center_num = "mean") {
  proto_num <- NULL
  proto_cat <- NULL
  if (ncol(x_num) > 0L) {
    proto_num <- do.call(rbind, lapply(seq_len(k), function(g) {
      rows <- cluster == g
      if (!any(rows)) return(rep(NA_real_, ncol(x_num)))
      vapply(x_num[rows, , drop = FALSE], .kmm_center_value, numeric(1), center_num = center_num)
    }))
    colnames(proto_num) <- colnames(x_num)
    rownames(proto_num) <- paste0("cluster_", seq_len(k))
  }
  if (ncol(x_cat) > 0L) {
    proto_cat <- do.call(rbind, lapply(seq_len(k), function(g) {
      rows <- cluster == g
      if (!any(rows)) return(rep(NA_character_, ncol(x_cat)))
      vapply(x_cat[rows, , drop = FALSE], .kmm_mode_value, character(1))
    }))
    colnames(proto_cat) <- colnames(x_cat)
    rownames(proto_cat) <- paste0("cluster_", seq_len(k))
  }
  list(num = proto_num, cat = proto_cat)
}

.kmm_within_components <- function(x_num, x_cat, cluster, prototypes, lambda) {
  num <- 0
  cat <- 0
  for (g in seq_len(length(unique(cluster)))) {
    idx <- which(cluster == g)
    if (length(idx) == 0L) next
    if (ncol(x_num) > 0L && !is.null(prototypes$num)) {
      num <- num + sum(rowSums((as.matrix(x_num[idx, , drop = FALSE]) -
        matrix(prototypes$num[g, ], length(idx), ncol(x_num), byrow = TRUE))^2))
    }
    if (ncol(x_cat) > 0L && !is.null(prototypes$cat)) {
      for (j in seq_len(ncol(x_cat))) {
        cat <- cat + sum(as.character(x_cat[idx, j]) != as.character(prototypes$cat[g, j]))
      }
    }
  }
  list(numeric = num, categorical = lambda * cat, total = num + lambda * cat)
}

.kmm_distance_matrix <- function(x_num, x_cat, proto, lambda, missing = "pairwise") {
  n <- .kmm_n(x_num, x_cat)
  mats <- .kmm_data_matrices(x_num, x_cat, n = n)
  .kmm_distance_matrix_mats(mats$x_num, mats$x_cat, proto, lambda, missing)
}

.kmm_distance_matrix_mats <- function(x_num_m, x_cat_m, proto, lambda, missing = "pairwise") {
  k <- if (!is.null(proto$num)) nrow(proto$num) else nrow(proto$cat)
  proto_mats <- .kmm_proto_matrices(proto, k = k)
  kmm_distance_matrix_cpp(x_num_m, x_cat_m, proto_mats$num, proto_mats$cat, lambda, missing)
}

.kmm_prototypes_original_scale <- function(prototypes, num_center, num_scale, scale_numeric) {
  proto <- prototypes
  if (scale_numeric && !is.null(proto$num) && !is.null(num_center) && !is.null(num_scale)) {
    proto$num <- sweep(proto$num, 2L, num_scale, `*`)
    proto$num <- sweep(proto$num, 2L, num_center, `+`)
  }
  proto
}

.kmm_combined_prototypes <- function(prototypes) {
  out <- NULL
  if (!is.null(prototypes$num)) out <- as.data.frame(prototypes$num)
  if (!is.null(prototypes$cat)) {
    cat_df <- as.data.frame(prototypes$cat, stringsAsFactors = FALSE)
    out <- if (is.null(out)) cat_df else cbind(out, cat_df)
  }
  out
}

estimate_lambda <- function(x_num, x_cat) {
  num_part <- 1
  if (ncol(x_num) > 0L) {
    num_part <- mean(vapply(x_num, stats::var, numeric(1), na.rm = TRUE), na.rm = TRUE)
  }
  if (ncol(x_cat) == 0L) return(0)
  gini <- function(z) {
    p <- prop.table(table(z, useNA = "no"))
    if (length(p) == 0L) return(NA_real_)
    1 - sum(p^2)
  }
  cat_part <- mean(vapply(x_cat, gini, numeric(1)), na.rm = TRUE)
  if (is.na(cat_part) || cat_part <= 0) return(1)
  num_part / cat_part
}

kmm_distance_matrix_cpp <- function(x_num, x_cat, proto_num, proto_cat, lambda, missing) {
  k <- if (nrow(proto_num) > 0L) nrow(proto_num) else nrow(proto_cat)
  n <- if (nrow(x_num) > 0L) nrow(x_num) else nrow(x_cat)
  d <- matrix(Inf, nrow = n, ncol = k)
  for (i in seq_len(n)) {
    d[i, ] <- kmm_distance(i, x_num, x_cat, list(num = proto_num, cat = proto_cat), lambda, missing)
  }
  d
}

kmm_distance <- function(i, x_num, x_cat, proto, lambda, missing = "pairwise") {
  k <- if (!is.null(proto$num)) nrow(proto$num) else nrow(proto$cat)
  d <- numeric(k)
  for (g in seq_len(k)) {
    d_num <- 0
    d_cat <- 0
    observed_weight <- 0
    total_weight <- 0
    if (ncol(x_num) > 0L) {
      xi <- unlist(x_num[i, , drop = FALSE], use.names = FALSE)
      mu <- proto$num[g, ]
      obs <- !is.na(xi) & !is.na(mu)
      if (missing == "fail" && any(!obs)) {
        stop("Missing values detected; use missing = 'pairwise' or handle missing values before fitting.", call. = FALSE)
      }
      d_num <- sum((xi[obs] - mu[obs])^2)
      observed_weight <- observed_weight + sum(obs)
      total_weight <- total_weight + length(xi)
    }
    if (ncol(x_cat) > 0L) {
      zi <- as.character(unlist(x_cat[i, , drop = FALSE], use.names = FALSE))
      mg <- as.character(proto$cat[g, ])
      obs <- !is.na(zi) & !is.na(mg)
      if (missing == "fail" && any(!obs)) {
        stop("Missing values detected; use missing = 'pairwise' or handle missing values before fitting.", call. = FALSE)
      }
      d_cat <- sum(zi[obs] != mg[obs])
      observed_weight <- observed_weight + lambda * sum(obs)
      total_weight <- total_weight + lambda * length(zi)
    }
    raw_d <- d_num + lambda * d_cat
    d[g] <- if (observed_weight == 0) Inf else raw_d * total_weight / observed_weight
  }
  d
}
