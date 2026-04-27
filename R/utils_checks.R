normalize_method_name <- function(method) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`method` must be a single non-missing string.", call. = FALSE)
  }
  tolower(method)
}

validate_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    stop("`seed` must be a single numeric value.", call. = FALSE)
  }
  invisible(seed)
}

check_numeric_input <- function(x) {
  if (inherits(x, "dist")) {
    return(list(data = x, kind = "dist"))
  }
  if (is.data.frame(x)) {
    numeric_cols <- vapply(x, is.numeric, logical(1))
    if (!all(numeric_cols)) {
      stop("All columns in `x` must be numeric for v1.", call. = FALSE)
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) {
    stop("`x` must be a numeric matrix or numeric data frame.", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  if (nrow(x) < 1L) {
    stop("`x` must contain at least one row.", call. = FALSE)
  }
  if (ncol(x) < 1L) {
    stop("`x` must contain at least one column.", call. = FALSE)
  }
  if (any(vapply(seq_len(ncol(x)), function(i) all(is.na(x[, i])), logical(1)))) {
    stop("`x` cannot contain all-missing columns.", call. = FALSE)
  }
  list(data = x, kind = "matrix")
}

check_required_k <- function(k) {
  if (is.null(k)) {
    stop("`k` is required for this method.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 2) {
    stop("`k` must be a single number greater than or equal to 2.", call. = FALSE)
  }
  as.integer(k)
}

check_k_grid <- function(k) {
  if (is.null(k) || length(k) < 1L || any(is.na(k)) || any(k < 2)) {
    stop("`k` must contain one or more integers greater than or equal to 2.", call. = FALSE)
  }
  sort(unique(as.integer(k)))
}

prepare_cluster_input <- function(x, method, scale, center, k) {
  checked <- check_numeric_input(x)
  if (checked$kind == "dist" && !method %in% c("hclust")) {
    stop("Distance inputs are only supported for hierarchical clustering in v1.", call. = FALSE)
  }

  raw <- checked$data
  processed <- raw
  preprocess <- list(center = FALSE, scale = FALSE)

  if (checked$kind != "dist") {
    if (scale && !center) {
      center <- TRUE
    }
    if (center || scale) {
      processed <- scale(raw, center = center, scale = scale)
      preprocess <- list(
        center = center,
        scale = scale,
        scaled_center = attr(processed, "scaled:center"),
        scaled_scale = attr(processed, "scaled:scale")
      )
    }
  }

  if (method %in% c("kmeans", "pam", "hclust")) {
    if (method != "hclust" || checked$kind != "dist") {
      check_required_k(k)
    }
  }

  list(
    data = processed,
    data_info = list(
      n_obs = if (checked$kind == "dist") attr(raw, "Size") else nrow(raw),
      n_features = if (checked$kind == "dist") NA_integer_ else ncol(raw),
      input_type = checked$kind,
      preprocessing = preprocess,
      original_data = raw,
      feature_names = if (checked$kind == "dist") NULL else colnames(raw)
    )
  )
}

validate_methods_vector <- function(methods) {
  if (!is.character(methods) || length(methods) < 1L) {
    stop("`methods` must be a character vector with at least one method.", call. = FALSE)
  }
  unique(tolower(methods))
}

with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(expr)
}
