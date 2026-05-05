validate_kproto_params <- function(data, params) {
  if (!requireNamespace("clustMixType", quietly = TRUE)) {
    stop("Package `clustMixType` is required for method `kproto`.", call. = FALSE)
  }
  k <- check_required_k(params$k)
  if (!is.data.frame(data)) {
    stop("`kproto` requires row-by-feature data.", call. = FALSE)
  }
  if (k > nrow(data)) {
    stop("`k` cannot exceed the number of observations.", call. = FALSE)
  }
}

fit_kproto <- function(data, params) {
  x <- prepare_kproto_input(data, scale = isTRUE(params$scale), center = isTRUE(params$center))
  nstart <- if (is.null(params$nstart)) 10L else as.integer(params$nstart)
  iter.max <- if (is.null(params$iter.max)) 100L else as.integer(params$iter.max)
  lambda <- params$lambda
  fitted <- clustMixType::kproto(
    x = x,
    k = as.integer(params$k),
    lambda = lambda,
    iter.max = iter.max,
    nstart = nstart,
    verbose = isTRUE(params$verbose)
  )
  prototypes <- as.data.frame(fitted$centers, stringsAsFactors = FALSE)
  rownames(prototypes) <- paste0("cluster_", seq_len(nrow(prototypes)))
  list(
    clusters = fitted$cluster,
    n_clusters = length(unique(fitted$cluster)),
    membership = NULL,
    centers = NULL,
    prototypes = prototypes,
    distance_info = list(metric = "k-prototypes", lambda = fitted$lambda),
    fitted_object = fitted,
    extras = list(
      withinss = fitted$withinss,
      tot.withinss = fitted$tot.withinss,
      input_info = attr(x, "phynotype_kproto_input")
    )
  )
}

predict_kproto <- function(object, new_data, ...) {
  info <- object$extras$input_info
  x <- prepare_kproto_newdata(new_data, info)
  prototypes <- object$prototypes
  lambda <- object$distance_info$lambda
  dmat <- mixed_prototype_distance(x, prototypes, gamma = lambda)
  cls <- max.col(-dmat)
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = sqrt(dmat),
    method = object$method,
    prediction_type = "nearest_prototype"
  )
}

prepare_kproto_input <- function(x, scale = TRUE, center = TRUE) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  numeric_cols <- vapply(x, is.numeric, logical(1))
  num_center <- NULL
  num_scale <- NULL
  if (any(numeric_cols)) {
    num <- as.matrix(x[, numeric_cols, drop = FALSE])
    num_medians <- apply(num, 2, stats::median, na.rm = TRUE)
    num_medians[!is.finite(num_medians)] <- 0
    for (j in seq_len(ncol(num))) {
      num[is.na(num[, j]), j] <- num_medians[[j]]
    }
    if (center || scale) {
      num_scaled <- scale(num, center = center, scale = scale)
      num_center <- attr(num_scaled, "scaled:center")
      num_scale <- attr(num_scaled, "scaled:scale")
      bad_scale <- !is.finite(colSums(num_scaled))
      if (any(bad_scale)) {
        num_scaled[, bad_scale] <- 0
      }
      x[, numeric_cols] <- as.data.frame(num_scaled)
    }
  }
  for (nm in names(x)[!numeric_cols]) {
    x[[nm]] <- as.factor(x[[nm]])
    if (anyNA(x[[nm]])) {
      x[[nm]] <- add_missing_level(x[[nm]])
    }
  }
  attr(x, "phynotype_kproto_input") <- list(
    numeric_cols = numeric_cols,
    center = center,
    scale = scale,
    num_center = num_center,
    num_scale = num_scale,
    num_medians = if (any(numeric_cols)) num_medians else NULL,
    factor_levels = lapply(x[, !numeric_cols, drop = FALSE], levels)
  )
  x
}

prepare_kproto_newdata <- function(new_data, info) {
  x <- as.data.frame(new_data, stringsAsFactors = FALSE)
  numeric_cols <- info$numeric_cols
  if (any(numeric_cols)) {
    num <- as.matrix(x[, numeric_cols, drop = FALSE])
    for (j in seq_len(ncol(num))) {
      num[is.na(num[, j]), j] <- info$num_medians[[j]]
    }
    if (isTRUE(info$center)) {
      num <- sweep(num, 2, info$num_center, FUN = "-")
    }
    if (isTRUE(info$scale)) {
      sc <- info$num_scale
      sc[sc == 0] <- 1
      num <- sweep(num, 2, sc, FUN = "/")
    }
    x[, numeric_cols] <- as.data.frame(num)
  }
  cat_names <- names(x)[!numeric_cols]
  for (nm in cat_names) {
    x[[nm]] <- factor(x[[nm]], levels = info$factor_levels[[nm]])
    if (anyNA(x[[nm]]) && "missing" %in% info$factor_levels[[nm]]) {
      x[[nm]][is.na(x[[nm]])] <- "missing"
    }
  }
  x
}

mixed_prototype_distance <- function(x, prototypes, gamma = 1) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  prototypes <- as.data.frame(prototypes, stringsAsFactors = FALSE)
  numeric_cols <- vapply(x, is.numeric, logical(1))
  n <- nrow(x)
  k <- nrow(prototypes)
  dmat <- matrix(0, nrow = n, ncol = k)
  for (c in seq_len(k)) {
    dn <- if (any(numeric_cols)) {
      proto_num <- as.numeric(prototypes[c, numeric_cols, drop = TRUE])
      rowSums((as.matrix(x[, numeric_cols, drop = FALSE]) -
        matrix(proto_num, nrow = n, ncol = length(proto_num), byrow = TRUE))^2)
    } else {
      rep(0, n)
    }
    dc <- if (any(!numeric_cols)) {
      cats <- names(x)[!numeric_cols]
      rowSums(vapply(cats, function(nm) as.character(x[[nm]]) != as.character(prototypes[[nm]][c]), logical(n)))
    } else {
      rep(0, n)
    }
    dmat[, c] <- dn + gamma * dc
  }
  colnames(dmat) <- rownames(prototypes)
  dmat
}
