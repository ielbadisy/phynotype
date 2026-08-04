normalize_embedding_method <- function(method) {
  if (is.null(method)) {
    method <- "auto"
  }
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`embedding` must be a single non-missing string.", call. = FALSE)
  }
  method <- tolower(method)
  if (!method %in% c("auto", "pca", "mds")) {
    stop("Unsupported embedding method `", method, "`.", call. = FALSE)
  }
  method
}

normalize_embedding_input <- function(data) {
  if (inherits(data, "dist")) {
    return(list(data = data, kind = "dist"))
  }
  if (is.data.frame(data)) {
    numeric_cols <- vapply(data, is.numeric, logical(1))
    if (!all(numeric_cols)) {
      data <- prepare_mixed_data(data, scale = TRUE)
      return(list(data = data, kind = "mixed"))
    }
    data <- as.matrix(data)
  }
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("`data` must be a numeric matrix/data frame or a distance object.", call. = FALSE)
  }
  list(data = data, kind = "numeric")
}

pad_embedding_coords <- function(coords) {
  coords <- as.matrix(coords)
  if (nrow(coords) == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = 2L))
  }
  if (ncol(coords) == 1L) {
    coords <- cbind(coords, 0)
  } else if (ncol(coords) < 1L) {
    coords <- matrix(0, nrow = nrow(coords), ncol = 2L)
  }
  colnames(coords)[1:2] <- c("x", "y")
  coords
}

compute_pca_embedding <- function(data, clusters) {
  if (inherits(data, "dist")) {
    stop("PCA embedding requires row-by-feature data, not a distance object.", call. = FALSE)
  }
  input <- normalize_embedding_input(data)
  pc <- stats::prcomp(input$data, center = TRUE, scale. = TRUE)
  coords <- pad_embedding_coords(pc$x)
  data.frame(
    x = coords[, 1],
    y = coords[, 2],
    cluster = factor(clusters)
  )
}

compute_mds_embedding <- function(data, clusters) {
  input <- normalize_embedding_input(data)
  if (inherits(input$data, "dist")) {
    d <- input$data
    n_obs <- attr(d, "Size")
  } else {
    d <- stats::dist(input$data)
    n_obs <- nrow(input$data)
  }
  coords <- if (n_obs <= 1L) {
    matrix(0, nrow = n_obs, ncol = 2L)
  } else {
    pad_embedding_coords(stats::cmdscale(d, k = 2, eig = FALSE))
  }
  data.frame(
    x = coords[, 1],
    y = coords[, 2],
    cluster = factor(clusters)
  )
}

compute_embedding <- function(data, clusters, method = "auto", data_info = NULL) {
  method <- normalize_embedding_method(method)
  input_kind <- if (!is.null(data_info) && !is.null(data_info$input_type)) {
    data_info$input_type
  } else if (inherits(data, "dist")) {
    "dist"
  } else if (is.data.frame(data) && !all(vapply(data, is.numeric, logical(1)))) {
    "mixed"
  } else {
    "numeric"
  }

  resolved <- if (method == "auto") {
    if (identical(input_kind, "dist")) "mds" else "pca"
  } else {
    method
  }

  embedding <- switch(
    resolved,
    pca = compute_pca_embedding(data, clusters),
    mds = compute_mds_embedding(data, clusters)
  )

  list(
    data = embedding,
    method = resolved,
    labels = if (resolved == "mds") {
      list(x = "MDS1", y = "MDS2")
    } else {
      list(x = "PC1", y = "PC2")
    }
  )
}
