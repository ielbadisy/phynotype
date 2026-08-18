normalize_embedding_method <- function(method) {
  if (is.null(method)) {
    method <- "auto"
  }
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`embedding` must be a single non-missing string.", call. = FALSE)
  }
  method <- tolower(method)
  if (!method %in% c("auto", "pca", "famd", "mca", "mds")) {
    stop("Unsupported embedding method `", method, "`.", call. = FALSE)
  }
  method
}

prepare_embedding_input <- function(data) {
  if (inherits(data, "dist")) {
    return(list(data = data, kind = "dist"))
  }
  if (is.matrix(data)) {
    if (!is.numeric(data)) {
      stop("`data` must be numeric when supplied as a matrix.", call. = FALSE)
    }
    return(list(data = data, kind = "numeric"))
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a matrix, data frame, or distance object.", call. = FALSE)
  }
  if (nrow(data) < 1L || ncol(data) < 1L) {
    stop("`data` must contain at least one row and one column.", call. = FALSE)
  }

  cleaned <- as.data.frame(data, stringsAsFactors = FALSE)
  has_numeric <- FALSE
  has_categorical <- FALSE

  for (nm in names(cleaned)) {
    col <- cleaned[[nm]]
    if (is.character(col) || is.logical(col) || is.factor(col) || is.ordered(col)) {
      cleaned[[nm]] <- add_missing_level(as.factor(col))
      has_categorical <- TRUE
    } else if (is.integer(col) && length(unique(col[!is.na(col)])) <= 10L) {
      cleaned[[nm]] <- add_missing_level(as.factor(col))
      has_categorical <- TRUE
    } else if (is.numeric(col)) {
      col <- as.numeric(col)
      if (anyNA(col)) {
        med <- stats::median(col, na.rm = TRUE)
        if (!is.finite(med)) {
          med <- 0
        }
        col[is.na(col)] <- med
      }
      cleaned[[nm]] <- col
      has_numeric <- TRUE
    } else {
      stop("`data` contains an unsupported column type in `", nm, "`.", call. = FALSE)
    }
  }

  kind <- if (has_numeric && has_categorical) {
    "mixed"
  } else if (has_categorical) {
    "categorical"
  } else {
    "numeric"
  }

  list(data = cleaned, kind = kind)
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
  coords <- coords[, 1:2, drop = FALSE]
  colnames(coords) <- c("x", "y")
  coords
}

embedding_labels <- function(method) {
  switch(
    method,
    pca = list(x = "PC1", y = "PC2"),
    famd = list(x = "Dim 1", y = "Dim 2"),
    mca = list(x = "Dim 1", y = "Dim 2"),
    mds = list(x = "MDS1", y = "MDS2")
  )
}

compute_pca_embedding <- function(data, clusters) {
  if (inherits(data, "dist")) {
    stop("PCA embedding requires row-by-feature data, not a distance object.", call. = FALSE)
  }
  input <- prepare_embedding_input(data)
  x <- if (input$kind == "numeric") {
    input$data
  } else {
    prepare_mixed_data(input$data, scale = TRUE)
  }
  pc <- stats::prcomp(x, center = TRUE, scale. = TRUE)
  coords <- pad_embedding_coords(pc$x)
  rotation <- pad_embedding_coords(pc$rotation)
  sdev <- if (length(pc$sdev) >= 2L) pc$sdev[1:2] else c(pc$sdev[1], 0)
  list(
    coords = data.frame(
      x = coords[, 1],
      y = coords[, 2],
      cluster = factor(clusters)
    ),
    loadings = data.frame(
      variable = rownames(pc$rotation),
      x = rotation[, 1] * sdev[1],
      y = rotation[, 2] * sdev[2],
      stringsAsFactors = FALSE
    )
  )
}

compute_famd_embedding <- function(data, clusters) {
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    stop("Package `FactoMineR` is required for FAMD embeddings.", call. = FALSE)
  }
  input <- prepare_embedding_input(data)
  if (input$kind != "mixed") {
    stop("FAMD embeddings require mixed numeric and categorical data.", call. = FALSE)
  }
  famd <- FactoMineR::FAMD(input$data, graph = FALSE)
  coords <- pad_embedding_coords(famd$ind$coord)
  var_coords <- pad_embedding_coords(famd$var$coord)
  list(
    coords = data.frame(
      x = coords[, 1],
      y = coords[, 2],
      cluster = factor(clusters)
    ),
    loadings = data.frame(
      variable = rownames(famd$var$coord),
      x = var_coords[, 1],
      y = var_coords[, 2],
      stringsAsFactors = FALSE
    )
  )
}

compute_mca_embedding <- function(data, clusters) {
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    stop("Package `FactoMineR` is required for MCA embeddings.", call. = FALSE)
  }
  input <- prepare_embedding_input(data)
  if (input$kind != "categorical") {
    stop("MCA embeddings require categorical data only.", call. = FALSE)
  }
  mca <- FactoMineR::MCA(input$data, graph = FALSE)
  coords <- pad_embedding_coords(mca$ind$coord)
  var_coords <- pad_embedding_coords(mca$var$coord)
  list(
    coords = data.frame(
      x = coords[, 1],
      y = coords[, 2],
      cluster = factor(clusters)
    ),
    loadings = data.frame(
      variable = rownames(mca$var$coord),
      x = var_coords[, 1],
      y = var_coords[, 2],
      stringsAsFactors = FALSE
    )
  )
}

compute_mds_embedding <- function(data, clusters) {
  input <- prepare_embedding_input(data)
  if (input$kind == "dist") {
    d <- input$data
    n_obs <- attr(d, "Size")
  } else if (input$kind == "numeric") {
    d <- stats::dist(input$data)
    n_obs <- nrow(input$data)
  } else {
    encoded <- prepare_mixed_data(input$data, scale = TRUE)
    d <- stats::dist(encoded)
    n_obs <- nrow(encoded)
  }
  coords <- if (n_obs <= 1L) {
    matrix(0, nrow = n_obs, ncol = 2L)
  } else {
    pad_embedding_coords(stats::cmdscale(d, k = 2, eig = FALSE))
  }
  list(
    coords = data.frame(
      x = coords[, 1],
      y = coords[, 2],
      cluster = factor(clusters)
    ),
    loadings = NULL
  )
}

compute_embedding <- function(data, clusters, method = "auto", data_info = NULL) {
  method <- normalize_embedding_method(method)
  input <- prepare_embedding_input(data)
  resolved <- if (method == "auto") {
    switch(
      input$kind,
      numeric = "pca",
      mixed = "famd",
      categorical = "mca",
      dist = "mds"
    )
  } else {
    method
  }

  embedding <- switch(
    resolved,
    pca = compute_pca_embedding(data, clusters),
    famd = compute_famd_embedding(data, clusters),
    mca = compute_mca_embedding(data, clusters),
    mds = compute_mds_embedding(data, clusters)
  )

  list(
    data = embedding$coords,
    loadings = embedding$loadings,
    method = resolved,
    labels = embedding_labels(resolved)
  )
}
