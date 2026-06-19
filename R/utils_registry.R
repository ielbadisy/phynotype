get_cluster_registry <- function() {
  list(
    kmeans = list(
      fit = fit_kmeans,
      predict = predict_kmeans,
      validate = validate_kmeans_params,
      supports_predict = TRUE
    ),
    pam = list(
      fit = fit_pam,
      predict = predict_pam,
      validate = validate_pam_params,
      supports_predict = TRUE
    ),
    hclust = list(
      fit = fit_hclust,
      predict = predict_hclust,
      validate = validate_hclust_params,
      supports_predict = TRUE
    ),
    agnes = list(
      fit = fit_agnes,
      predict = predict_agnes,
      validate = validate_agnes_params,
      supports_predict = TRUE
    ),
    dbscan = list(
      fit = fit_dbscan,
      predict = predict_dbscan,
      validate = validate_dbscan_params,
      supports_predict = TRUE
    ),
    gmm = list(
      fit = fit_gmm,
      predict = predict_gmm,
      validate = validate_gmm_params,
      supports_predict = TRUE
    ),
    kproto = list(
      fit = fit_kproto,
      predict = predict_kproto,
      validate = validate_kproto_params,
      supports_predict = TRUE
    ),
    kmm = list(
      fit = fit_kmm,
      predict = predict_kmm,
      validate = validate_kmm_params,
      supports_predict = TRUE
    )
  )
}
