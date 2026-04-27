new_cluster_fit <- function(method,
                            call,
                            params,
                            clusters,
                            n_clusters,
                            membership = NULL,
                            centers = NULL,
                            prototypes = NULL,
                            distance_info = NULL,
                            fitted_object,
                            data_info,
                            extras = list()) {
  structure(
    list(
      method = method,
      call = call,
      params = params,
      clusters = as.integer(clusters),
      n_clusters = as.integer(n_clusters),
      membership = membership,
      centers = centers,
      prototypes = prototypes,
      distance_info = distance_info,
      fitted_object = fitted_object,
      data_info = data_info,
      extras = extras
    ),
    class = "cluster_fit"
  )
}

new_cluster_validation <- function(metrics_table,
                                   per_cluster_table = NULL,
                                   settings = list(),
                                   object_type = "cluster_fit",
                                   extras = list()) {
  structure(
    list(
      metrics_table = metrics_table,
      per_cluster_table = per_cluster_table,
      settings = settings,
      object_type = object_type,
      extras = extras
    ),
    class = "cluster_validation"
  )
}

new_metacluster_fit <- function(call,
                                params,
                                candidate_fits,
                                candidate_labels,
                                candidate_table,
                                coassoc_matrix,
                                consensus_dissimilarity,
                                consensus_fit,
                                final_clusters,
                                final_k,
                                selection_summary,
                                stability_summary = NULL,
                                data_info,
                                extras = list()) {
  structure(
    list(
      method = "metacluster",
      call = call,
      params = params,
      candidate_fits = candidate_fits,
      candidate_labels = candidate_labels,
      candidate_table = candidate_table,
      coassoc_matrix = coassoc_matrix,
      consensus_dissimilarity = consensus_dissimilarity,
      consensus_fit = consensus_fit,
      final_clusters = as.integer(final_clusters),
      final_k = as.integer(final_k),
      selection_summary = selection_summary,
      stability_summary = stability_summary,
      data_info = data_info,
      extras = extras
    ),
    class = "metacluster_fit"
  )
}

new_cluster_explore <- function(size_table,
                                feature_summary,
                                separation_table,
                                prototype_table = NULL,
                                embedding,
                                plot_data) {
  structure(
    list(
      size_table = size_table,
      feature_summary = feature_summary,
      separation_table = separation_table,
      prototype_table = prototype_table,
      embedding = embedding,
      plot_data = plot_data
    ),
    class = "cluster_explore"
  )
}

new_cluster_prediction <- function(clusters,
                                   membership = NULL,
                                   distances = NULL,
                                   method,
                                   prediction_type = "native") {
  structure(
    list(
      clusters = as.integer(clusters),
      membership = membership,
      distances = distances,
      method = method,
      prediction_type = prediction_type
    ),
    class = "cluster_prediction"
  )
}

new_feature_importance <- function(results, summary, settings) {
  structure(
    list(results = results, summary = summary, settings = settings),
    class = "feature_importance"
  )
}

new_ceteris_paribus <- function(profiles, settings) {
  structure(
    list(profiles = profiles, settings = settings),
    class = "ceteris_paribus"
  )
}

new_lime_explanation <- function(explanations, neighborhoods, settings) {
  structure(
    list(explanations = explanations, neighborhoods = neighborhoods, settings = settings),
    class = "lime_explanation"
  )
}
